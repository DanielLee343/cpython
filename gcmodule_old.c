/*

  Reference Cycle Garbage Collection
  ==================================

  Neil Schemenauer <nas@arctrix.com>

  Based on a post on the python-dev list.  Ideas from Guido van Rossum,
  Eric Tiedemann, and various others.

  http://www.arctrix.com/nas/python/gc/

  The following mailing list threads provide a historical perspective on
  the design of this module.  Note that a fair amount of refinement has
  occurred since those discussions.

  http://mail.python.org/pipermail/python-dev/2000-March/002385.html
  http://mail.python.org/pipermail/python-dev/2000-March/002434.html
  http://mail.python.org/pipermail/python-dev/2000-March/002497.html

  For a highlevel view of the collection process, read the collect
  function.

*/

#include "Python.h"
#include "pycore_context.h"
#include "pycore_initconfig.h"
#include "pycore_interp.h" // PyInterpreterState.gc
#include "pycore_object.h"
#include "pycore_pyerrors.h"
#include "pycore_pystate.h" // _PyThreadState_GET()
#include "pydtrace.h"

#include "utlist.h"
#include <numaif.h>
#include <numa.h>
#include <setjmp.h>

#undef CUCKOO_TABLE_NAME
#undef CUCKOO_KEY_TYPE
#undef CUCKOO_MAPPED_TYPE
#include "allHeats.h"
#undef CUCKOO_TABLE_NAME
#undef CUCKOO_KEY_TYPE
#undef CUCKOO_MAPPED_TYPE
#include "curHeats.h"
#undef CUCKOO_TABLE_NAME
#undef CUCKOO_KEY_TYPE
#undef CUCKOO_MAPPED_TYPE
#include "op_gc.h"

#include "pages2move.h"
#include "myset.h"
#include "kh_set_wrap.h"
pthread_mutex_t mutex;
pthread_mutexattr_t attr;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond_slow = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond_fast = PTHREAD_COND_INITIALIZER;
static double total_cur_cascading_time = 0.0;
static int total_num_slow = 0;
unsigned long global_try2_sched = 0;
int need2_check_set = 1;
extern unsigned long num_container_collected;
extern PyThreadState *py_main_tstate;

// khash_t(ptrset) * global_op_set;

kvec_t(PyObject *) local_ptr_vec;

KHASH_MAP_INIT_INT64(ptr2int, int)
BookkeepArgs *global_bookkeep_args;
uintptr_t glb_lowest_op = UINTPTR_MAX;
int rescan_thresh_glb = 0;

static clock_t last_live_trace_time;
static clock_t last_migrate_time;
extern int enable_bk;
unsigned long get_live_time_thresh;
unsigned long migration_time_thresh;
unsigned long num_gc_cycles = 0;
#define PAGE_SIZE 4096
#define PAGE_MASK (~(PAGE_SIZE - 1))
#define LEN_THRESHOLD 0
#define DEPTH_THRESHOLD 100
#define LOWER_BOUND 100000000000000
#define RESET_MD_DEC_THRESH -100000
#define HOT_THRESH 100
#define TRIGGER_SCAN_WM 35
#define DRAM_MASK 0
#define CXL_MASK 1
#define DROP_OUT_OFF 14
#define LOCATION_OFF 15
#define HOTNESS_MASK 0x3FFF
unsigned long cur_fast_num_hot = 0;
static bool very_first_bk = true;
unsigned long not_in_global_set = 0;

typedef struct heat_node
{
    // uintptr_t op;
    PyObject *op;
    Temperature *temp;
    struct heat_node *next, *prev;
} heat_node;
// typedef struct
// {
//     PyObject *op;      // 8
//     long prev_refcnt;  // 8
//     short diffs[20];   // 40
//     size_t cur_sizeof; // 8
// } OBJ_TEMP;
#include "obj_temp.h"
OBJ_TEMP *all_temps = NULL;
typedef struct GEN_ID_BOUND
{
    int gen_id;
    uintptr_t low;
    uintptr_t high;
    uintptr_t head;
} GEN_ID_BOUND;

GEN_ID_BOUND gen_id_bound[NUM_GENERATIONS];

typedef struct cur_survived_node
{
    // uintptr_t op;
    PyObject *op;
    struct cur_survived_node *next, *prev;
} cur_survived_node;

int allow_fast = 0;
int allow_slow = 0;
PyGC_Head *global_old;
static int gen_idx = -1;

int cmp_func(heat_node *a, heat_node *b)
{
    return a->op != b->op;
}

typedef struct
{
    uintptr_t start;
    uintptr_t end;
} PyObj_range;

typedef struct op_hotness
{
    PyObject *op;
    unsigned long hotness;
} op_hotness;

int compareIntervals(const void *a, const void *b)
{
    PyObj_range *A = (PyObj_range *)a;
    PyObj_range *B = (PyObj_range *)b;
    return A->start - B->start; // Corrected to compare start times
}
int compareOpHotnessDesc(const void *a, const void *b)
{
    const OBJ_TEMP *A = (const OBJ_TEMP *)a;
    const OBJ_TEMP *B = (const OBJ_TEMP *)b;
    return (B->diffs[rescan_thresh_glb] - A->diffs[rescan_thresh_glb]);
    // const op_hotness *A = (const op_hotness *)a;
    // const op_hotness *B = (const op_hotness *)b;
    // if (B->hotness > A->hotness)
    //     return 1;
    // if (B->hotness < A->hotness)
    //     return -1;
    // return 0;
}

static inline uintptr_t max(uintptr_t a, uintptr_t b)
{
    return a > b ? a : b;
}
volatile short terminate_flag = 0;
static heat_node *global_heat_utlist;

typedef struct _gc_runtime_state GCState;
void update_recursive_utlist(PyObject *each_op, heat_node **heat_node_head);
void update_recursive(PyObject *each_op, cur_heats_table *table);
void gc_get_objects_impl_op_gc(Py_ssize_t generation, op_gc_table *table);
void update_recursive_visitor(PyObject *each_op, unsigned int *depth);
void inner_traversing(PyObject *each_op, unsigned int *depth);
bool found_in_local_kset(PyObject *op);
static void add_to_survived_from_young(PyGC_Head *survived);

/*[clinic input]
module gc
[clinic start generated code]*/
/*[clinic end generated code: output=da39a3ee5e6b4b0d input=b5c9690ecc842d79]*/

#ifdef Py_DEBUG
#define GC_DEBUG
#endif

#define GC_NEXT _PyGCHead_NEXT
#define GC_PREV _PyGCHead_PREV

// update_refs() set this bit for all objects in current generation.
// subtract_refs() and move_unreachable() uses this to distinguish
// visited object is in GCing or not.
//
// move_unreachable() removes this flag from reachable objects.
// Only unreachable objects have this flag.
//
// No objects in interpreter have this flag after GC ends.
#define PREV_MASK_COLLECTING _PyGC_PREV_MASK_COLLECTING

// Lowest bit of _gc_next is used for UNREACHABLE flag.
//
// This flag represents the object is in unreachable list in move_unreachable()
//
// Although this flag is used only in move_unreachable(), move_unreachable()
// doesn't clear this flag to skip unnecessary iteration.
// move_legacy_finalizers() removes this flag instead.
// Between them, unreachable list is not normal list and we can not use
// most gc_list_* functions for it.
#define NEXT_MASK_UNREACHABLE (1)

/* Get an object's GC head */
#define AS_GC(o) ((PyGC_Head *)(((char *)(o)) - sizeof(PyGC_Head)))

/* Get the object given the GC head */
#define FROM_GC(g) ((PyObject *)(((char *)(g)) + sizeof(PyGC_Head)))

static inline int gc_is_collecting(PyGC_Head *g)
{
    return (g->_gc_prev & PREV_MASK_COLLECTING) != 0;
}

static inline void
gc_clear_collecting(PyGC_Head *g)
{
    g->_gc_prev &= ~PREV_MASK_COLLECTING;
}

static inline Py_ssize_t
gc_get_refs(PyGC_Head *g)
{
    return (Py_ssize_t)(g->_gc_prev >> _PyGC_PREV_SHIFT);
}

static inline void
gc_set_refs(PyGC_Head *g, Py_ssize_t refs)
{
    g->_gc_prev = (g->_gc_prev & ~_PyGC_PREV_MASK) | ((uintptr_t)(refs) << _PyGC_PREV_SHIFT);
}

static inline void
gc_reset_refs(PyGC_Head *g, Py_ssize_t refs)
{
    g->_gc_prev = (g->_gc_prev & _PyGC_PREV_MASK_FINALIZED) | PREV_MASK_COLLECTING | ((uintptr_t)(refs) << _PyGC_PREV_SHIFT);
}

static inline void
gc_decref(PyGC_Head *g)
{
    _PyObject_ASSERT_WITH_MSG(FROM_GC(g),
                              gc_get_refs(g) > 0,
                              "refcount is too small");
    g->_gc_prev -= 1 << _PyGC_PREV_SHIFT;
}

/* set for debugging information */
#define DEBUG_STATS (1 << 0)         /* print collection statistics */
#define DEBUG_COLLECTABLE (1 << 1)   /* print collectable objects */
#define DEBUG_UNCOLLECTABLE (1 << 2) /* print uncollectable objects */
#define DEBUG_SAVEALL (1 << 5)       /* save all garbage in gc.garbage */
#define DEBUG_LEAK DEBUG_COLLECTABLE |       \
                       DEBUG_UNCOLLECTABLE | \
                       DEBUG_SAVEALL

#define GEN_HEAD(gcstate, n) (&(gcstate)->generations[n].head)

static GCState *
get_gc_state(void)
{
    PyInterpreterState *interp = _PyInterpreterState_GET();
    return &interp->gc;
}

void _PyGC_InitState(GCState *gcstate)
{
#define INIT_HEAD(GEN)                              \
    do                                              \
    {                                               \
        GEN.head._gc_next = (uintptr_t) & GEN.head; \
        GEN.head._gc_prev = (uintptr_t) & GEN.head; \
    } while (0)

    for (int i = 0; i < NUM_GENERATIONS; i++)
    {
        assert(gcstate->generations[i].count == 0);
        INIT_HEAD(gcstate->generations[i]);
    };
    gcstate->generation0 = GEN_HEAD(gcstate, 0);
    INIT_HEAD(gcstate->permanent_generation);

#undef INIT_HEAD
}

PyStatus
_PyGC_Init(PyInterpreterState *interp)
{
    last_live_trace_time = clock();
    enable_bk = 0;
    get_live_time_thresh = INT_MAX;  // don't trigger slow scan by default
    migration_time_thresh = INT_MAX; // don't trigger migration by default
    py_main_tstate = _PyThreadState_GET();
    for (int i = 0; i < NUM_GENERATIONS; i++)
    {
        gen_id_bound[i].gen_id = i;
        gen_id_bound[i].low = UINTPTR_MAX;
        gen_id_bound[i].high = 0;
        gen_id_bound[i].head = 0;
    }
    GCState *gcstate = &interp->gc;

    gcstate->garbage = PyList_New(0);
    if (gcstate->garbage == NULL)
    {
        return _PyStatus_NO_MEMORY();
    }

    gcstate->callbacks = PyList_New(0);
    if (gcstate->callbacks == NULL)
    {
        return _PyStatus_NO_MEMORY();
    }
    return _PyStatus_OK();
}

/*
_gc_prev values
---------------

Between collections, _gc_prev is used for doubly linked list.

Lowest two bits of _gc_prev are used for flags.
PREV_MASK_COLLECTING is used only while collecting and cleared before GC ends
or _PyObject_GC_UNTRACK() is called.

During a collection, _gc_prev is temporary used for gc_refs, and the gc list
is singly linked until _gc_prev is restored.

gc_refs
    At the start of a collection, update_refs() copies the true refcount
    to gc_refs, for each object in the generation being collected.
    subtract_refs() then adjusts gc_refs so that it equals the number of
    times an object is referenced directly from outside the generation
    being collected.

PREV_MASK_COLLECTING
    Objects in generation being collected are marked PREV_MASK_COLLECTING in
    update_refs().


_gc_next values
---------------

_gc_next takes these values:

0
    The object is not tracked

!= 0
    Pointer to the next object in the GC list.
    Additionally, lowest bit is used temporary for
    NEXT_MASK_UNREACHABLE flag described below.

NEXT_MASK_UNREACHABLE
    move_unreachable() then moves objects not reachable (whether directly or
    indirectly) from outside the generation into an "unreachable" set and
    set this flag.

    Objects that are found to be reachable have gc_refs set to 1.
    When this flag is set for the reachable object, the object must be in
    "unreachable" set.
    The flag is unset and the object is moved back to "reachable" set.

    move_legacy_finalizers() will remove this flag from "unreachable" set.
*/

/*** list functions ***/

static inline void
gc_list_init(PyGC_Head *list)
{
    // List header must not have flags.
    // We can assign pointer by simple cast.
    list->_gc_prev = (uintptr_t)list;
    list->_gc_next = (uintptr_t)list;
}

static inline int
gc_list_is_empty(PyGC_Head *list)
{
    return (list->_gc_next == (uintptr_t)list);
}

/* Append `node` to `list`. */
static inline void
gc_list_append(PyGC_Head *node, PyGC_Head *list)
{
    PyGC_Head *last = (PyGC_Head *)list->_gc_prev;

    // last <-> node
    _PyGCHead_SET_PREV(node, last);
    _PyGCHead_SET_NEXT(last, node);

    // node <-> list
    _PyGCHead_SET_NEXT(node, list);
    list->_gc_prev = (uintptr_t)node;
}

/* Remove `node` from the gc list it's currently in. */
static inline void
gc_list_remove(PyGC_Head *node)
{
    PyGC_Head *prev = GC_PREV(node);
    PyGC_Head *next = GC_NEXT(node);

    _PyGCHead_SET_NEXT(prev, next);
    _PyGCHead_SET_PREV(next, prev);

    node->_gc_next = 0; /* object is not currently tracked */
}

/* Move `node` from the gc list it's currently in (which is not explicitly
 * named here) to the end of `list`.  This is semantically the same as
 * gc_list_remove(node) followed by gc_list_append(node, list).
 */
static void
gc_list_move(PyGC_Head *node, PyGC_Head *list)
{
    /* Unlink from current list. */
    PyGC_Head *from_prev = GC_PREV(node);
    PyGC_Head *from_next = GC_NEXT(node);
    _PyGCHead_SET_NEXT(from_prev, from_next);
    _PyGCHead_SET_PREV(from_next, from_prev);

    /* Relink at end of new list. */
    // list must not have flags.  So we can skip macros.
    PyGC_Head *to_prev = (PyGC_Head *)list->_gc_prev;
    _PyGCHead_SET_PREV(node, to_prev);
    _PyGCHead_SET_NEXT(to_prev, node);
    list->_gc_prev = (uintptr_t)node;
    _PyGCHead_SET_NEXT(node, list);
}

/* append list `from` onto list `to`; `from` becomes an empty list */
static void
gc_list_merge(PyGC_Head *from, PyGC_Head *to)
{
    assert(from != to);
    if (!gc_list_is_empty(from))
    {
        PyGC_Head *to_tail = GC_PREV(to);
        PyGC_Head *from_head = GC_NEXT(from);
        PyGC_Head *from_tail = GC_PREV(from);
        assert(from_head != from);
        assert(from_tail != from);

        _PyGCHead_SET_NEXT(to_tail, from_head);
        _PyGCHead_SET_PREV(from_head, to_tail);

        _PyGCHead_SET_NEXT(from_tail, to);
        _PyGCHead_SET_PREV(to, from_tail);
    }
    gc_list_init(from);
}

static Py_ssize_t
gc_list_size(PyGC_Head *list)
{
    PyGC_Head *gc;
    Py_ssize_t n = 0;
    for (gc = GC_NEXT(list); gc != list; gc = GC_NEXT(gc))
    {
        n++;
    }
    return n;
}

/* Walk the list and mark all objects as non-collecting */
static inline void
gc_list_clear_collecting(PyGC_Head *collectable)
{
    PyGC_Head *gc;
    for (gc = GC_NEXT(collectable); gc != collectable; gc = GC_NEXT(gc))
    {
        gc_clear_collecting(gc);
    }
}

/* Append objects in a GC list to a Python list.
 * Return 0 if all OK, < 0 if error (out of memory for list)
 */
static int
append_objects(PyObject *py_list, PyGC_Head *gc_list)
{
    PyGC_Head *gc;
    for (gc = GC_NEXT(gc_list); gc != gc_list; gc = GC_NEXT(gc))
    {
        PyObject *op = FROM_GC(gc);
        if (op != py_list)
        {
            if (PyList_Append(py_list, op))
            {
                return -1; /* exception */
            }
        }
    }
    return 0;
}

// Constants for validate_list's flags argument.
enum flagstates
{
    collecting_clear_unreachable_clear,
    collecting_clear_unreachable_set,
    collecting_set_unreachable_clear,
    collecting_set_unreachable_set
};

#ifdef GC_DEBUG
// validate_list checks list consistency.  And it works as document
// describing when flags are expected to be set / unset.
// `head` must be a doubly-linked gc list, although it's fine (expected!) if
// the prev and next pointers are "polluted" with flags.
// What's checked:
// - The `head` pointers are not polluted.
// - The objects' PREV_MASK_COLLECTING and NEXT_MASK_UNREACHABLE flags are all
//   `set or clear, as specified by the 'flags' argument.
// - The prev and next pointers are mutually consistent.
static void
validate_list(PyGC_Head *head, enum flagstates flags)
{
    assert((head->_gc_prev & PREV_MASK_COLLECTING) == 0);
    assert((head->_gc_next & NEXT_MASK_UNREACHABLE) == 0);
    uintptr_t prev_value = 0, next_value = 0;
    switch (flags)
    {
    case collecting_clear_unreachable_clear:
        break;
    case collecting_set_unreachable_clear:
        prev_value = PREV_MASK_COLLECTING;
        break;
    case collecting_clear_unreachable_set:
        next_value = NEXT_MASK_UNREACHABLE;
        break;
    case collecting_set_unreachable_set:
        prev_value = PREV_MASK_COLLECTING;
        next_value = NEXT_MASK_UNREACHABLE;
        break;
    default:
        assert(!"bad internal flags argument");
    }
    PyGC_Head *prev = head;
    PyGC_Head *gc = GC_NEXT(head);
    while (gc != head)
    {
        PyGC_Head *trueprev = GC_PREV(gc);
        PyGC_Head *truenext = (PyGC_Head *)(gc->_gc_next & ~NEXT_MASK_UNREACHABLE);
        assert(truenext != NULL);
        assert(trueprev == prev);
        assert((gc->_gc_prev & PREV_MASK_COLLECTING) == prev_value);
        assert((gc->_gc_next & NEXT_MASK_UNREACHABLE) == next_value);
        prev = gc;
        gc = truenext;
    }
    assert(prev == GC_PREV(head));
}
#else
#define validate_list(x, y) \
    do                      \
    {                       \
    } while (0)
#endif

/*** end of list stuff ***/

/* Set all gc_refs = ob_refcnt.  After this, gc_refs is > 0 and
 * PREV_MASK_COLLECTING bit is set for all objects in containers.
 */
static void
update_refs(PyGC_Head *containers)
{
    PyGC_Head *next;
    PyGC_Head *gc = GC_NEXT(containers);

    while (gc != containers)
    {
        next = GC_NEXT(gc);
        /* Move any object that might have become immortal to the
         * permanent generation as the reference count is not accurately
         * reflecting the actual number of live references to this object
         */
        if (_Py_IsImmortal(FROM_GC(gc)))
        {
            gc_list_move(gc, &get_gc_state()->permanent_generation.head);
            gc = next;
            continue;
        }
        gc_reset_refs(gc, Py_REFCNT(FROM_GC(gc)));
        /* Python's cyclic gc should never see an incoming refcount
         * of 0:  if something decref'ed to 0, it should have been
         * deallocated immediately at that time.
         * Possible cause (if the assert triggers):  a tp_dealloc
         * routine left a gc-aware object tracked during its teardown
         * phase, and did something-- or allowed something to happen --
         * that called back into Python.  gc can trigger then, and may
         * see the still-tracked dying object.  Before this assert
         * was added, such mistakes went on to allow gc to try to
         * delete the object again.  In a debug build, that caused
         * a mysterious segfault, when _Py_ForgetReference tried
         * to remove the object from the doubly-linked list of all
         * objects a second time.  In a release build, an actual
         * double deallocation occurred, which leads to corruption
         * of the allocator's internal bookkeeping pointers.  That's
         * so serious that maybe this should be a release-build
         * check instead of an assert?
         */
        _PyObject_ASSERT(FROM_GC(gc), gc_get_refs(gc) != 0);
        gc = next;
    }
}

/* A traversal callback for subtract_refs. */
static int
visit_decref(PyObject *op, void *parent)
{
    _PyObject_ASSERT(_PyObject_CAST(parent), !_PyObject_IsFreed(op));

    if (_PyObject_IS_GC(op))
    {
        PyGC_Head *gc = AS_GC(op);
        /* We're only interested in gc_refs for objects in the
         * generation being collected, which can be recognized
         * because only they have positive gc_refs.
         */
        if (gc_is_collecting(gc))
        {
            gc_decref(gc);
        }
    }
    return 0;
}

/* Subtract internal references from gc_refs.  After this, gc_refs is >= 0
 * for all objects in containers, and is GC_REACHABLE for all tracked gc
 * objects not in containers.  The ones with gc_refs > 0 are directly
 * reachable from outside containers, and so can't be collected.
 */
static void
subtract_refs(PyGC_Head *containers)
{
    traverseproc traverse;
    PyGC_Head *gc = GC_NEXT(containers);
    for (; gc != containers; gc = GC_NEXT(gc))
    {
        PyObject *op = FROM_GC(gc);
        traverse = Py_TYPE(op)->tp_traverse;
        (void)traverse(op,
                       (visitproc)visit_decref,
                       op);
    }
}

/* A traversal callback for move_unreachable. */
static int
visit_reachable(PyObject *op, PyGC_Head *reachable)
{
    if (!_PyObject_IS_GC(op))
    {
        return 0;
    }

    PyGC_Head *gc = AS_GC(op);
    const Py_ssize_t gc_refs = gc_get_refs(gc);
    int ret;

    // Ignore objects in other generation.
    // This also skips objects "to the left" of the current position in
    // move_unreachable's scan of the 'young' list - they've already been
    // traversed, and no longer have the PREV_MASK_COLLECTING flag.
    if (!gc_is_collecting(gc))
    {
        return 0;
    }
    // It would be a logic error elsewhere if the collecting flag were set on
    // an untracked object.
    assert(gc->_gc_next != 0);

    if (gc->_gc_next & NEXT_MASK_UNREACHABLE)
    {
        /* This had gc_refs = 0 when move_unreachable got
         * to it, but turns out it's reachable after all.
         * Move it back to move_unreachable's 'young' list,
         * and move_unreachable will eventually get to it
         * again.
         */
        // Manually unlink gc from unreachable list because the list functions
        // don't work right in the presence of NEXT_MASK_UNREACHABLE flags.
        PyGC_Head *prev = GC_PREV(gc);
        PyGC_Head *next = (PyGC_Head *)(gc->_gc_next & ~NEXT_MASK_UNREACHABLE);
        _PyObject_ASSERT(FROM_GC(prev),
                         prev->_gc_next & NEXT_MASK_UNREACHABLE);
        _PyObject_ASSERT(FROM_GC(next),
                         next->_gc_next & NEXT_MASK_UNREACHABLE);
        prev->_gc_next = gc->_gc_next; // copy NEXT_MASK_UNREACHABLE
        _PyGCHead_SET_PREV(next, prev);

        gc_list_append(gc, reachable);
        gc_set_refs(gc, 1);
    }
    else if (gc_refs == 0)
    {
        /* This is in move_unreachable's 'young' list, but
         * the traversal hasn't yet gotten to it.  All
         * we need to do is tell move_unreachable that it's
         * reachable.
         */
        gc_set_refs(gc, 1);
    }
    /* Else there's nothing to do.
     * If gc_refs > 0, it must be in move_unreachable's 'young'
     * list, and move_unreachable will eventually get to it.
     */
    else
    {
        _PyObject_ASSERT_WITH_MSG(op, gc_refs > 0, "refcount is too small");
    }
    return 0;
}

/* Move the unreachable objects from young to unreachable.  After this,
 * all objects in young don't have PREV_MASK_COLLECTING flag and
 * unreachable have the flag.
 * All objects in young after this are directly or indirectly reachable
 * from outside the original young; and all objects in unreachable are
 * not.
 *
 * This function restores _gc_prev pointer.  young and unreachable are
 * doubly linked list after this function.
 * But _gc_next in unreachable list has NEXT_MASK_UNREACHABLE flag.
 * So we can not gc_list_* functions for unreachable until we remove the flag.
 */
static void
move_unreachable(PyGC_Head *young, PyGC_Head *unreachable)
{
    // previous elem in the young list, used for restore gc_prev.
    PyGC_Head *prev = young;
    PyGC_Head *gc = GC_NEXT(young);

    /* Invariants:  all objects "to the left" of us in young are reachable
     * (directly or indirectly) from outside the young list as it was at entry.
     *
     * All other objects from the original young "to the left" of us are in
     * unreachable now, and have NEXT_MASK_UNREACHABLE.  All objects to the
     * left of us in 'young' now have been scanned, and no objects here
     * or to the right have been scanned yet.
     */

    while (gc != young)
    {
        if (gc_get_refs(gc))
        {
            /* gc is definitely reachable from outside the
             * original 'young'.  Mark it as such, and traverse
             * its pointers to find any other objects that may
             * be directly reachable from it.  Note that the
             * call to tp_traverse may append objects to young,
             * so we have to wait until it returns to determine
             * the next object to visit.
             */
            PyObject *op = FROM_GC(gc);
            traverseproc traverse = Py_TYPE(op)->tp_traverse;
            _PyObject_ASSERT_WITH_MSG(op, gc_get_refs(gc) > 0,
                                      "refcount is too small");
            // NOTE: visit_reachable may change gc->_gc_next when
            // young->_gc_prev == gc.  Don't do gc = GC_NEXT(gc) before!
            (void)traverse(op,
                           (visitproc)visit_reachable,
                           (void *)young);
            // relink gc_prev to prev element.
            _PyGCHead_SET_PREV(gc, prev);
            // gc is not COLLECTING state after here.
            gc_clear_collecting(gc);
            prev = gc;
        }
        else
        {
            /* This *may* be unreachable.  To make progress,
             * assume it is.  gc isn't directly reachable from
             * any object we've already traversed, but may be
             * reachable from an object we haven't gotten to yet.
             * visit_reachable will eventually move gc back into
             * young if that's so, and we'll see it again.
             */
            // Move gc to unreachable.
            // No need to gc->next->prev = prev because it is single linked.
            prev->_gc_next = gc->_gc_next;

            // We can't use gc_list_append() here because we use
            // NEXT_MASK_UNREACHABLE here.
            PyGC_Head *last = GC_PREV(unreachable);
            // NOTE: Since all objects in unreachable set has
            // NEXT_MASK_UNREACHABLE flag, we set it unconditionally.
            // But this may pollute the unreachable list head's 'next' pointer
            // too. That's semantically senseless but expedient here - the
            // damage is repaired when this function ends.
            last->_gc_next = (NEXT_MASK_UNREACHABLE | (uintptr_t)gc);
            _PyGCHead_SET_PREV(gc, last);
            gc->_gc_next = (NEXT_MASK_UNREACHABLE | (uintptr_t)unreachable);
            unreachable->_gc_prev = (uintptr_t)gc;
        }
        gc = (PyGC_Head *)prev->_gc_next;
    }
    // young->_gc_prev must be last element remained in the list.
    young->_gc_prev = (uintptr_t)prev;
    // don't let the pollution of the list head's next pointer leak
    unreachable->_gc_next &= ~NEXT_MASK_UNREACHABLE;
}

static void
untrack_tuples(PyGC_Head *head)
{
    PyGC_Head *next, *gc = GC_NEXT(head);
    while (gc != head)
    {
        PyObject *op = FROM_GC(gc);
        next = GC_NEXT(gc);
        if (PyTuple_CheckExact(op))
        {
            _PyTuple_MaybeUntrack(op);
        }
        gc = next;
    }
}

/* Try to untrack all currently tracked dictionaries */
static void
untrack_dicts(PyGC_Head *head)
{
    PyGC_Head *next, *gc = GC_NEXT(head);
    while (gc != head)
    {
        PyObject *op = FROM_GC(gc);
        next = GC_NEXT(gc);
        if (PyDict_CheckExact(op))
        {
            _PyDict_MaybeUntrack(op);
        }
        gc = next;
    }
}

/* Return true if object has a pre-PEP 442 finalization method. */
static int
has_legacy_finalizer(PyObject *op)
{
    return Py_TYPE(op)->tp_del != NULL;
}

/* Move the objects in unreachable with tp_del slots into `finalizers`.
 *
 * This function also removes NEXT_MASK_UNREACHABLE flag
 * from _gc_next in unreachable.
 */
static void
move_legacy_finalizers(PyGC_Head *unreachable, PyGC_Head *finalizers)
{
    PyGC_Head *gc, *next;
    assert((unreachable->_gc_next & NEXT_MASK_UNREACHABLE) == 0);

    /* March over unreachable.  Move objects with finalizers into
     * `finalizers`.
     */
    for (gc = GC_NEXT(unreachable); gc != unreachable; gc = next)
    {
        PyObject *op = FROM_GC(gc);

        _PyObject_ASSERT(op, gc->_gc_next & NEXT_MASK_UNREACHABLE);
        gc->_gc_next &= ~NEXT_MASK_UNREACHABLE;
        next = (PyGC_Head *)gc->_gc_next;

        if (has_legacy_finalizer(op))
        {
            gc_clear_collecting(gc);
            gc_list_move(gc, finalizers);
        }
    }
}

static inline void
clear_unreachable_mask(PyGC_Head *unreachable)
{
    /* Check that the list head does not have the unreachable bit set */
    assert(((uintptr_t)unreachable & NEXT_MASK_UNREACHABLE) == 0);

    PyGC_Head *gc, *next;
    assert((unreachable->_gc_next & NEXT_MASK_UNREACHABLE) == 0);
    for (gc = GC_NEXT(unreachable); gc != unreachable; gc = next)
    {
        _PyObject_ASSERT((PyObject *)FROM_GC(gc), gc->_gc_next & NEXT_MASK_UNREACHABLE);
        gc->_gc_next &= ~NEXT_MASK_UNREACHABLE;
        next = (PyGC_Head *)gc->_gc_next;
    }
    validate_list(unreachable, collecting_set_unreachable_clear);
}

/* A traversal callback for move_legacy_finalizer_reachable. */
static int
visit_move(PyObject *op, PyGC_Head *tolist)
{
    if (_PyObject_IS_GC(op))
    {
        PyGC_Head *gc = AS_GC(op);
        if (gc_is_collecting(gc))
        {
            gc_list_move(gc, tolist);
            gc_clear_collecting(gc);
        }
    }
    return 0;
}

/* Move objects that are reachable from finalizers, from the unreachable set
 * into finalizers set.
 */
static void
move_legacy_finalizer_reachable(PyGC_Head *finalizers)
{
    traverseproc traverse;
    PyGC_Head *gc = GC_NEXT(finalizers);
    for (; gc != finalizers; gc = GC_NEXT(gc))
    {
        /* Note that the finalizers list may grow during this. */
        traverse = Py_TYPE(FROM_GC(gc))->tp_traverse;
        (void)traverse(FROM_GC(gc),
                       (visitproc)visit_move,
                       (void *)finalizers);
    }
}

/* Clear all weakrefs to unreachable objects, and if such a weakref has a
 * callback, invoke it if necessary.  Note that it's possible for such
 * weakrefs to be outside the unreachable set -- indeed, those are precisely
 * the weakrefs whose callbacks must be invoked.  See gc_weakref.txt for
 * overview & some details.  Some weakrefs with callbacks may be reclaimed
 * directly by this routine; the number reclaimed is the return value.  Other
 * weakrefs with callbacks may be moved into the `old` generation.  Objects
 * moved into `old` have gc_refs set to GC_REACHABLE; the objects remaining in
 * unreachable are left at GC_TENTATIVELY_UNREACHABLE.  When this returns,
 * no object in `unreachable` is weakly referenced anymore.
 */
static int
handle_weakrefs(PyGC_Head *unreachable, PyGC_Head *old)
{
    PyGC_Head *gc;
    PyObject *op;           /* generally FROM_GC(gc) */
    PyWeakReference *wr;    /* generally a cast of op */
    PyGC_Head wrcb_to_call; /* weakrefs with callbacks to call */
    PyGC_Head *next;
    int num_freed = 0;

    gc_list_init(&wrcb_to_call);

    /* Clear all weakrefs to the objects in unreachable.  If such a weakref
     * also has a callback, move it into `wrcb_to_call` if the callback
     * needs to be invoked.  Note that we cannot invoke any callbacks until
     * all weakrefs to unreachable objects are cleared, lest the callback
     * resurrect an unreachable object via a still-active weakref.  We
     * make another pass over wrcb_to_call, invoking callbacks, after this
     * pass completes.
     */
    for (gc = GC_NEXT(unreachable); gc != unreachable; gc = next)
    {
        PyWeakReference **wrlist;

        op = FROM_GC(gc);
        next = GC_NEXT(gc);

        if (PyWeakref_Check(op))
        {
            /* A weakref inside the unreachable set must be cleared.  If we
             * allow its callback to execute inside delete_garbage(), it
             * could expose objects that have tp_clear already called on
             * them.  Or, it could resurrect unreachable objects.  One way
             * this can happen is if some container objects do not implement
             * tp_traverse.  Then, wr_object can be outside the unreachable
             * set but can be deallocated as a result of breaking the
             * reference cycle.  If we don't clear the weakref, the callback
             * will run and potentially cause a crash.  See bpo-38006 for
             * one example.
             */
            _PyWeakref_ClearRef((PyWeakReference *)op);
        }

        if (!_PyType_SUPPORTS_WEAKREFS(Py_TYPE(op)))
            continue;

        /* It supports weakrefs.  Does it have any?
         *
         * This is never triggered for static types so we can avoid the
         * (slightly) more costly _PyObject_GET_WEAKREFS_LISTPTR().
         */
        wrlist = _PyObject_GET_WEAKREFS_LISTPTR_FROM_OFFSET(op);

        /* `op` may have some weakrefs.  March over the list, clear
         * all the weakrefs, and move the weakrefs with callbacks
         * that must be called into wrcb_to_call.
         */
        for (wr = *wrlist; wr != NULL; wr = *wrlist)
        {
            PyGC_Head *wrasgc; /* AS_GC(wr) */

            /* _PyWeakref_ClearRef clears the weakref but leaves
             * the callback pointer intact.  Obscure:  it also
             * changes *wrlist.
             */
            _PyObject_ASSERT((PyObject *)wr, wr->wr_object == op);
            _PyWeakref_ClearRef(wr);
            _PyObject_ASSERT((PyObject *)wr, wr->wr_object == Py_None);
            if (wr->wr_callback == NULL)
            {
                /* no callback */
                continue;
            }

            /* Headache time.  `op` is going away, and is weakly referenced by
             * `wr`, which has a callback.  Should the callback be invoked?  If wr
             * is also trash, no:
             *
             * 1. There's no need to call it.  The object and the weakref are
             *    both going away, so it's legitimate to pretend the weakref is
             *    going away first.  The user has to ensure a weakref outlives its
             *    referent if they want a guarantee that the wr callback will get
             *    invoked.
             *
             * 2. It may be catastrophic to call it.  If the callback is also in
             *    cyclic trash (CT), then although the CT is unreachable from
             *    outside the current generation, CT may be reachable from the
             *    callback.  Then the callback could resurrect insane objects.
             *
             * Since the callback is never needed and may be unsafe in this case,
             * wr is simply left in the unreachable set.  Note that because we
             * already called _PyWeakref_ClearRef(wr), its callback will never
             * trigger.
             *
             * OTOH, if wr isn't part of CT, we should invoke the callback:  the
             * weakref outlived the trash.  Note that since wr isn't CT in this
             * case, its callback can't be CT either -- wr acted as an external
             * root to this generation, and therefore its callback did too.  So
             * nothing in CT is reachable from the callback either, so it's hard
             * to imagine how calling it later could create a problem for us.  wr
             * is moved to wrcb_to_call in this case.
             */
            if (gc_is_collecting(AS_GC(wr)))
            {
                /* it should already have been cleared above */
                assert(wr->wr_object == Py_None);
                continue;
            }

            /* Create a new reference so that wr can't go away
             * before we can process it again.
             */
            Py_INCREF(wr);

            /* Move wr to wrcb_to_call, for the next pass. */
            wrasgc = AS_GC(wr);
            assert(wrasgc != next); /* wrasgc is reachable, but
                                       next isn't, so they can't
                                       be the same */
            gc_list_move(wrasgc, &wrcb_to_call);
        }
    }

    /* Invoke the callbacks we decided to honor.  It's safe to invoke them
     * because they can't reference unreachable objects.
     */
    while (!gc_list_is_empty(&wrcb_to_call))
    {
        PyObject *temp;
        PyObject *callback;

        gc = (PyGC_Head *)wrcb_to_call._gc_next;
        op = FROM_GC(gc);
        _PyObject_ASSERT(op, PyWeakref_Check(op));
        wr = (PyWeakReference *)op;
        callback = wr->wr_callback;
        _PyObject_ASSERT(op, callback != NULL);

        /* copy-paste of weakrefobject.c's handle_callback() */
        temp = PyObject_CallOneArg(callback, (PyObject *)wr);
        if (temp == NULL)
            PyErr_WriteUnraisable(callback);
        else
            Py_DECREF(temp);

        /* Give up the reference we created in the first pass.  When
         * op's refcount hits 0 (which it may or may not do right now),
         * op's tp_dealloc will decref op->wr_callback too.  Note
         * that the refcount probably will hit 0 now, and because this
         * weakref was reachable to begin with, gc didn't already
         * add it to its count of freed objects.  Example:  a reachable
         * weak value dict maps some key to this reachable weakref.
         * The callback removes this key->weakref mapping from the
         * dict, leaving no other references to the weakref (excepting
         * ours).
         */
        Py_DECREF(op);
        if (wrcb_to_call._gc_next == (uintptr_t)gc)
        {
            /* object is still alive -- move it */
            gc_list_move(gc, old);
        }
        else
        {
            ++num_freed;
        }
    }

    return num_freed;
}

static void
debug_cycle(const char *msg, PyObject *op)
{
    PySys_FormatStderr("gc: %s <%s %p>\n",
                       msg, Py_TYPE(op)->tp_name, op);
}

/* Handle uncollectable garbage (cycles with tp_del slots, and stuff reachable
 * only from such cycles).
 * If DEBUG_SAVEALL, all objects in finalizers are appended to the module
 * garbage list (a Python list), else only the objects in finalizers with
 * __del__ methods are appended to garbage.  All objects in finalizers are
 * merged into the old list regardless.
 */
static void
handle_legacy_finalizers(PyThreadState *tstate,
                         GCState *gcstate,
                         PyGC_Head *finalizers, PyGC_Head *old)
{
    assert(!_PyErr_Occurred(tstate));
    assert(gcstate->garbage != NULL);

    PyGC_Head *gc = GC_NEXT(finalizers);
    for (; gc != finalizers; gc = GC_NEXT(gc))
    {
        PyObject *op = FROM_GC(gc);

        if ((gcstate->debug & DEBUG_SAVEALL) || has_legacy_finalizer(op))
        {
            if (PyList_Append(gcstate->garbage, op) < 0)
            {
                _PyErr_Clear(tstate);
                break;
            }
        }
    }
    // add_to_survived_from_young(finalizers);
    gc_list_merge(finalizers, old); // here finalizers contain new survived
}

/* Run first-time finalizers (if any) on all the objects in collectable.
 * Note that this may remove some (or even all) of the objects from the
 * list, due to refcounts falling to 0.
 */
static void
finalize_garbage(PyThreadState *tstate, PyGC_Head *collectable)
{
    destructor finalize;
    PyGC_Head seen;

    /* While we're going through the loop, `finalize(op)` may cause op, or
     * other objects, to be reclaimed via refcounts falling to zero.  So
     * there's little we can rely on about the structure of the input
     * `collectable` list across iterations.  For safety, we always take the
     * first object in that list and move it to a temporary `seen` list.
     * If objects vanish from the `collectable` and `seen` lists we don't
     * care.
     */
    gc_list_init(&seen);

    while (!gc_list_is_empty(collectable))
    {
        PyGC_Head *gc = GC_NEXT(collectable);
        PyObject *op = FROM_GC(gc);
        gc_list_move(gc, &seen);
        if (!_PyGCHead_FINALIZED(gc) &&
            (finalize = Py_TYPE(op)->tp_finalize) != NULL)
        {
            _PyGCHead_SET_FINALIZED(gc);
            Py_INCREF(op);
            finalize(op);
            assert(!_PyErr_Occurred(tstate));
            Py_DECREF(op);
        }
    }
    gc_list_merge(&seen, collectable);
}

/* Break reference cycles by clearing the containers involved.  This is
 * tricky business as the lists can be changing and we don't know which
 * objects may be freed.  It is possible I screwed something up here.
 */
static void
delete_garbage(PyThreadState *tstate, GCState *gcstate,
               PyGC_Head *collectable, PyGC_Head *old)
{
    assert(!_PyErr_Occurred(tstate));
    while (!gc_list_is_empty(collectable))
    {
        PyGC_Head *gc = GC_NEXT(collectable);
        PyObject *op = FROM_GC(gc);

        _PyObject_ASSERT_WITH_MSG(op, Py_REFCNT(op) > 0,
                                  "refcount is too small");

        if (gcstate->debug & DEBUG_SAVEALL)
        {
            assert(gcstate->garbage != NULL);
            if (PyList_Append(gcstate->garbage, op) < 0)
            {
                _PyErr_Clear(tstate);
            }
        }
        else
        {
            inquiry clear;
            if ((clear = Py_TYPE(op)->tp_clear) != NULL)
            {
                Py_INCREF(op);
                (void)clear(op);
                num_container_collected++;
                // erase_from_global((uintptr_t)op);
                if (_PyErr_Occurred(tstate))
                {
                    _PyErr_WriteUnraisableMsg("in tp_clear of",
                                              (PyObject *)Py_TYPE(op));
                }
                Py_DECREF(op);
            }
        }
        if (GC_NEXT(collectable) == gc)
        {
            /* object is still alive, move it, it may die later */
            gc_clear_collecting(gc);
            gc_list_move(gc, old);
        }
    }
}

/* Clear all free lists
 * All free lists are cleared during the collection of the highest generation.
 * Allocated items in the free list may keep a pymalloc arena occupied.
 * Clearing the free lists may give back memory to the OS earlier.
 */
static void
clear_freelists(PyInterpreterState *interp)
{
    _PyTuple_ClearFreeList(interp);
    _PyFloat_ClearFreeList(interp);
    _PyList_ClearFreeList(interp);
    _PyDict_ClearFreeList(interp);
    _PyAsyncGen_ClearFreeLists(interp);
    _PyContext_ClearFreeList(interp);
}

// Show stats for objects in each generations
static void
show_stats_each_generations(GCState *gcstate)
{
    char buf[100];
    size_t pos = 0;

    for (int i = 0; i < NUM_GENERATIONS && pos < sizeof(buf); i++)
    {
        pos += PyOS_snprintf(buf + pos, sizeof(buf) - pos,
                             " %zd",
                             gc_list_size(GEN_HEAD(gcstate, i)));
    }

    PySys_FormatStderr(
        "gc: objects in each generation:%s\n"
        "gc: objects in permanent generation: %zd\n",
        buf, gc_list_size(&gcstate->permanent_generation.head));
}

/* Deduce which objects among "base" are unreachable from outside the list
   and move them to 'unreachable'. The process consist in the following steps:

1. Copy all reference counts to a different field (gc_prev is used to hold
   this copy to save memory).
2. Traverse all objects in "base" and visit all referred objects using
   "tp_traverse" and for every visited object, subtract 1 to the reference
   count (the one that we copied in the previous step). After this step, all
   objects that can be reached directly from outside must have strictly positive
   reference count, while all unreachable objects must have a count of exactly 0.
3. Identify all unreachable objects (the ones with 0 reference count) and move
   them to the "unreachable" list. This step also needs to move back to "base" all
   objects that were initially marked as unreachable but are referred transitively
   by the reachable objects (the ones with strictly positive reference count).

Contracts:

    * The "base" has to be a valid list with no mask set.

    * The "unreachable" list must be uninitialized (this function calls
      gc_list_init over 'unreachable').

IMPORTANT: This function leaves 'unreachable' with the NEXT_MASK_UNREACHABLE
flag set but it does not clear it to skip unnecessary iteration. Before the
flag is cleared (for example, by using 'clear_unreachable_mask' function or
by a call to 'move_legacy_finalizers'), the 'unreachable' list is not a normal
list and we can not use most gc_list_* functions for it. */
static inline void
deduce_unreachable(PyGC_Head *base, PyGC_Head *unreachable)
{
    validate_list(base, collecting_clear_unreachable_clear);
    /* Using ob_refcnt and gc_refs, calculate which objects in the
     * container set are reachable from outside the set (i.e., have a
     * refcount greater than 0 when all the references within the
     * set are taken into account).
     */
    update_refs(base); // gc_prev is used for gc_refs
    subtract_refs(base);

    /* Leave everything reachable from outside base in base, and move
     * everything else (in base) to unreachable.
     *
     * NOTE:  This used to move the reachable objects into a reachable
     * set instead.  But most things usually turn out to be reachable,
     * so it's more efficient to move the unreachable things.  It "sounds slick"
     * to move the unreachable objects, until you think about it - the reason it
     * pays isn't actually obvious.
     *
     * Suppose we create objects A, B, C in that order.  They appear in the young
     * generation in the same order.  If B points to A, and C to B, and C is
     * reachable from outside, then the adjusted refcounts will be 0, 0, and 1
     * respectively.
     *
     * When move_unreachable finds A, A is moved to the unreachable list.  The
     * same for B when it's first encountered.  Then C is traversed, B is moved
     * _back_ to the reachable list.  B is eventually traversed, and then A is
     * moved back to the reachable list.
     *
     * So instead of not moving at all, the reachable objects B and A are moved
     * twice each.  Why is this a win?  A straightforward algorithm to move the
     * reachable objects instead would move A, B, and C once each.
     *
     * The key is that this dance leaves the objects in order C, B, A - it's
     * reversed from the original order.  On all _subsequent_ scans, none of
     * them will move.  Since most objects aren't in cycles, this can save an
     * unbounded number of moves across an unbounded number of later collections.
     * It can cost more only the first time the chain is scanned.
     *
     * Drawback:  move_unreachable is also used to find out what's still trash
     * after finalizers may resurrect objects.  In _that_ case most unreachable
     * objects will remain unreachable, so it would be more efficient to move
     * the reachable objects instead.  But this is a one-time cost, probably not
     * worth complicating the code to speed just a little.
     */
    gc_list_init(unreachable);
    move_unreachable(base, unreachable); // gc_prev is pointer again
    validate_list(base, collecting_clear_unreachable_clear);
    validate_list(unreachable, collecting_set_unreachable_set);
}

/* Handle objects that may have resurrected after a call to 'finalize_garbage', moving
   them to 'old_generation' and placing the rest on 'still_unreachable'.

   Contracts:
       * After this function 'unreachable' must not be used anymore and 'still_unreachable'
         will contain the objects that did not resurrect.

       * The "still_unreachable" list must be uninitialized (this function calls
         gc_list_init over 'still_unreachable').

IMPORTANT: After a call to this function, the 'still_unreachable' set will have the
PREV_MARK_COLLECTING set, but the objects in this set are going to be removed so
we can skip the expense of clearing the flag to avoid extra iteration. */
static inline void
handle_resurrected_objects(PyGC_Head *unreachable, PyGC_Head *still_unreachable,
                           PyGC_Head *old_generation)
{
    // Remove the PREV_MASK_COLLECTING from unreachable
    // to prepare it for a new call to 'deduce_unreachable'
    gc_list_clear_collecting(unreachable);

    // After the call to deduce_unreachable, the 'still_unreachable' set will
    // have the PREV_MARK_COLLECTING set, but the objects are going to be
    // removed so we can skip the expense of clearing the flag.
    PyGC_Head *resurrected = unreachable;
    deduce_unreachable(resurrected, still_unreachable);
    clear_unreachable_mask(still_unreachable);

    // Move the resurrected objects to the old generation for future collection.
    // add_to_survived_from_young(resurrected);
    gc_list_merge(resurrected, old_generation);
}

int insert_2_merge_op_gc(op_gc_table *table, PyGC_Head *each_reachable)
{
    uint8_t dummy_val;
    PyGC_Head *gc;
    unsigned long num_ops = 0;
    for (gc = GC_NEXT(each_reachable); gc != each_reachable; gc = GC_NEXT(gc))
    // while (!gc_list_is_empty(each_reachable))
    {
        num_ops += 1;
        // PyGC_Head *gc = GC_NEXT(each_reachable);
        PyObject *op = FROM_GC(gc);
        uintptr_t casted_op = (uintptr_t)op;
        // if (!op_gc_table_contains(table, &casted_op))
        {
            op_gc_table_insert(table, &casted_op, &dummy_val);
        }
        // else
        // {
        //     op_gc_table_update(table, &casted_op, &dummy_val);
        // }
    }
    return num_ops;
}

int delete_2_merge_op_gc(op_gc_table *table, PyGC_Head *unreachable)
{
    PyGC_Head *gc;
    unsigned long num_ops = 0;
    for (gc = GC_NEXT(unreachable); gc != unreachable; gc = GC_NEXT(gc))
    // while (!gc_list_is_empty(unreachable))
    {
        num_ops += 1;
        // PyGC_Head *gc = GC_NEXT(unreachable);
        PyObject *op = FROM_GC(gc);
        uintptr_t casted_op = (uintptr_t)op;
        // if (!op_gc_table_contains(table, &casted_op))
        // {
        op_gc_table_erase(table, &casted_op);
        // }
    }
    return num_ops;
}

void debug_type_set(PyGC_Head *gc_head, char *insert_or_delete)
{
    PyGC_Head *gc;
    for (gc = GC_NEXT(gc_head); gc != gc_head; gc = GC_NEXT(gc))
    {
        PyObject *op = FROM_GC(gc);
        PyTypeObject *get_type = Py_TYPE(op);
        if ((void *)get_type == (void *)0xffffffffffffffff)
        {
            fprintf(stderr, "type not set duing %s\n", insert_or_delete);
        }
    }
}

static void empty_global_heat_utlist()
{
    heat_node *elt, *tmp;
    DL_FOREACH_SAFE(global_heat_utlist, elt, tmp)
    {
        DL_DELETE(global_heat_utlist, elt);
        free(elt);
    }
    global_heat_utlist = NULL;
}

int check_io_type(PyObject *obj)
{
    PyObject *io_module = PyImport_ImportModule("io");
    if (!io_module)
    {
        PyErr_Print();
    }
    // const char *io_types[] = {"IOBase", "IncrementalNewlineDecoder", "RawIOBase", "BufferedIOBase", "BufferedRWPair", "BufferedRandom", "BufferedReader",
    //                           "BufferedWriter", "BytesIOBuffer", "BytesIO", "FileIO", "StringIO", "TextIOBase", "TextIOWrapper", NULL};
    const char *io_types[] = {"FileIO", "BufferedReader", "BufferedWriter", "TextIOWrapper", NULL};
    for (int i = 0; io_types[i] != NULL; i++)
    {
        PyObject *io_type = PyObject_GetAttrString(io_module, io_types[i]);
        if (PyObject_IsInstance(obj, io_type))
        {
            Py_DECREF(io_type);
            // break;
            fprintf(stderr, "skipping %ld, IO type: %s\n", (uintptr_t)obj, io_types[i]);
            return 1;
        }
        Py_DECREF(io_type);
    }
    Py_DECREF(io_module);
    return 0;
}
int check_ipaddr_type(PyObject *obj)
{
    PyObject *ipaddress_module = PyImport_ImportModule("ipaddress");
    if (!ipaddress_module)
    {
        PyErr_Print();
    }
    const char *ipaddr_types[] = {"IPv4Address", "IPv6Address", "IPv4Network", "IPv6Network", NULL};
    for (int i = 0; ipaddr_types[i] != NULL; i++)
    {
        PyObject *ipaddr_type = PyObject_GetAttrString(ipaddress_module, ipaddr_types[i]);
        // if (!ipv4address_class)
        // {
        //     PyErr_Print();
        //     Py_DECREF(ipaddress_module);
        //     return 1;
        // }
        if (PyObject_IsInstance(obj, ipaddr_type))
        {
            Py_DECREF(ipaddr_type);
            // fprintf(stderr, "skipping %ld, ipaddr type: %s\n", (uintptr_t)obj, ipaddr_types[i]);
            return 1;
        }
        Py_DECREF(ipaddr_type);
    }
    Py_DECREF(ipaddress_module);
    return 0;
}

int check_itertools_count_type(PyObject *obj)
{
    PyObject *itertools_module, *count_type;
    itertools_module = PyImport_ImportModule("itertools");
    if (!itertools_module)
    {
        PyErr_Print();
    }
    count_type = PyObject_GetAttrString(itertools_module, "count");
    if (PyObject_IsInstance(obj, count_type))
    {
        Py_DECREF(count_type);
        fprintf(stderr, "skipping %ld, itertools.count\n", (uintptr_t)obj);
        return 1;
    }
    Py_DECREF(count_type);

    return 0;
}

int check_object_type(PyObject *obj)
{
    if (check_io_type(obj) == 1)
    {
        return 1;
    }
    else if (check_ipaddr_type(obj) == 1)
    {
        return 1;
    }
    else if (check_itertools_count_type(obj) == 1)
    {
        return 1;
    }
    else if (PyGen_Check(obj))
        return 1;
    return 0;
}
static double try_cascading_old(int slow_idx)
{
    struct timespec start, end;
    double elapsed;
    clock_gettime(CLOCK_MONOTONIC, &start);

    if (!enable_bk || !global_old || gen_idx == -1)
    {
        fprintf(stderr, "returned\n");
        return;
    }
    // clock_t start_gil = clock();
    PyGILState_STATE gstate = PyGILState_Ensure();
    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = end.tv_sec - start.tv_sec;
    elapsed += (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    fprintf(stderr, "\nentering slow.. acquire GIL time: %.3f\n", elapsed);

    PyGC_Head *gc;
    PyObject *container_op;
    int ret;
    fprintf(stderr, "global_old: %ld, generation: %d\n", (uintptr_t)global_old, gen_idx);
    uintptr_t local_lowest_op = UINTPTR_MAX;
    uintptr_t cur_gen_low_bound = gen_id_bound[gen_idx].low;
    if (gen_id_bound[gen_idx].head == 0)
    {
        gen_id_bound[gen_idx].head = (uintptr_t)global_old;
    }
    else if (gen_id_bound[gen_idx].head != (uintptr_t)global_old)
    {
        fprintf(stderr, "head changed! not cool!\n");
    }
    // struct timespec ts;
    // clock_gettime(CLOCK_MONOTONIC, &ts);
    if (0) // append first, cascade later
    {
        // unsigned long count = 0;
        for (gc = GC_NEXT(global_old); gc != global_old; gc = GC_NEXT(gc))
        {
            container_op = FROM_GC(gc);
            uintptr_t casted_op = (uintptr_t)container_op;
            // unsigned long cur_len = Py_SIZE(container_op); // || cur_len > 10000000
            // if (cur_len > 5)
            //     continue;
            if (casted_op < cur_gen_low_bound)
            {
                // if (!Py_TYPE(container_op)->tp_iter || check_io_type(container_op)) // this is slow
                //     continue;
                if (casted_op < local_lowest_op && casted_op > LOWER_BOUND)
                {
                    local_lowest_op = casted_op;
                }
                kv_push(PyObject *, local_ptr_vec, container_op);
            }
            // count++;
        }
        gen_id_bound[gen_idx].low = local_lowest_op;
    }
    {
        PyGC_Head *oldest_gen_head = (PyGC_Head *)gen_id_bound[NUM_GENERATIONS - 1].head;
        PyGC_Head *tracing_head = (oldest_gen_head == NULL) ? global_old : oldest_gen_head; // by default, last gen, otherwise (empty), use recently GC-ed gen
        PyThreadState *tstate = _PyThreadState_GET();
        GCState *gcstate = &tstate->interp->gc;
        for (int i = 0; i < NUM_GENERATIONS; i++)
        {
            tracing_head = GEN_HEAD(gcstate, i);
            for (gc = GC_NEXT(tracing_head); gc != tracing_head; gc = GC_NEXT(gc))
            {
                container_op = FROM_GC(gc);
                uintptr_t casted_op = (uintptr_t)container_op;
                // if (casted_op < cur_gen_low_bound)
                {
                    if (check_in_global(casted_op))
                    // if (found_in_kset_helper(container_op))
                    // if (check_in_libcuckoo(casted_op))
                    {
                        continue;
                    }
                    // unsigned long cur_len = Py_SIZE(container_op); // || cur_len > 10000000
                    // if (cur_len > LEN_THRESHOLD)
                    //     continue;
                    if (casted_op < local_lowest_op && casted_op > LOWER_BOUND)
                    {
                        local_lowest_op = casted_op;
                    }
                    insert_into_global(casted_op); // dedup
                    // insert_into_libcuckoo(casted_op);
                    // kh_put(ptrset, global_op_set, casted_op, &ret);
                    // insert_global_set_helper(container_op);
                    kv_push(PyObject *, local_ptr_vec, container_op); // vector, dynamic resizing
                    unsigned int combined = 0;
                    update_recursive_visitor(container_op, &combined); // cascading
                }
            }
        }
        gen_id_bound[gen_idx].low = local_lowest_op;
    }
    // now, delete the collected objs
    // remove_dead_container_op(); // TODO: double check if necessary

    // PyGILState_STATE gstate = PyGILState_Ensure();
    // fprintf(stderr, "global_set_size: %u\n", my_set_size);
    PyGILState_Release(gstate);

    // print_addr(global_bookkeep_args->fd, slow_idx);
    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = end.tv_sec - start.tv_sec;
    elapsed += (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    fprintf(stderr, "slow trace time (including holding GIL): %.3f seconds, new op: %ld\n", elapsed, kv_size(local_ptr_vec));
    return elapsed;
}
unsigned int old_num_op = 0;
unsigned int prev_num_op = 0;
unsigned int last_global_size = 0;
int gen_temps()
{
    clock_t t = clock();
    unsigned int num_op = 0;
    if (!all_temps) // first time populate
    {
        // first time / reset metadata --> malloc
        free(all_temps);
        num_op = kv_size(local_ptr_vec);
        all_temps = (OBJ_TEMP *)calloc(num_op, sizeof(OBJ_TEMP));
        if (all_temps == NULL)
        {
            fprintf(stderr, "Failed to allocate memory for all ops\n");
            return 0;
        }
        for (int i = 0; i < num_op; i++)
        {
            all_temps[i].op = kv_A(local_ptr_vec, i);
            all_temps[i].prev_refcnt = 0;
            // all_temps[i].diffs[0] = 1; // newly init ones, mark diffs[0] as 1
        }
        old_num_op = num_op;
        fprintf(stderr, "first time appended %u nodes\n", num_op);
        fprintf(stderr, "forming all_temp time: %.3f sec\n", (float)(clock() - t) / CLOCKS_PER_SEC);
        return 1;
    }
    else
    {
        // not first time --> realloc
        num_op = kv_size(local_ptr_vec);
        prev_num_op = old_num_op;
        unsigned int new_num_op = num_op + old_num_op;
        OBJ_TEMP *temp = (OBJ_TEMP *)realloc(all_temps, new_num_op * sizeof(OBJ_TEMP));
        if (temp == NULL)
        {
            fprintf(stderr, "Failed to realloc\n");
            free(all_temps);
            return 0;
        }
        all_temps = temp;
        for (int i = old_num_op; i < new_num_op; i++)
        {
            all_temps[i].op = kv_A(local_ptr_vec, i - old_num_op);
            all_temps[i].prev_refcnt = 0;
            // all_temps[i].diffs[0] = 1; // newly init ones, mark diffs[0] as 1
        }
        old_num_op = new_num_op;
        fprintf(stderr, "newly appended: %u, total: %u\n", num_op, old_num_op);
        fprintf(stderr, "forming all_temp time: %.3f sec\n", (float)(clock() - t) / CLOCKS_PER_SEC);
        return 2;
    }
}

void gen_temps_refchain()
{
    clock_t t;
    t = clock();
    unsigned int num_op;
    if (all_temps)
        free(all_temps);
    if (kv_size(local_ptr_vec) > 0)
    {
        // first time --> malloc
        num_op = kv_size(local_ptr_vec);
        all_temps = (OBJ_TEMP *)malloc(num_op * sizeof(OBJ_TEMP));
        if (all_temps == NULL)
        {
            fprintf(stderr, "Failed to allocate memory for all ops\n");
            return -1;
        }
        for (int i = 0; i < num_op; i++)
        {
            all_temps[i].op = kv_A(local_ptr_vec, i);
            all_temps[i].prev_refcnt = 0;
        }
        old_num_op = num_op;
        // fprintf(stderr, "first time appended %u nodes\n", num_op);
    }
    fprintf(stderr, "total: %u, forming all_temp time: %.3f sec\n", old_num_op, (float)(clock() - t) / CLOCKS_PER_SEC);
}

// return value:
// -1: no new op & a lot recent collected ops, all_temps freed, sleep and do next slow
// 0: slow scan not triggered， use old all_temps
// 1: slow scan triggered, all_teamp formed
// 2: all_temps realloced
static int try_trigger_slow_scan()
{
    // fprintf(stderr, "entering trigger slow scan...\n");
    // if (!enable_bk)
    //     return -1; // this would never happens
    // double durationInSeconds = (double)time_since_gc_triggerd / CLOCKS_PER_SEC;
    // fprintf(stderr, "time_since_gc_triggerd: %ld, durationInSeconds: %.3f, get_live_time_thresh: %ld\n", time_since_gc_triggerd, durationInSeconds, get_live_time_thresh);
    // if (time_since_gc_triggerd > get_live_time_thresh) // current: 1s, is a good threshold?
    // if (num_gc_cycles <= 5)
    // {
    //     fprintf(stderr, "skipping slow...\n");
    //     num_gc_cycles = 0;
    //     return 0;
    // }
    unsigned int cur_global_size = get_global_size();
    // unsigned int cur_global_size = get_libcuckoo_size();
    // unsigned int cur_global_size = get_global_kh_size();
    fprintf(stderr, "num_gc_cycles: %ld\n", num_gc_cycles);
    // PyGILState_STATE gstate = PyGILState_Ensure();
    // PyThreadState *tstate = _PyThreadState_GET();
    // PyInterpreterState *interp = tstate->interp;
    // GCState *gcstate = &interp->gc;
    // PyGILState_Release(gstate);
    fprintf(stderr, "global_try2_sched: %ld\n", global_try2_sched);
    fprintf(stderr, "recent num_container_collected: %lu, global size: %d\n", num_container_collected, cur_global_size);
    // if (cur_global_size < 1000)
    // {
    //     fprintf(stderr, "zero global size, no need for fast scan\n");
    //     old_num_op = 0;
    //     num_container_collected = 0;
    //     global_try2_sched = 0;
    //     num_gc_cycles = 0;
    //     return -1;
    // }
    if (num_container_collected == 0 || num_container_collected < cur_global_size)
    {
        fprintf(stderr, "yesneed\n");
        need2_check_set = 0;
    }
    else
    {
        fprintf(stderr, "noneed\n");
        need2_check_set = 1;
    }

    if (!very_first_bk && (global_try2_sched < 100 && num_container_collected < 100000)) // if either newly created container_op or recently connected op is small, then just skip slow
    // if (!very_first_bk)
    {
        fprintf(stderr, "I don't see much container op created, skip slow...\n");
        // num_container_collected = 0;
        // global_try2_sched = 0;
        return 0;
    }
    // do cascade tracing
    kv_init(local_ptr_vec);
    double cur_cascading_time = try_cascading_old(total_num_slow);
    {
        total_cur_cascading_time += cur_cascading_time;
        total_num_slow++;
    }
    fprintf(stderr, "total_cur_cascading_time: %.3f, total_num_slow: %d\n", total_cur_cascading_time, total_num_slow);
    int ret;
    fprintf(stderr, "new_op size: %d\n", kv_size(local_ptr_vec));

    // if (div > 1.2)
    // {
    //     fprintf(stderr, "regen temps\n");
    //     free(all_temps);
    //     all_temps = NULL;
    //     last_global_size = cur_global_size;
    //     num_container_collected = 0;
    //     global_try2_sched = 0;
    //     num_gc_cycles = 0;
    //     clock_t pop_start = clock();
    //     fprintf(stderr, "pop time: %.3f\n", (float)(clock() - pop_start) / CLOCKS_PER_SEC);

    //     kv_destroy(local_ptr_vec);
    //     return 1;
    // }
    // if (kv_size(local_ptr_vec) > 1000)
    {
        ret = gen_temps(); // 0. error, 1. reset all_temps, 2. realloc (append) to all_temps
        if (!ret)
            fprintf(stderr, "\nerror\n");
    }
    double div = (double)old_num_op / cur_global_size;
    fprintf(stderr, "div: %.3f\n", div);
    // else if (num_container_collected > 1000000)
    int num_inc_op = cur_global_size - last_global_size;
    fprintf(stderr, "num_inc_op: %d, cur_global_size: %d, old_num_op: %d\n", num_inc_op, cur_global_size, old_num_op);

    if (num_inc_op < 0)
    {
        fprintf(stderr, "yesneed\n");
        need2_check_set = 1;
        //     fprintf(stderr, "free all_temps, relexing slow....\n");
        //     free(all_temps);
        //     all_temps = NULL;
        //     old_num_op = 0;
        //     num_container_collected = 0;
        //     global_try2_sched = 0;
        //     num_gc_cycles = 0;
        //     kv_destroy(local_ptr_vec);
        //     last_global_size = cur_global_size;

        //     return -1;
    }
    kv_destroy(local_ptr_vec);
    num_container_collected = 0;
    global_try2_sched = 0;
    num_gc_cycles = 0;
    // if (very_first_bk)
    //     very_first_bk = false;
    last_global_size = cur_global_size;
    return ret;
}

static void add_to_survived_from_young(PyGC_Head *survived)
{
    if (!enable_bk)
        return;
    PyGC_Head *gc;
    int ret;
    for (gc = GC_NEXT(survived); gc != survived; gc = GC_NEXT(gc))
    {
        PyObject *op = FROM_GC(gc);
        // try_append_survived_set(op);
    }
}

/* This is the main function.  Read this to understand how the
 * collection process works. */
static Py_ssize_t
gc_collect_main(PyThreadState *tstate, int generation,
                Py_ssize_t *n_collected, Py_ssize_t *n_uncollectable,
                int nofail)
{
    int i;
    Py_ssize_t m = 0;      /* # objects collected */
    Py_ssize_t n = 0;      /* # unreachable objects that couldn't be collected */
    PyGC_Head *young;      /* the generation we are examining */
    PyGC_Head *old;        /* next older generation */
    PyGC_Head unreachable; /* non-problematic unreachable trash */
    PyGC_Head finalizers;  /* objects with, & reachable from, __del__ */
    PyGC_Head *gc;
    _PyTime_t t1 = 0; /* initialize to prevent a compiler warning */
    GCState *gcstate = &tstate->interp->gc;

    // gc_collect_main() must not be called before _PyGC_Init
    // or after _PyGC_Fini()
    assert(gcstate->garbage != NULL);
    assert(!_PyErr_Occurred(tstate));

    if (gcstate->debug & DEBUG_STATS)
    {
        PySys_WriteStderr("gc: collecting generation %d...\n", generation);
        show_stats_each_generations(gcstate);
        t1 = _PyTime_GetPerfCounter();
    }

    if (PyDTrace_GC_START_ENABLED())
        PyDTrace_GC_START(generation);

    /* update collection and allocation counters */
    if (generation + 1 < NUM_GENERATIONS)
        gcstate->generations[generation + 1].count += 1;
    for (i = 0; i <= generation; i++)
        gcstate->generations[i].count = 0;

    /* merge younger generations with one we are currently collecting */
    for (i = 0; i < generation; i++)
    {
        gc_list_merge(GEN_HEAD(gcstate, i), GEN_HEAD(gcstate, generation));
    }

    /* handy references */
    young = GEN_HEAD(gcstate, generation);
    if (generation < NUM_GENERATIONS - 1) // 0, 1
        old = GEN_HEAD(gcstate, generation + 1);
    else
        old = young;
    validate_list(old, collecting_clear_unreachable_clear);

    deduce_unreachable(young, &unreachable);

    untrack_tuples(young);
    // add_to_survived_from_young(young);
    /* Move reachable objects to next generation. */
    if (young != old) // 0, 1
    {
        if (generation == NUM_GENERATIONS - 2) // 1
        {
            gcstate->long_lived_pending += gc_list_size(young);
        }
        /* insert still reachable to global table
         *   `young` is HEAD of all objs that are still reachable (to be merged). */
        gc_list_merge(young, old);
    }
    else
    { // if the oldest gen
        /* We only un-track dicts in full collections, to avoid quadratic
        dict build-up. See issue #14775. */
        untrack_dicts(young);
        gcstate->long_lived_pending = 0;
        gcstate->long_lived_total = gc_list_size(young);
    }

    /* All objects in unreachable are trash, but objects reachable from
     * legacy finalizers (e.g. tp_del) can't safely be deleted.
     */
    gc_list_init(&finalizers);
    // NEXT_MASK_UNREACHABLE is cleared here.
    // After move_legacy_finalizers(), unreachable is normal list.
    move_legacy_finalizers(&unreachable, &finalizers);
    /* finalizers contains the unreachable objects with a legacy finalizer;
     * unreachable objects reachable *from* those are also uncollectable,
     * and we move those into the finalizers list too.
     */
    move_legacy_finalizer_reachable(&finalizers);

    validate_list(&finalizers, collecting_clear_unreachable_clear);
    validate_list(&unreachable, collecting_set_unreachable_clear);

    /* Print debugging information. */
    if (gcstate->debug & DEBUG_COLLECTABLE)
    {
        for (gc = GC_NEXT(&unreachable); gc != &unreachable; gc = GC_NEXT(gc))
        {
            debug_cycle("collectable", FROM_GC(gc));
        }
    }

    /* Clear weakrefs and invoke callbacks as necessary. */
    m += handle_weakrefs(&unreachable, old);

    validate_list(old, collecting_clear_unreachable_clear);
    validate_list(&unreachable, collecting_set_unreachable_clear);

    /* Call tp_finalize on objects which have one. */
    finalize_garbage(tstate, &unreachable);

    /* Handle any objects that may have resurrected after the call
     * to 'finalize_garbage' and continue the collection with the
     * objects that are still unreachable */
    PyGC_Head final_unreachable;
    handle_resurrected_objects(&unreachable, &final_unreachable, old);

    /* Call tp_clear on objects in the final_unreachable set.  This will cause
     * the reference cycles to be broken.  It may also cause some objects
     * in finalizers to be freed.
     */
    m += gc_list_size(&final_unreachable);
    /* delete (future) unreachable objs */
    {
        delete_garbage(tstate, gcstate, &final_unreachable, old);
    }

    /* Collect statistics on uncollectable objects found and print
     * debugging information. */
    for (gc = GC_NEXT(&finalizers); gc != &finalizers; gc = GC_NEXT(gc))
    {
        n++;
        if (gcstate->debug & DEBUG_UNCOLLECTABLE)
            debug_cycle("uncollectable", FROM_GC(gc));
    }
    if (gcstate->debug & DEBUG_STATS)
    {
        double d = _PyTime_AsSecondsDouble(_PyTime_GetPerfCounter() - t1);
        PySys_WriteStderr(
            "gc: done, %zd unreachable, %zd uncollectable, %.4fs elapsed\n",
            n + m, n, d);
    }
    // fprintf(stderr, "%zd unreachable, %zd collected, %zd uncollectable\n", n + m, m, n);

    /* Append instances in the uncollectable set to a Python
     * reachable list of garbage.  The programmer has to deal with
     * this if they insist on creating this type of structure.
     */
    handle_legacy_finalizers(tstate, gcstate, &finalizers, old);
    validate_list(old, collecting_clear_unreachable_clear);
    global_old = old;
    gen_idx = generation;

    /* Clear free list only during the collection of the highest
     * generation */
    if (generation == NUM_GENERATIONS - 1)
    {
        clear_freelists(tstate->interp);
    }

    if (_PyErr_Occurred(tstate))
    {
        if (nofail)
        {
            _PyErr_Clear(tstate);
        }
        else
        {
            _PyErr_WriteUnraisableMsg("in garbage collection", NULL);
        }
    }

    /* Update stats */
    if (n_collected)
    {
        *n_collected = m;
    }
    if (n_uncollectable)
    {
        *n_uncollectable = n;
    }

    struct gc_generation_stats *stats = &gcstate->generation_stats[generation];
    stats->collections++;
    stats->collected += m;
    stats->uncollectable += n;

    if (PyDTrace_GC_DONE_ENABLED())
    {
        PyDTrace_GC_DONE(n + m);
    }

    assert(!_PyErr_Occurred(tstate));
    return n + m;
}

/* Invoke progress callbacks to notify clients that garbage collection
 * is starting or stopping
 */
static void
invoke_gc_callback(PyThreadState *tstate, const char *phase,
                   int generation, Py_ssize_t collected,
                   Py_ssize_t uncollectable)
{
    assert(!_PyErr_Occurred(tstate));

    /* we may get called very early */
    GCState *gcstate = &tstate->interp->gc;
    if (gcstate->callbacks == NULL)
    {
        return;
    }

    /* The local variable cannot be rebound, check it for sanity */
    assert(PyList_CheckExact(gcstate->callbacks));
    PyObject *info = NULL;
    if (PyList_GET_SIZE(gcstate->callbacks) != 0)
    {
        info = Py_BuildValue("{sisnsn}",
                             "generation", generation,
                             "collected", collected,
                             "uncollectable", uncollectable);
        if (info == NULL)
        {
            PyErr_WriteUnraisable(NULL);
            return;
        }
    }

    PyObject *phase_obj = PyUnicode_FromString(phase);
    if (phase_obj == NULL)
    {
        Py_XDECREF(info);
        PyErr_WriteUnraisable(NULL);
        return;
    }

    PyObject *stack[] = {phase_obj, info};
    for (Py_ssize_t i = 0; i < PyList_GET_SIZE(gcstate->callbacks); i++)
    {
        PyObject *r, *cb = PyList_GET_ITEM(gcstate->callbacks, i);
        Py_INCREF(cb); /* make sure cb doesn't go away */
        r = PyObject_Vectorcall(cb, stack, 2, NULL);
        if (r == NULL)
        {
            PyErr_WriteUnraisable(cb);
        }
        else
        {
            Py_DECREF(r);
        }
        Py_DECREF(cb);
    }
    Py_DECREF(phase_obj);
    Py_XDECREF(info);
    assert(!_PyErr_Occurred(tstate));
    if (!strcmp(phase, "stop"))
    {
        num_gc_cycles++;
    }
}

/* Perform garbage collection of a generation and invoke
 * progress callbacks.
 */
static Py_ssize_t
gc_collect_with_callback(PyThreadState *tstate, int generation)
{
    assert(!_PyErr_Occurred(tstate));
    Py_ssize_t result, collected, uncollectable;
    invoke_gc_callback(tstate, "start", generation, 0, 0);
    result = gc_collect_main(tstate, generation, &collected, &uncollectable, 0);
    invoke_gc_callback(tstate, "stop", generation, collected, uncollectable);
    assert(!_PyErr_Occurred(tstate));
    return result;
}

static Py_ssize_t
gc_collect_generations(PyThreadState *tstate)
{
    GCState *gcstate = &tstate->interp->gc;
    /* Find the oldest generation (highest numbered) where the count
     * exceeds the threshold.  Objects in the that generation and
     * generations younger than it will be collected. */
    Py_ssize_t n = 0;
    for (int i = NUM_GENERATIONS - 1; i >= 0; i--)
    {
        if (gcstate->generations[i].count > gcstate->generations[i].threshold)
        {
            /* Avoid quadratic performance degradation in number
            of tracked objects (see also issue #4074):

            To limit the cost of garbage collection, there are two strategies;
                - make each collection faster, e.g. by scanning fewer objects
                - do less collections
            This heuristic is about the latter strategy.

            In addition to the various configurable thresholds, we only trigger a
            full collection if the ratio

                long_lived_pending / long_lived_total

            is above a given value (hardwired to 25%).

            The reason is that, while "non-full" collections (i.e., collections of
            the young and middle generations) will always examine roughly the same
            number of objects -- determined by the aforementioned thresholds --,
            the cost of a full collection is proportional to the total number of
            long-lived objects, which is virtually unbounded.

            Indeed, it has been remarked that doing a full collection every
            <constant number> of object creations entails a dramatic performance
            degradation in workloads which consist in creating and storing lots of
            long-lived objects (e.g. building a large list of GC-tracked objects would
            show quadratic performance, instead of linear as expected: see issue #4074).

            Using the above ratio, instead, yields amortized linear performance in
            the total number of objects (the effect of which can be summarized
            thusly: "each full garbage collection is more and more costly as the
            number of objects grows, but we do fewer and fewer of them").

            This heuristic was suggested by Martin von Löwis on python-dev in
            June 2008. His original analysis and proposal can be found at:
            http://mail.python.org/pipermail/python-dev/2008-June/080579.html
            */
            if (i == NUM_GENERATIONS - 1 && gcstate->long_lived_pending < gcstate->long_lived_total / 4)
                continue;
            n = gc_collect_with_callback(tstate, i);
            break;
        }
    }
    return n;
}

#include "clinic/gcmodule.c.h"

/*[clinic input]
gc.enable

Enable automatic garbage collection.
[clinic start generated code]*/

static PyObject *
gc_enable_impl(PyObject *module)
/*[clinic end generated code: output=45a427e9dce9155c input=81ac4940ca579707]*/
{
    PyGC_Enable();
    Py_RETURN_NONE;
}

/*[clinic input]
gc.disable

Disable automatic garbage collection.
[clinic start generated code]*/

static PyObject *
gc_disable_impl(PyObject *module)
/*[clinic end generated code: output=97d1030f7aa9d279 input=8c2e5a14e800d83b]*/
{
    PyGC_Disable();
    Py_RETURN_NONE;
}

/*[clinic input]
gc.isenabled -> bool

Returns true if automatic garbage collection is enabled.
[clinic start generated code]*/

static int
gc_isenabled_impl(PyObject *module)
/*[clinic end generated code: output=1874298331c49130 input=30005e0422373b31]*/
{
    return PyGC_IsEnabled();
}

/*[clinic input]
gc.collect -> Py_ssize_t

    generation: int(c_default="NUM_GENERATIONS - 1") = 2

Run the garbage collector.

With no arguments, run a full collection.  The optional argument
may be an integer specifying which generation to collect.  A ValueError
is raised if the generation number is invalid.

The number of unreachable objects is returned.
[clinic start generated code]*/

static Py_ssize_t
gc_collect_impl(PyObject *module, int generation)
/*[clinic end generated code: output=b697e633043233c7 input=40720128b682d879]*/
{
    PyThreadState *tstate = _PyThreadState_GET();

    if (generation < 0 || generation >= NUM_GENERATIONS)
    {
        _PyErr_SetString(tstate, PyExc_ValueError, "invalid generation");
        return -1;
    }

    GCState *gcstate = &tstate->interp->gc;
    Py_ssize_t n;
    if (gcstate->collecting)
    {
        /* already collecting, don't do anything */
        n = 0;
    }
    else
    {
        gcstate->collecting = 1;
        n = gc_collect_with_callback(tstate, generation);
        gcstate->collecting = 0;
    }
    return n;
}

/*[clinic input]
gc.set_debug

    flags: int
        An integer that can have the following bits turned on:
        DEBUG_STATS - Print statistics during collection.
        DEBUG_COLLECTABLE - Print collectable objects found.
        DEBUG_UNCOLLECTABLE - Print unreachable but uncollectable objects
            found.
        DEBUG_SAVEALL - Save objects to gc.garbage rather than freeing them.
        DEBUG_LEAK - Debug leaking programs (everything but STATS).
    /

Set the garbage collection debugging flags.

Debugging information is written to sys.stderr.
[clinic start generated code]*/

static PyObject *
gc_set_debug_impl(PyObject *module, int flags)
/*[clinic end generated code: output=7c8366575486b228 input=5e5ce15e84fbed15]*/
{
    GCState *gcstate = get_gc_state();
    gcstate->debug = flags;
    Py_RETURN_NONE;
}

/*[clinic input]
gc.get_debug -> int

Get the garbage collection debugging flags.
[clinic start generated code]*/

static int
gc_get_debug_impl(PyObject *module)
/*[clinic end generated code: output=91242f3506cd1e50 input=91a101e1c3b98366]*/
{
    GCState *gcstate = get_gc_state();
    return gcstate->debug;
}

PyDoc_STRVAR(gc_set_thresh__doc__,
             "set_threshold(threshold0, [threshold1, threshold2]) -> None\n"
             "\n"
             "Sets the collection thresholds.  Setting threshold0 to zero disables\n"
             "collection.\n");

static PyObject *
gc_set_threshold(PyObject *self, PyObject *args)
{
    GCState *gcstate = get_gc_state();
    if (!PyArg_ParseTuple(args, "i|ii:set_threshold",
                          &gcstate->generations[0].threshold,
                          &gcstate->generations[1].threshold,
                          &gcstate->generations[2].threshold))
        return NULL;
    for (int i = 3; i < NUM_GENERATIONS; i++)
    {
        /* generations higher than 2 get the same threshold */
        gcstate->generations[i].threshold = gcstate->generations[2].threshold;
    }
    Py_RETURN_NONE;
}

/*[clinic input]
gc.get_threshold

Return the current collection thresholds.
[clinic start generated code]*/

static PyObject *
gc_get_threshold_impl(PyObject *module)
/*[clinic end generated code: output=7902bc9f41ecbbd8 input=286d79918034d6e6]*/
{
    GCState *gcstate = get_gc_state();
    return Py_BuildValue("(iii)",
                         gcstate->generations[0].threshold,
                         gcstate->generations[1].threshold,
                         gcstate->generations[2].threshold);
}

/*[clinic input]
gc.get_count

Return a three-tuple of the current collection counts.
[clinic start generated code]*/

static PyObject *
gc_get_count_impl(PyObject *module)
/*[clinic end generated code: output=354012e67b16398f input=a392794a08251751]*/
{
    GCState *gcstate = get_gc_state();
    return Py_BuildValue("(iii)",
                         gcstate->generations[0].count,
                         gcstate->generations[1].count,
                         gcstate->generations[2].count);
}

static int
referrersvisit(PyObject *obj, PyObject *objs)
{
    Py_ssize_t i;
    for (i = 0; i < PyTuple_GET_SIZE(objs); i++)
        if (PyTuple_GET_ITEM(objs, i) == obj)
            return 1;
    return 0;
}

static int
gc_referrers_for(PyObject *objs, PyGC_Head *list, PyObject *resultlist)
{
    PyGC_Head *gc;
    PyObject *obj;
    traverseproc traverse;
    for (gc = GC_NEXT(list); gc != list; gc = GC_NEXT(gc))
    {
        obj = FROM_GC(gc);
        traverse = Py_TYPE(obj)->tp_traverse;
        if (obj == objs || obj == resultlist)
            continue;
        if (traverse(obj, (visitproc)referrersvisit, objs))
        {
            if (PyList_Append(resultlist, obj) < 0)
                return 0; /* error */
        }
    }
    return 1; /* no error */
}

PyDoc_STRVAR(gc_get_referrers__doc__,
             "get_referrers(*objs) -> list\n\
    Return the list of objects that directly refer to any of objs.");

static PyObject *
gc_get_referrers(PyObject *self, PyObject *args)
{
    if (PySys_Audit("gc.get_referrers", "(O)", args) < 0)
    {
        return NULL;
    }

    PyObject *result = PyList_New(0);
    if (!result)
    {
        return NULL;
    }

    GCState *gcstate = get_gc_state();
    for (int i = 0; i < NUM_GENERATIONS; i++)
    {
        if (!(gc_referrers_for(args, GEN_HEAD(gcstate, i), result)))
        {
            Py_DECREF(result);
            return NULL;
        }
    }
    return result;
}

/* Append obj to list; return true if error (out of memory), false if OK. */
static int
referentsvisit(PyObject *obj, PyObject *list)
{
    // PyObject_Print(obj, stderr, 1);
    // fprintf(stderr, "\t from inner\n");
    return PyList_Append(list, obj) < 0;
}

static int cascadingvisitor(PyObject *inner_op, unsigned int *combined)
{
    // if (check_in_set((uintptr_t)inner_op))
    // if (found_in_local_kset(inner_op))
    if (check_in_global((uintptr_t)inner_op))
    // if (found_in_kset_helper(inner_op))
    // if (check_in_libcuckoo((uintptr_t)inner_op))
    {
        return 0;
    }

    // *combined += 1; // increase length, stop while traversing
    // unsigned int extracted_len = *combined & 0xFFFF;
    // if (extracted_len > LEN_THRESHOLD)
    // {
    //     return; // stop
    // }
    // fprintf(stderr, "length: %u\n", extracted_len);

    // int ret;
    insert_into_global((uintptr_t)inner_op);
    // insert_into_libcuckoo((uintptr_t)inner_op);
    // kh_put(ptrset, global_op_set, (uintptr_t)inner_op, &ret);
    // insert_global_set_helper(inner_op);
    kv_push(PyObject *, local_ptr_vec, inner_op); // vector, dynamic resizing
    // inner_traversing(inner_op, combined); // for testing gc.get_referents()
    update_recursive_visitor(inner_op, combined); // for real
    return 0;
}
void inner_traversing(PyObject *each_op, unsigned int *combined)
{
    if (!each_op || !_PyObject_IS_GC(each_op))
    {
        return;
    }
    traverseproc traverse = Py_TYPE(each_op)->tp_traverse;
    if (!traverse)
        return;

    // unsigned int new_len = 0;
    // new_len |= (*combined << 16);

    unsigned int last_length = *combined & 0xFFFF;
    *combined &= 0xFFFF0000; // reset the lower 2 bytes (lengths), this is needed
    // fprintf(stderr, "last length preserved: %u\n", last_length);

    *combined += (1 << 16); // increase depths
    unsigned int extractedDepth = (*combined >> 16);
    fprintf(stderr, "depth: %u\n", extractedDepth);

    traverse(each_op, (visitproc)cascadingvisitor, combined);
    *combined = (*combined & 0xFFFF0000) | last_length;
    // set length to last_length
}

PyDoc_STRVAR(gc_get_referents__doc__,
             "get_referents(*objs) -> list\n\
    Return the list of objects that are directly referred to by objs.");

// static PyObject *
// gc_get_referents(PyObject *self, PyObject *args)
// {
//     Py_ssize_t i;
//     if (PySys_Audit("gc.get_referents", "(O)", args) < 0)
//     {
//         return NULL;
//     }
//     PyObject *result = PyList_New(0);

//     if (result == NULL)
//         return NULL;

//     for (i = 0; i < PyTuple_GET_SIZE(args); i++)
//     {
//         traverseproc traverse;
//         PyObject *obj = PyTuple_GET_ITEM(args, i);

//         if (!_PyObject_IS_GC(obj))
//             continue;
//         traverse = Py_TYPE(obj)->tp_traverse;
//         if (!traverse)
//             continue;
//         if (traverse(obj, referentsvisit, result))
//         {
//             Py_DECREF(result);
//             return NULL;
//         }
//     }
//     return result;
// }

static PyObject *
gc_get_referents(PyObject *self, PyObject *args)
{
    Py_ssize_t i;
    if (PySys_Audit("gc.get_referents", "(O)", args) < 0)
    {
        return NULL;
    }
    PyObject *result = PyList_New(0);
    heat_node *heat_node_head = NULL;

    if (result == NULL)
        return NULL;

    int ret;
    for (i = 0; i < PyTuple_GET_SIZE(args); i++)
    {
        PyObject *obj = PyTuple_GET_ITEM(args, i);
        // if (check_in_set((uintptr_t)obj))
        // {
        //     continue;
        // }
        // PyObject_Print(obj, stderr, 1);
        // fprintf(stderr, "\t from outter\n");
        // Temperature *dummy_temp = malloc(sizeof(Temperature)); // TODO: considering pre-malloc a bunch of dummy_temp at once and assign here, but how much?
        // heat_node *each_node = malloc(sizeof(heat_node));
        // each_node->temp = dummy_temp;
        // each_node->op = obj;
        // DL_APPEND(heat_node_head, each_node);
        // fprintf(stderr, "inserting outter...");
        // PyObject_Print(obj, stderr, 1);
        // fprintf(stderr, "\n");
        unsigned int combined = 0;
        inner_traversing(obj, &combined);
        // unsigned int new_depth = (*combined >> 16) & 0xffff;
        // fprintf(stderr, "cur_depth: %d\n", cur_depth);
    }
    // fprintf(stderr, "global live op size: %u\n", global_op_size);

    return result;
    // return NULL;
}

/*[clinic input]
gc.get_objects
    generation: Py_ssize_t(accept={int, NoneType}, c_default="-1") = None
        Generation to extract the objects from.

Return a list of objects tracked by the collector (excluding the list returned).

If generation is not None, return only the objects tracked by the collector
that are in that generation.
[clinic start generated code]*/

static PyObject *
gc_get_objects_impl(PyObject *module, Py_ssize_t generation)
/*[clinic end generated code: output=48b35fea4ba6cb0e input=ef7da9df9806754c]*/
{
    PyThreadState *tstate = _PyThreadState_GET();
    int i;
    PyObject *result;
    GCState *gcstate = &tstate->interp->gc;

    if (PySys_Audit("gc.get_objects", "n", generation) < 0)
    {
        return NULL;
    }

    result = PyList_New(0);
    if (result == NULL)
    {
        return NULL;
    }

    /* If generation is passed, we extract only that generation */
    if (generation != -1)
    {
        if (generation >= NUM_GENERATIONS)
        {
            _PyErr_Format(tstate, PyExc_ValueError,
                          "generation parameter must be less than the number of "
                          "available generations (%i)",
                          NUM_GENERATIONS);
            goto error;
        }

        if (generation < 0)
        {
            _PyErr_SetString(tstate, PyExc_ValueError,
                             "generation parameter cannot be negative");
            goto error;
        }

        if (append_objects(result, GEN_HEAD(gcstate, generation)))
        {
            goto error;
        }

        return result;
    }

    /* If generation is not passed or None, get all objects from all generations */
    for (i = 0; i < NUM_GENERATIONS; i++)
    {
        if (append_objects(result, GEN_HEAD(gcstate, i)))
        {
            goto error;
        }
    }
    return result;

error:
    Py_DECREF(result);
    return NULL;
}

/*[clinic input]
gc.get_stats

Return a list of dictionaries containing per-generation statistics.
[clinic start generated code]*/

static PyObject *
gc_get_stats_impl(PyObject *module)
/*[clinic end generated code: output=a8ab1d8a5d26f3ab input=1ef4ed9d17b1a470]*/
{
    int i;
    struct gc_generation_stats stats[NUM_GENERATIONS], *st;

    /* To get consistent values despite allocations while constructing
    the result list, we use a snapshot of the running stats. */
    GCState *gcstate = get_gc_state();
    for (i = 0; i < NUM_GENERATIONS; i++)
    {
        stats[i] = gcstate->generation_stats[i];
    }

    PyObject *result = PyList_New(0);
    if (result == NULL)
        return NULL;

    for (i = 0; i < NUM_GENERATIONS; i++)
    {
        PyObject *dict;
        st = &stats[i];
        dict = Py_BuildValue("{snsnsn}",
                             "collections", st->collections,
                             "collected", st->collected,
                             "uncollectable", st->uncollectable);
        if (dict == NULL)
            goto error;
        if (PyList_Append(result, dict))
        {
            Py_DECREF(dict);
            goto error;
        }
        Py_DECREF(dict);
    }
    return result;

error:
    Py_XDECREF(result);
    return NULL;
}

/*[clinic input]
gc.is_tracked

    obj: object
    /

Returns true if the object is tracked by the garbage collector.

Simple atomic objects will return false.
[clinic start generated code]*/

static PyObject *
gc_is_tracked(PyObject *module, PyObject *obj)
/*[clinic end generated code: output=14f0103423b28e31 input=d83057f170ea2723]*/
{
    PyObject *result;

    if (_PyObject_IS_GC(obj) && _PyObject_GC_IS_TRACKED(obj))
        result = Py_True;
    else
        result = Py_False;
    return Py_NewRef(result);
}

/*[clinic input]
gc.is_finalized

    obj: object
    /

Returns true if the object has been already finalized by the GC.
[clinic start generated code]*/

static PyObject *
gc_is_finalized(PyObject *module, PyObject *obj)
/*[clinic end generated code: output=e1516ac119a918ed input=201d0c58f69ae390]*/
{
    if (_PyObject_IS_GC(obj) && _PyGCHead_FINALIZED(AS_GC(obj)))
    {
        Py_RETURN_TRUE;
    }
    Py_RETURN_FALSE;
}

/*[clinic input]
gc.freeze

Freeze all current tracked objects and ignore them for future collections.

This can be used before a POSIX fork() call to make the gc copy-on-write friendly.
Note: collection before a POSIX fork() call may free pages for future allocation
which can cause copy-on-write.
[clinic start generated code]*/

static PyObject *
gc_freeze_impl(PyObject *module)
/*[clinic end generated code: output=502159d9cdc4c139 input=b602b16ac5febbe5]*/
{
    GCState *gcstate = get_gc_state();
    for (int i = 0; i < NUM_GENERATIONS; ++i)
    {
        gc_list_merge(GEN_HEAD(gcstate, i), &gcstate->permanent_generation.head);
        gcstate->generations[i].count = 0;
    }
    Py_RETURN_NONE;
}

/*[clinic input]
gc.unfreeze

Unfreeze all objects in the permanent generation.

Put all objects in the permanent generation back into oldest generation.
[clinic start generated code]*/

static PyObject *
gc_unfreeze_impl(PyObject *module)
/*[clinic end generated code: output=1c15f2043b25e169 input=2dd52b170f4cef6c]*/
{
    GCState *gcstate = get_gc_state();
    gc_list_merge(&gcstate->permanent_generation.head,
                  GEN_HEAD(gcstate, NUM_GENERATIONS - 1));
    Py_RETURN_NONE;
}

/*[clinic input]
gc.get_freeze_count -> Py_ssize_t

Return the number of objects in the permanent generation.
[clinic start generated code]*/

static Py_ssize_t
gc_get_freeze_count_impl(PyObject *module)
/*[clinic end generated code: output=61cbd9f43aa032e1 input=45ffbc65cfe2a6ed]*/
{
    GCState *gcstate = get_gc_state();
    return gc_list_size(&gcstate->permanent_generation.head);
}

PyDoc_STRVAR(gc__doc__,
             "This module provides access to the garbage collector for reference cycles.\n"
             "\n"
             "enable() -- Enable automatic garbage collection.\n"
             "disable() -- Disable automatic garbage collection.\n"
             "isenabled() -- Returns true if automatic collection is enabled.\n"
             "collect() -- Do a full collection right now.\n"
             "get_count() -- Return the current collection counts.\n"
             "get_stats() -- Return list of dictionaries containing per-generation stats.\n"
             "set_debug() -- Set debugging flags.\n"
             "get_debug() -- Get debugging flags.\n"
             "set_threshold() -- Set the collection thresholds.\n"
             "get_threshold() -- Return the current the collection thresholds.\n"
             "get_objects() -- Return a list of all objects tracked by the collector.\n"
             "is_tracked() -- Returns true if a given object is tracked.\n"
             "is_finalized() -- Returns true if a given object has been already finalized.\n"
             "get_referrers() -- Return the list of objects that refer to an object.\n"
             "get_referents() -- Return the list of objects that an object refers to.\n"
             "freeze() -- Freeze all tracked objects and ignore them for future collections.\n"
             "unfreeze() -- Unfreeze all objects in the permanent generation.\n"
             "get_freeze_count() -- Return the number of objects in the permanent generation.\n");

static PyMethodDef GcMethods[] = {
    GC_ENABLE_METHODDEF
        GC_DISABLE_METHODDEF
            GC_ISENABLED_METHODDEF
                GC_SET_DEBUG_METHODDEF
                    GC_GET_DEBUG_METHODDEF
                        GC_GET_COUNT_METHODDEF{"set_threshold", gc_set_threshold, METH_VARARGS, gc_set_thresh__doc__},
    GC_GET_THRESHOLD_METHODDEF
        GC_COLLECT_METHODDEF
            GC_GET_OBJECTS_METHODDEF
                GC_GET_STATS_METHODDEF
                    GC_IS_TRACKED_METHODDEF
                        GC_IS_FINALIZED_METHODDEF{"get_referrers", gc_get_referrers, METH_VARARGS,
                                                  gc_get_referrers__doc__},
    {"get_referents", gc_get_referents, METH_VARARGS,
     gc_get_referents__doc__},
    GC_FREEZE_METHODDEF
        GC_UNFREEZE_METHODDEF
            GC_GET_FREEZE_COUNT_METHODDEF{NULL, NULL} /* Sentinel */
};

static int
gcmodule_exec(PyObject *module)
{
    GCState *gcstate = get_gc_state();

    /* garbage and callbacks are initialized by _PyGC_Init() early in
     * interpreter lifecycle. */
    assert(gcstate->garbage != NULL);
    if (PyModule_AddObjectRef(module, "garbage", gcstate->garbage) < 0)
    {
        return -1;
    }
    assert(gcstate->callbacks != NULL);
    if (PyModule_AddObjectRef(module, "callbacks", gcstate->callbacks) < 0)
    {
        return -1;
    }

#define ADD_INT(NAME)                                     \
    if (PyModule_AddIntConstant(module, #NAME, NAME) < 0) \
    {                                                     \
        return -1;                                        \
    }
    ADD_INT(DEBUG_STATS);
    ADD_INT(DEBUG_COLLECTABLE);
    ADD_INT(DEBUG_UNCOLLECTABLE);
    ADD_INT(DEBUG_SAVEALL);
    ADD_INT(DEBUG_LEAK);
#undef ADD_INT
    return 0;
}

static PyModuleDef_Slot gcmodule_slots[] = {
    {Py_mod_exec, gcmodule_exec},
    {Py_mod_multiple_interpreters, Py_MOD_PER_INTERPRETER_GIL_SUPPORTED},
    {0, NULL}};

static struct PyModuleDef gcmodule = {
    PyModuleDef_HEAD_INIT,
    .m_name = "gc",
    .m_doc = gc__doc__,
    .m_size = 0, // per interpreter state, see: get_gc_state()
    .m_methods = GcMethods,
    .m_slots = gcmodule_slots};

PyMODINIT_FUNC
PyInit_gc(void)
{
    return PyModuleDef_Init(&gcmodule);
}

/* C API for controlling the state of the garbage collector */
int PyGC_Enable(void)
{
    GCState *gcstate = get_gc_state();
    int old_state = gcstate->enabled;
    gcstate->enabled = 1;
    return old_state;
}

int PyGC_Disable(void)
{
    GCState *gcstate = get_gc_state();
    int old_state = gcstate->enabled;
    gcstate->enabled = 0;
    return old_state;
}

int PyGC_IsEnabled(void)
{
    GCState *gcstate = get_gc_state();
    return gcstate->enabled;
}

/* Public API to invoke gc.collect() from C */
Py_ssize_t
PyGC_Collect(void)
{
    PyThreadState *tstate = _PyThreadState_GET();
    GCState *gcstate = &tstate->interp->gc;

    if (!gcstate->enabled)
    {
        return 0;
    }

    Py_ssize_t n;
    if (gcstate->collecting)
    {
        /* already collecting, don't do anything */
        n = 0;
    }
    else
    {
        gcstate->collecting = 1;
        PyObject *exc = _PyErr_GetRaisedException(tstate);
        n = gc_collect_with_callback(tstate, NUM_GENERATIONS - 1);
        _PyErr_SetRaisedException(tstate, exc);
        gcstate->collecting = 0;
    }

    return n;
}

Py_ssize_t
_PyGC_CollectNoFail(PyThreadState *tstate)
{
    /* Ideally, this function is only called on interpreter shutdown,
    and therefore not recursively.  Unfortunately, when there are daemon
    threads, a daemon thread can start a cyclic garbage collection
    during interpreter shutdown (and then never finish it).
    See http://bugs.python.org/issue8713#msg195178 for an example.
    */
    GCState *gcstate = &tstate->interp->gc;
    if (gcstate->collecting)
    {
        return 0;
    }

    Py_ssize_t n;
    gcstate->collecting = 1;
    n = gc_collect_main(tstate, NUM_GENERATIONS - 1, NULL, NULL, 1);
    gcstate->collecting = 0;
    return n;
}

void _PyGC_DumpShutdownStats(PyInterpreterState *interp)
{
    GCState *gcstate = &interp->gc;
    if (!(gcstate->debug & DEBUG_SAVEALL) && gcstate->garbage != NULL && PyList_GET_SIZE(gcstate->garbage) > 0)
    {
        const char *message;
        if (gcstate->debug & DEBUG_UNCOLLECTABLE)
            message = "gc: %zd uncollectable objects at "
                      "shutdown";
        else
            message = "gc: %zd uncollectable objects at "
                      "shutdown; use gc.set_debug(gc.DEBUG_UNCOLLECTABLE) to list them";
        /* PyErr_WarnFormat does too many things and we are at shutdown,
        the warnings module's dependencies (e.g. linecache) may be gone
        already. */
        if (PyErr_WarnExplicitFormat(PyExc_ResourceWarning, "gc", 0,
                                     "gc", NULL, message,
                                     PyList_GET_SIZE(gcstate->garbage)))
            PyErr_WriteUnraisable(NULL);
        if (gcstate->debug & DEBUG_UNCOLLECTABLE)
        {
            PyObject *repr = NULL, *bytes = NULL;
            repr = PyObject_Repr(gcstate->garbage);
            if (!repr || !(bytes = PyUnicode_EncodeFSDefault(repr)))
                PyErr_WriteUnraisable(gcstate->garbage);
            else
            {
                PySys_WriteStderr(
                    "      %s\n",
                    PyBytes_AS_STRING(bytes));
            }
            Py_XDECREF(repr);
            Py_XDECREF(bytes);
        }
    }
}

void _PyGC_Fini(PyInterpreterState *interp)
{
    GCState *gcstate = &interp->gc;
    Py_CLEAR(gcstate->garbage);
    Py_CLEAR(gcstate->callbacks);

    /* We expect that none of this interpreters objects are shared
    with other interpreters.
    See https://github.com/python/cpython/issues/90228. */
}

/* for debugging */
void _PyGC_Dump(PyGC_Head *g)
{
    _PyObject_Dump(FROM_GC(g));
}

#ifdef Py_DEBUG
static int
visit_validate(PyObject *op, void *parent_raw)
{
    PyObject *parent = _PyObject_CAST(parent_raw);
    if (_PyObject_IsFreed(op))
    {
        _PyObject_ASSERT_FAILED_MSG(parent,
                                    "PyObject_GC_Track() object is not valid");
    }
    return 0;
}
#endif

/* extension modules might be compiled with GC support so these
functions must always be available */

void PyObject_GC_Track(void *op_raw)
{
    PyObject *op = _PyObject_CAST(op_raw);
    if (_PyObject_GC_IS_TRACKED(op))
    {
        _PyObject_ASSERT_FAILED_MSG(op,
                                    "object already tracked "
                                    "by the garbage collector");
    }
    _PyObject_GC_TRACK(op);

#ifdef Py_DEBUG
    /* Check that the object is valid: validate objects traversed
    by tp_traverse() */
    traverseproc traverse = Py_TYPE(op)->tp_traverse;
    (void)traverse(op, visit_validate, op);
#endif
}

void PyObject_GC_UnTrack(void *op_raw)
{
    PyObject *op = _PyObject_CAST(op_raw);
    /* Obscure:  the Py_TRASHCAN mechanism requires that we be able to
     * call PyObject_GC_UnTrack twice on an object.
     */
    if (_PyObject_GC_IS_TRACKED(op))
    {
        _PyObject_GC_UNTRACK(op);
    }
}

int PyObject_IS_GC(PyObject *obj)
{
    return _PyObject_IS_GC(obj);
}

void _Py_ScheduleGC(PyInterpreterState *interp)
{
    GCState *gcstate = &interp->gc;
    if (gcstate->collecting == 1)
    {
        return;
    }
    struct _ceval_state *ceval = &interp->ceval;
    if (!_Py_atomic_load_relaxed(&ceval->gc_scheduled))
    {
        _Py_atomic_store_relaxed(&ceval->gc_scheduled, 1);
        _Py_atomic_store_relaxed(&ceval->eval_breaker, 1);
    }
}

void _PyObject_GC_Link(PyObject *op)
{
    PyGC_Head *g = AS_GC(op);
    assert(((uintptr_t)g & (sizeof(uintptr_t) - 1)) == 0); // g must be correctly aligned

    PyThreadState *tstate = _PyThreadState_GET();
    GCState *gcstate = &tstate->interp->gc;
    g->_gc_next = 0;
    g->_gc_prev = 0;
    gcstate->generations[0].count++; /* number of allocated GC objects */
    // fprintf(stderr, "GC trying to schedule\n"); // TODO, determine whether to schedule GC depends on number of recently scheduled
    // if (enable_bk)
    global_try2_sched++;
    if (gcstate->generations[0].count > gcstate->generations[0].threshold &&
        gcstate->enabled &&
        gcstate->generations[0].threshold &&
        !gcstate->collecting &&
        !_PyErr_Occurred(tstate))
    {
        _Py_ScheduleGC(tstate->interp);
    }
}

void _Py_RunGC(PyThreadState *tstate)
{
    GCState *gcstate = &tstate->interp->gc;
    gcstate->collecting = 1;
    gc_collect_generations(tstate);
    gcstate->collecting = 0;
}

static PyObject *
gc_alloc(size_t basicsize, size_t presize)
{
    PyThreadState *tstate = _PyThreadState_GET();
    if (basicsize > PY_SSIZE_T_MAX - presize)
    {
        return _PyErr_NoMemory(tstate);
    }
    size_t size = presize + basicsize;
    char *mem = PyObject_Malloc(size);
    if (mem == NULL)
    {
        return _PyErr_NoMemory(tstate);
    }
    ((PyObject **)mem)[0] = NULL;
    ((PyObject **)mem)[1] = NULL;
    PyObject *op = (PyObject *)(mem + presize);
    _PyObject_GC_Link(op);
    return op;
}

PyObject *
_PyObject_GC_New(PyTypeObject *tp)
{
    size_t presize = _PyType_PreHeaderSize(tp);
    PyObject *op = gc_alloc(_PyObject_SIZE(tp), presize);
    if (op == NULL)
    {
        return NULL;
    }
    _PyObject_Init(op, tp);
    return op;
}

PyVarObject *
_PyObject_GC_NewVar(PyTypeObject *tp, Py_ssize_t nitems)
{
    PyVarObject *op;

    if (nitems < 0)
    {
        PyErr_BadInternalCall();
        return NULL;
    }
    size_t presize = _PyType_PreHeaderSize(tp);
    size_t size = _PyObject_VAR_SIZE(tp, nitems);
    op = (PyVarObject *)gc_alloc(size, presize);
    if (op == NULL)
    {
        return NULL;
    }
    _PyObject_InitVar(op, tp, nitems);
    return op;
}

PyObject *
PyUnstable_Object_GC_NewWithExtraData(PyTypeObject *tp, size_t extra_size)
{
    size_t presize = _PyType_PreHeaderSize(tp);
    PyObject *op = gc_alloc(_PyObject_SIZE(tp) + extra_size, presize);
    if (op == NULL)
    {
        return NULL;
    }
    memset(op, 0, _PyObject_SIZE(tp) + extra_size);
    _PyObject_Init(op, tp);
    return op;
}

PyVarObject *
_PyObject_GC_Resize(PyVarObject *op, Py_ssize_t nitems)
{
    const size_t basicsize = _PyObject_VAR_SIZE(Py_TYPE(op), nitems);
    const size_t presize = _PyType_PreHeaderSize(((PyObject *)op)->ob_type);
    _PyObject_ASSERT((PyObject *)op, !_PyObject_GC_IS_TRACKED(op));
    if (basicsize > (size_t)PY_SSIZE_T_MAX - presize)
    {
        return (PyVarObject *)PyErr_NoMemory();
    }
    char *mem = (char *)op - presize;
    mem = (char *)PyObject_Realloc(mem, presize + basicsize);
    if (mem == NULL)
    {
        return (PyVarObject *)PyErr_NoMemory();
    }
    op = (PyVarObject *)(mem + presize);
    Py_SET_SIZE(op, nitems);
    return op;
}

void PyObject_GC_Del(void *op)
{
    size_t presize = _PyType_PreHeaderSize(((PyObject *)op)->ob_type);
    PyGC_Head *g = AS_GC(op);
    if (_PyObject_GC_IS_TRACKED(op))
    {
#ifdef Py_DEBUG
        if (PyErr_WarnExplicitFormat(PyExc_ResourceWarning, "gc", 0,
                                     "gc", NULL, "Object of type %s is not untracked before destruction",
                                     ((PyObject *)op)->ob_type->tp_name))
        {
            PyErr_WriteUnraisable(NULL);
        }
#endif
        gc_list_remove(g);
    }
    GCState *gcstate = get_gc_state();
    if (gcstate->generations[0].count > 0)
    {
        gcstate->generations[0].count--;
    }
    PyObject_Free(((char *)op) - presize);
}

int PyObject_GC_IsTracked(PyObject *obj)
{
    if (_PyObject_IS_GC(obj) && _PyObject_GC_IS_TRACKED(obj))
    {
        return 1;
    }
    return 0;
}

int PyObject_GC_IsFinalized(PyObject *obj)
{
    if (_PyObject_IS_GC(obj) && _PyGCHead_FINALIZED(AS_GC(obj)))
    {
        return 1;
    }
    return 0;
}

void PyUnstable_GC_VisitObjects(gcvisitobjects_t callback, void *arg)
{
    size_t i;
    GCState *gcstate = get_gc_state();
    int origenstate = gcstate->enabled;
    gcstate->enabled = 0;
    for (i = 0; i < NUM_GENERATIONS; i++)
    {
        PyGC_Head *gc_list, *gc;
        gc_list = GEN_HEAD(gcstate, i);
        for (gc = GC_NEXT(gc_list); gc != gc_list; gc = GC_NEXT(gc))
        {
            PyObject *op = FROM_GC(gc);
            Py_INCREF(op);
            int res = callback(op, arg);
            Py_DECREF(op);
            if (!res)
            {
                goto done;
            }
        }
    }
done:
    gcstate->enabled = origenstate;
}
// void update_recursive(PyObject *each_op, op_gc_table *table)
void update_recursive(PyObject *each_op, cur_heats_table *table)
{
    if (_Py_IsImmortal(each_op))
    {
        // PyObject_Print(each_op, stderr, 1);
        // fprintf(stderr, " is immortal\n");
        return;
    }
    // if (curr_depth >= 100)
    //     return;
    // if (each_op->ob_refcnt == 1)
    // {
    //     return;
    // }
    // else
    if (each_op->ob_refcnt != 1)
    { // filter out those objs whose refcnt was increased just because calling PyIter_Next()
        Temperature dummy_temp = {
            .prev_refcnt = 0, // Initialize prev_refcnt
            .diffs = {0},     // Initialize all elements of diffs to 0
            .cur_sizeof = 0   // Example: Initialize cur_sizeof to the size of the Temperature struct
        };
        uintptr_t each_op_casted = (uintptr_t)each_op;
        cur_heats_table_insert(table, &each_op_casted, &dummy_temp);
    }
    PyObject *iterable = PyObject_GetIter(each_op);
    if (!iterable)
    {
        // PyErr_Clear();
        return;
    }
    PyObject *inner_op;
    while ((inner_op = PyIter_Next(iterable)))
    {
        // // uint8_t dummy_val;
        // // Temperature dummy_temp_;
        // // if (op_gc_table_find(table, &inner_op_casted, &dummy_val))
        uintptr_t inner_op_casted = (uintptr_t)inner_op;
        if (cur_heats_table_contains(table, &inner_op_casted))
        {
            continue;
        }
        // inner_op->hotness = 0;
        update_recursive(inner_op, table);
        Py_DECREF(inner_op);
    }
    // another way of traversing, works fine
    //  Py_ssize_t cur_size = PyList_Size(each_op);
    //  for(Py_ssize_t i = 0; i < cur_size; i++){
    //      inner_op = PyList_GetItem(each_op, i);
    //      uintptr_t inner_op_casted = (uintptr_t)inner_op;
    //      uint8_t dummy_val;
    //      if(op_gc_table_find(table, &inner_op_casted, &dummy_val)){
    //          // fprintf(stderr, "found dupped %p\n", inner_op);
    //          continue;
    //      }
    //      update_recursive(inner_op, table);
    //  }
    // if (PyErr_Occurred()) {
    //     PyErr_Print();
    // }
    Py_DECREF(iterable);
    return;
}

// returns true if error
// static void visit_func(PyObject *inner_op, heat_node **heat_node_head)
// {
//     heat_node *each_node = (heat_node *)malloc(sizeof(heat_node));
//     Temperature *dummy_temp = malloc(sizeof(Temperature));
//     each_node->temp = dummy_temp;
//     each_node->op = inner_op;
//     DL_APPEND(*heat_node_head, each_node);
//     return 0;
// }

static bool ismapped(const void *ptr, int bytes)
{
    if (ptr == NULL)
    {
        return 0;
    }
    int fd[2];
    int valid = 1;
    pipe(fd);
    if (write(fd[1], ptr, bytes) < 0)
    { // try to write it, if getting outside, SEGFAULT
        if (errno == EFAULT)
        {
            valid = 0;
        }
    }
    close(fd[0]);
    close(fd[1]);
    return valid;
}

// void update_recursive_visitor(PyObject *each_op, heat_node **heat_node_head)
void update_recursive_visitor(PyObject *each_op, unsigned int *combined)
{
    if (!each_op)
    {
        return;
    }
    // if (PyList_Check(each_op))
    // {
    //     Py_ssize_t list_size = PyList_Size(each_op);
    //     for (Py_ssize_t i = 0; i < list_size; i++)
    //     {
    //         PyObject *item = PyList_GetItem(each_op, i);
    //         if (check_ipaddr_type(item))
    //         {
    //             fprintf(stderr, "skipping inner ipaddr\n");
    //             return;
    //         }
    //         break;
    //     }
    // }
    if (_Py_IsImmortal(each_op))
    {
        return;
    }

    PyTypeObject *t = Py_TYPE(each_op);
    if (!t->tp_iter || !t->tp_repr)
        return;
    if (!_PyObject_IS_GC(each_op))
        return;
    traverseproc traverse;
    traverse = Py_TYPE(each_op)->tp_traverse;
    if (!traverse)
        return;
    // {                                             // skip before traversing
    //     unsigned long cur_len = Py_SIZE(each_op); // || cur_len > 10000000
    //     if (cur_len > LEN_THRESHOLD)
    //         return;
    // }

    unsigned int last_length = *combined & 0xFFFF;
    *combined &= 0xFFFF0000; // reset the lower 2 bytes (lengths), this is needed
    *combined += (1 << 16);  // increase depths
    // {                        // skip large depth objs
    //     unsigned int extractedDepth = (*combined >> 16);
    //     if (extractedDepth > DEPTH_THRESHOLD)
    //     {
    //         return;
    //     }
    // }
    traverse(each_op, (visitproc)cascadingvisitor, combined);
    *combined = (*combined & 0xFFFF0000) | last_length;
}

static int append_objects_op_gc(PyGC_Head *gc_list, op_gc_table *table)
// static int append_objects_op_gc(PyGC_Head *gc_list, cur_heats_table *table)
{
    PyGC_Head *gc;
    int cur_gen_size = 0;
    uint8_t dummy_val;
    for (gc = GC_NEXT(gc_list); gc != gc_list; gc = GC_NEXT(gc))
    {
        cur_gen_size++;
        PyObject *container_op = FROM_GC(gc);
        uintptr_t container_op_casted = (uintptr_t)container_op;
        op_gc_table_insert(table, &container_op_casted, &dummy_val);
        // update_recursive(container_op, table); // now do it after getting 1st level container objs
    }
    fprintf(stderr, "cur GC generation size: %d\n", cur_gen_size);
    return 0;
}

void gc_get_objects_impl_op_gc(Py_ssize_t generation, op_gc_table *table)
// void gc_get_objects_impl_op_gc(Py_ssize_t generation, cur_heats_table *table)
{
    PyThreadState *tstate = _PyThreadState_GET();
    int i;
    GCState *gcstate = &tstate->interp->gc;

    /* If generation is passed, we extract only that generation */
    if (generation != -1)
    {
        // if (generation >= NUM_GENERATIONS)
        // {
        //     _PyErr_Format(tstate, PyExc_ValueError,
        //                   "generation parameter must be less than the number of "
        //                   "available generations (%i)",
        //                   NUM_GENERATIONS);
        //     goto error;
        // }
        if (generation < 0)
        {
            _PyErr_SetString(tstate, PyExc_ValueError,
                             "generation parameter cannot be negative");
            goto error;
        }
        if (generation == 0 || generation == 1 || generation == 2)
        {
            fprintf(stderr, "inspecting generation %zu\n", generation);
            if (append_objects_op_gc(GEN_HEAD(gcstate, generation), table))
            {
                goto error;
            }
            return;
        }
        else if (generation == 3) // gen 0 + 1
        {
            if (append_objects_op_gc(GEN_HEAD(gcstate, 0), table))
            {
                goto error;
            }
            if (append_objects_op_gc(GEN_HEAD(gcstate, 1), table))
            {
                goto error;
            }
            return;
        }
        else if (generation == 4) // gen 1 + 2
        {
            if (append_objects_op_gc(GEN_HEAD(gcstate, 1), table))
            {
                goto error;
            }
            if (append_objects_op_gc(GEN_HEAD(gcstate, 2), table))
            {
                goto error;
            }
            return;
        }
        else
        {
            fprintf(stderr, "invalid generation!\n");
            goto error;
        }
    }
    /* If generation is not passed or None, get all objects from all generations */
    for (i = 0; i < NUM_GENERATIONS; i++)
    {
        fprintf(stderr, "inspecting generation %d\n", i);
        if (append_objects_op_gc(GEN_HEAD(gcstate, i), table))
        {
            goto error;
        }
    }
    return;

error:
    return;
}
unsigned long max_num_hot = 0; // accumulated hot objs for 10 fast cycles
double cur_hot_in_all = 0.0;   // current ratio of changed op within old_num_op
double last_hot_in_all = 0.0;  // last ratio of changed op within old_num_op
unsigned int zero_hot_num = 0;

static sigjmp_buf jump_buffer;

void sigsegv_handler(int sig)
{
    // write(STDERR_FILENO, "Caught segmentation fault, jumping to recovery point...\n", 56);
    siglongjmp(jump_buffer, 1);
}

void enable_sigsegv_handler()
{
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_flags = SA_SIGINFO;
    sa.sa_handler = sigsegv_handler;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGSEGV, &sa, NULL);
}
int check_in_global_helper(uintptr_t op)
{
    if (need2_check_set)
    {
        return check_in_global(op);
        // return found_in_kset_helper((PyObject *)op);
        // return check_in_libcuckoo(op);
    }
    else
    {
        return 1;
    }
}

void record_temp(int scan_idx, int rescan_thresh, unsigned int num_total)
{
    // return;
    // PyGILState_STATE gstate = PyGILState_Ensure();
    unsigned int skipped = 0;
    int prev_scan_idx = (scan_idx == 0) ? (rescan_thresh - 1) : (scan_idx - 1);
    for (int i = 0; i < num_total; i++)
    {
        // if (all_temps[i].diffs[rescan_thresh] & (1 << DROP_OUT_OFF)) // falls out of sampling range, skip
        // {
        //     skipped++;
        // }
        // else
        // if (sigsetjmp(jump_buffer, 1) == 0)
        // {
        if (!check_in_global_helper((uintptr_t)all_temps[i].op))
        {
            not_in_global_set++;
            continue;
        }
        all_temps[i].diffs[scan_idx] = (abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt) > SHRT_MAX) ? -1 : (short)abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt);
        // all_temps[i].diffs[scan_idx] = (short)abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt);
        all_temps[i].prev_refcnt = all_temps[i].op->hotness;
        // mapping model: populate hotness in diffs[rescan_thresh]
        if (all_temps[i].diffs[scan_idx] != 0)
        {
            all_temps[i].diffs[rescan_thresh] += 1;
            cur_fast_num_hot++;
        }
        else
        {
            zero_hot_num++;
        }
        if (all_temps[i].diffs[scan_idx] != all_temps[i].diffs[prev_scan_idx] && all_temps[i].diffs[prev_scan_idx] != 0 && all_temps[i].diffs[scan_idx] != 0) // TODO: for better cache hit, we can check diffs[scan_idx+1]
        {
            all_temps[i].diffs[rescan_thresh] += 1;
        }
        // }
    }
    fprintf(stderr, "skipped %u objs\n", skipped);
    // PyGILState_Release(gstate);
}

void update_prev_refcnt_slow_trace(cur_heats_table_locked_table *curHeats_locked, cur_heats_table *curHeats)
{
    cur_heats_table_iterator *curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
    cur_heats_table_iterator *curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
    cur_heats_table_locked_table_unlock(curHeats_locked);
    uintptr_t each_op;
    // Temperature temp = {0, 0, 0};
    Temperature temp = {
        .prev_refcnt = 0, // Initialize prev_refcnt
        .diffs = {0},     // Initialize all elements of diffs to 0
        .cur_sizeof = 0   // Example: Initialize cur_sizeof to the size of the Temperature struct
    };
    for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_iterator_increment(curHeats_it))
    {
        each_op = *cur_heats_table_iterator_key(curHeats_it); // each_op is uintptr_t type
        PyObject *op = (PyObject *)each_op;
        // Temperature temp = *cur_heats_table_iterator_mapped(curHeats_it);
        temp.prev_refcnt = op->ob_refcnt;
        cur_heats_table_update(curHeats, &each_op, &temp);
        // op->prev_refcnt = op->ob_refcnt;
    }
    cur_heats_table_iterator_free(curHeats_end);
    cur_heats_table_iterator_free(curHeats_it);
}

void inspect_objects(op_gc_table *cur_op_gc_table)
{
    fprintf(stderr, "inspecting objects...\n");
    op_gc_table_locked_table *locked_table = op_gc_table_lock_table(cur_op_gc_table);
    op_gc_table_iterator *locked_it = op_gc_table_locked_table_begin(locked_table);
    op_gc_table_iterator *locked_end = op_gc_table_locked_table_end(locked_table);
    op_gc_table_locked_table_unlock(locked_table);
    uintptr_t each_op;
    for (; !op_gc_table_iterator_equal(locked_it, locked_end); op_gc_table_iterator_increment(locked_it))
    {
        each_op = *op_gc_table_iterator_key(locked_it);
        PyObject *op = (PyObject *)each_op;
        PyObject_Print(op, stderr, 1);
        fprintf(stderr, "\n");
    }
    op_gc_table_iterator_free(locked_it);
    op_gc_table_iterator_free(locked_end);
    op_gc_table_locked_table_free(locked_table);
}

// with op_gc version
// void *thread_trace_from_gc_list(void *arg)
// {
//     /* TODO: later on needs to consider different generations tracking frequency.
//         Larger # objs trace more frequently?? ie correlation btw obj lifetime VS obj hotness */
//     fprintf(stderr, "start bookkeep thread\n");
//     if (bookkeep_args->fd == NULL)
//     {
//         perror("Failed to find fd passed in\n");
//     }
//     // if (bookkeep_args->sample_dur == 0)
//     // {
//     //     fprintf(stderr, "missing sample_dur, setting to 1ms\n");
//     //     bookkeep_args->sample_dur = 1000;
//     // }
//     PyGILState_STATE gstate;
//     all_heats_table *allHeats = all_heats_table_init(0);
//     all_heats_table_locked_table *allHeats_locked = all_heats_table_lock_table(allHeats);
//     ts_blob outter_key_wrapper;
//     cur_heats_table *curHeats;
//     cur_heats_table_locked_table *curHeats_locked;
//     unsigned int doIO_ = bookkeep_args->doIO;
//     // RefTrackHeatmapHash *outter_item, *tmp_outter;
//     // CurTimeObjHeat *inner_item, *tmp_inner;
//     int rescan_thresh = 6; // threshold to say after how many fast scan, that we do a complete scan, not const, probably need to dynamically change later
//     int cur_scan_idx = 0;  // indicates how many fast scan bk thread has done, resets to zero when reaching rescan_thresh
//     cur_heats_table *last_few_scans_arr[rescan_thresh];
//     cur_heats_table_locked_table *last_few_scans_arr_locked[rescan_thresh];
//     struct timespec ts;
//     clock_t update_prev_refcnt_start, update_prev_refcnt_end, insert_record_start, insert_record_end, IO_start, IO_end;
//     double update_prev_refcnt_time, total_hold_GIL_time, insert_record_time, whole_IO_time;
//     // unsigned int longer_sleep = 2 * bookkeep_args->sample_dur;
//     int fast_scan_to_drop = bookkeep_args->fast_scan_to_drop;
//     // fprintf(stderr, "fast_scan_to_drop is %d\n", fast_scan_to_drop);
//     if (fast_scan_to_drop == 0)
//     {
//         perror("fast_scan_to_drop cannot be 0\n");
//         exit(-1);
//     }
//     else if (fast_scan_to_drop >= rescan_thresh) // can only be 1, 2, 3, 4
//     {
//         perror("fast_scan_to_drop cannot be larger than rescan_thresh\n");
//         exit(-1);
//     }
//     op_gc_table *cur_op_gc_table;
//     op_gc_table_locked_table *cur_op_gc_locked_table;
//     uintptr_t prev_changed_max;
//     uintptr_t prev_changed_min = ULONG_MAX;
//     uintptr_t no_93_upper = 100000000000000;
//     while (!terminate_flag)
//     {
//         update_prev_refcnt_start = clock();
//         curHeats = cur_heats_table_init(0);
//         /* slow trace: populate curHeats, from scratch (cascade GC_list)
//             fast trace: populate cur_op_gc_table, from previous curHeats*/
//         if (cur_scan_idx == 0)
//         { // slow trace, bookkeep_args->gen: [-1, 0, 1, 2], where -1 means all generations
//             cur_op_gc_table = op_gc_table_init(0);
//             fprintf(stderr, "slow peeking...\n");
//             gstate = PyGILState_Ensure();
//             gc_get_objects_impl_op_gc(bookkeep_args->gen, cur_op_gc_table); // previous version
//             // gc_get_objects_impl_op_gc(bookkeep_args->gen, curHeats);

//             // cascade GC trace
//             cur_op_gc_locked_table = op_gc_table_lock_table(cur_op_gc_table); // for iterating, remember to free this
//             uintptr_t each_op;
//             op_gc_table_iterator *op_gc_it = op_gc_table_locked_table_begin(cur_op_gc_locked_table);
//             op_gc_table_iterator *op_gc_end = op_gc_table_locked_table_end(cur_op_gc_locked_table);
//             for (; !op_gc_table_iterator_equal(op_gc_it, op_gc_end); op_gc_table_iterator_increment(op_gc_it))
//             {
//                 each_op = *op_gc_table_iterator_key(op_gc_it);
//                 PyObject *container_op = (PyObject *)each_op;
//                 update_recursive(container_op, curHeats);
//             }
//             PyGILState_Release(gstate);
//             op_gc_table_iterator_free(op_gc_end);
//             op_gc_table_iterator_free(op_gc_it);
//             op_gc_table_locked_table_free(cur_op_gc_locked_table);
//             op_gc_table_free(cur_op_gc_table);

//             // now, update prev_refcnt for cascade traced objs
//             curHeats_locked = cur_heats_table_lock_table(curHeats);
//             update_prev_refcnt_slow_trace(curHeats_locked);
//         }
//         else
//         { // fast
//             cur_op_gc_table = op_gc_table_init(0);
//             cur_heats_table_const_iterator *curHeats_it, *curHeats_end;
//             fprintf(stderr, "fast peeking...\n");
//             uintptr_t foundInner;
//             uint8_t dummy_value = 0;
//             curHeats_it = cur_heats_table_locked_table_begin(last_few_scans_arr_locked[cur_scan_idx - 1]); // iterate the previous curHeats
//             curHeats_end = cur_heats_table_locked_table_end(last_few_scans_arr_locked[cur_scan_idx - 1]);
//             if (cur_scan_idx == 1) // update all traced objs' prev_refcnt, from casecad trace
//             {
//                 for (; !cur_heats_table_const_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_const_iterator_increment(curHeats_it))
//                 {
//                     foundInner = *cur_heats_table_const_iterator_key(curHeats_it);
//                     PyObject *each_op = (PyObject *)foundInner;
//                     each_op->prev_refcnt = each_op->ob_refcnt;
//                     op_gc_table_insert(cur_op_gc_table, &foundInner, &dummy_value);
//                 }
//             }
//             else
//             { // only capture those within the diff range
//                 for (; !cur_heats_table_const_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_const_iterator_increment(curHeats_it))
//                 {
//                     foundInner = *cur_heats_table_const_iterator_key(curHeats_it);
//                     /* Reason being outside: no matter where the objs lies in,
//                         we update the prev_refcnt for accuracy,
//                         but need to check performance difference */
//                     if (foundInner > prev_changed_min && foundInner < prev_changed_max)
//                     {
//                         PyObject *each_op = (PyObject *)foundInner;
//                         each_op->prev_refcnt = each_op->ob_refcnt; // TODO: need to put this line inside?
//                         op_gc_table_insert(cur_op_gc_table, &foundInner, &dummy_value);
//                     }
//                 }
//             }
//             cur_heats_table_const_iterator_free(curHeats_end);
//             cur_heats_table_const_iterator_free(curHeats_it); // this only free the iterator, not the table, table will be freed later in batches
//         }
//         // PyGILState_Release(gstate);
//         update_prev_refcnt_end = clock();
//         update_prev_refcnt_time = (double)(update_prev_refcnt_end - update_prev_refcnt_start) / CLOCKS_PER_SEC;
//         fprintf(stderr, "trace %d update prev_cnt time: %.3f second\n", cur_scan_idx, update_prev_refcnt_time);
//         if (cur_scan_idx == 0)
//         {
//             total_hold_GIL_time += update_prev_refcnt_time;
//         }
//         clock_gettime(CLOCK_MONOTONIC, &ts);
//         outter_key_wrapper.ts = ts;
//         outter_key_wrapper.scan_idx = cur_scan_idx;

//         // Py_BEGIN_ALLOW_THREADS
//         if (cur_scan_idx == 0)
//         {
//             // unsigned int time_to_sleep;
//             // if((update_prev_refcnt_end - update_prev_refcnt_start) / CLOCKS_PER_SEC > bookkeep_args->sample_dur)
//             // {// don't sleep, directly go to next bk loop
//             //     dummy_copy();
//             //     fprintf(stderr, "slow traversing too slow, no need to check diff\n");
//             // }
//             // goto slow_trace_dump;
//             // usleep(bookkeep_args->sample_dur);
//         }
//         else
//         {
//             usleep(bookkeep_args->sample_dur);
//         }
//         // Py_END_ALLOW_THREADS

//         if (cur_scan_idx != 0)
//         { // fast trace
//             insert_record_start = clock();
//             Temperature temp;
//             size_t foundSizeof; // can be here?? make sure populated correctly, but shoudn't matter anyway

//             cur_op_gc_locked_table = op_gc_table_lock_table(cur_op_gc_table); // for iterating, remember to free this
//             // fprintf(stderr, "all size is: %zu\n", op_gc_table_locked_table_size(cur_op_gc_locked_table));
//             op_gc_table_iterator *op_gc_it = op_gc_table_locked_table_begin(cur_op_gc_locked_table);
//             op_gc_table_iterator *op_gc_end = op_gc_table_locked_table_end(cur_op_gc_locked_table);
//             uintptr_t each_op;
//             if (cur_scan_idx == 1)
//             {
//                 for (; !op_gc_table_iterator_equal(op_gc_it, op_gc_end); op_gc_table_iterator_increment(op_gc_it))
//                 {
//                     each_op = *op_gc_table_iterator_key(op_gc_it); // each_op is uintptr_t type
//                     PyObject *op = (PyObject *)each_op;
//                     temp.cur_sizeof = 0;
//                     temp.diff = (long)(op->ob_refcnt - op->prev_refcnt);
//                     // udpate min & max, for the rest fast cycles
//                     if (temp.diff != 0)
//                     {
//                         if (each_op > prev_changed_max)
//                         {
//                             prev_changed_max = each_op;
//                         }
//                         else if (each_op < prev_changed_min && each_op > no_93_upper)
//                         {
//                             prev_changed_min = each_op;
//                         }
//                     }
//                     cur_heats_table_insert(curHeats, &each_op, &temp); // attention: this line must be outside!
//                 }
//             }
//             else
//             {
//                 for (; !op_gc_table_iterator_equal(op_gc_it, op_gc_end); op_gc_table_iterator_increment(op_gc_it))
//                 {
//                     each_op = *op_gc_table_iterator_key(op_gc_it); // each_op is uintptr_t type
//                     PyObject *op = (PyObject *)each_op;
//                     temp.diff = (long)(op->ob_refcnt - op->prev_refcnt);
//                     temp.cur_sizeof = 0;
//                     cur_heats_table_insert(curHeats, &each_op, &temp);
//                     // if(temp.diff != 0) { // uncomment for debugging
//                     //     gstate = PyGILState_Ensure();
//                     //     fprintf(stderr, "%ld\n", each_op);
//                     //     // PyObject_Print(op, stderr, 1);
//                     //     // fprintf(stderr, "\n");
//                     //     PyGILState_Release(gstate);
//                     // }
//                 }
//             }
//             insert_record_end = clock();
//             insert_record_time = (double)(insert_record_end - insert_record_start) / CLOCKS_PER_SEC;
//             fprintf(stderr, "insert record time: %.3f seconds\n", insert_record_time);
//             op_gc_table_iterator_free(op_gc_end);
//             op_gc_table_iterator_free(op_gc_it);
//             op_gc_table_locked_table_free(cur_op_gc_locked_table);
//             op_gc_table_free(cur_op_gc_table);
//             curHeats_locked = cur_heats_table_lock_table(curHeats);
//         }

//         fprintf(stderr, "cur round: %d, # all: %d, # changed: ~~\n", cur_scan_idx, cur_heats_table_locked_table_size(curHeats_locked)); // ? what's the value
//         if (cur_scan_idx != 0)
//         {
//             uintptr_t curHeats_locked_casted = (uintptr_t)curHeats_locked;
//             all_heats_table_locked_table_insert(allHeats_locked, &outter_key_wrapper, &curHeats_locked_casted, NULL);
//         }
//         last_few_scans_arr[cur_scan_idx] = curHeats;
//         last_few_scans_arr_locked[cur_scan_idx] = curHeats_locked;
//         // if (doIO_ && cur_scan_idx != 0)
//         // {
//         //     clock_t start_IO, end_IO;
//         //     double IO_time;
//         //     cur_heats_table_iterator *curHeats_it, *curHeats_end;
//         //     start_IO = clock();
//         //     uintptr_t foundInner;
//         //     fprintf(stderr, "doing IO...\n");
//         //     curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
//         //     curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
//         //     for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
//         //          cur_heats_table_iterator_increment(curHeats_it))
//         //     {
//         //         foundInner = *cur_heats_table_iterator_key(curHeats_it);
//         //         Temperature temp = *cur_heats_table_iterator_mapped(curHeats_it); // later we need to add outter for loop to loop allHeats, so that the time stamp can get from there, but shouldn't matter anyway??
//         //         fprintf(bookkeep_args->fd, "%ld.%ld\t%ld\t%ld\t%u\t%d\n",
//         //                 outter_key_wrapper.ts.tv_sec, outter_key_wrapper.ts.tv_nsec,
//         //                 foundInner, temp.diff, temp.cur_sizeof, outter_key_wrapper.scan_idx);
//         //     }
//         //     cur_heats_table_iterator_free(curHeats_end);
//         //     cur_heats_table_iterator_free(curHeats_it);
//         //     end_IO = clock();
//         //     IO_time = (double)(end_IO - start_IO) / CLOCKS_PER_SEC;
//         //     fprintf(stderr, "IO time: %.3f seconds\n", IO_time);
//         // }
//         if (++cur_scan_idx == rescan_thresh) // we first increment, then compare. Thus when cur_scan_idx == 4, we reset the idx to 0 before next loop
//         {
//             cur_heats_table_locked_table_free(last_few_scans_arr_locked[0]); // if we do IO at last, here we only need to free slow scan curHheats
//             cur_heats_table_free(last_few_scans_arr[0]);
//             for (int i = 0; i < cur_scan_idx; i++)
//             {
//                 // cur_heats_table_locked_table_free(last_few_scans_arr_locked[i]);
//                 // cur_heats_table_free(last_few_scans_arr[i]);
//                 last_few_scans_arr[i] = NULL;
//                 last_few_scans_arr_locked[i] = NULL;
//             }
//             cur_scan_idx = 0;
//         }
//     }
//     // after tracing, dump traces
//     if (doIO_)
//     {
//         fprintf(stderr, "doing IO... this should appear together with 111...\n");
//         fflush(stderr);
//         all_heats_table_iterator *allHeats_it, *allHeats_end;
//         cur_heats_table_iterator *curHeats_it, *curHeats_end;
//         IO_start = clock();
//         uintptr_t found_each_cur_heats;
//         uintptr_t foundInner;
//         allHeats_it = all_heats_table_locked_table_begin(allHeats_locked);
//         allHeats_end = all_heats_table_locked_table_end(allHeats_locked);
//         for (; !all_heats_table_iterator_equal(allHeats_it, allHeats_end); all_heats_table_iterator_increment(allHeats_it))
//         {
//             outter_key_wrapper = *all_heats_table_iterator_key(allHeats_it);
//             found_each_cur_heats = *all_heats_table_iterator_mapped(allHeats_it);
//             curHeats_locked = (cur_heats_table_locked_table *)found_each_cur_heats;
//            // fprintf(stderr, "inner table size is %lu\n", cur_heats_table_locked_table_size(curHeats_locked));
//             fflush(stderr);
//             curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
//             curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
//             for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
//                  cur_heats_table_iterator_increment(curHeats_it))
//             {
//                 foundInner = *cur_heats_table_iterator_key(curHeats_it);
//                 Temperature temp = *cur_heats_table_iterator_mapped(curHeats_it); // later we need to add outter for loop to loop allHeats, so that the time stamp can get from there, but shouldn't matter anyway??
//                 fprintf(bookkeep_args->fd, "%ld.%ld\t%ld\t%ld\t%u\t%d\n",
//                         outter_key_wrapper.ts.tv_sec, outter_key_wrapper.ts.tv_nsec,
//                         foundInner, temp.diff, temp.cur_sizeof, outter_key_wrapper.scan_idx);
//                 // fflush(bookkeep_args->fd);
//             }

//             cur_heats_table_iterator_free(curHeats_end);
//             cur_heats_table_iterator_free(curHeats_it);
//             // here we it's safe to free each cur_heats_table
//             cur_heats_table_locked_table_free(curHeats_locked);
//             // cur_heats_table_free(curHeats); // how to get this?? TODO
//         }
//         all_heats_table_iterator_free(allHeats_it);
//         all_heats_table_iterator_free(allHeats_end);
//         IO_end = clock();
//         whole_IO_time = (double)(IO_end - IO_start) / CLOCKS_PER_SEC;
//         fprintf(stderr, "IO time: %.3f seconds\n", whole_IO_time);
//     }
//     fprintf(stderr, "total_hold_GIL_time: %.3f\n", total_hold_GIL_time);
//     all_heats_table_locked_table_free(allHeats_locked);
//     all_heats_table_free(allHeats);
//     terminate_flag = 0;
//     fprintf(stderr, "finish bookkeeping, shutdown\n");
// }

// only curHeats version
void *thread_trace_from_gc_list(void *arg)
{
    // fprintf(stderr, "__LP64__: %d\n", __LP64__); // 1
    /* TODO: later on needs to consider different generations tracking frequency.
        Larger # objs trace more frequently?? ie correlation btw obj lifetime VS obj hotness */
    fprintf(stderr, "start bookkeep thread\n");
    BookkeepArgs *bookkeep_args = (BookkeepArgs *)arg;
    if (bookkeep_args->fd == NULL)
    {
        perror("Failed to find fd passed in\n");
    }
    PyGILState_STATE gstate;
    all_heats_table *allHeats = all_heats_table_init(0);
    all_heats_table_locked_table *allHeats_locked = NULL;
    ts_blob outter_key_wrapper;
    cur_heats_table *curHeats = NULL;
    cur_heats_table_locked_table *curHeats_locked = NULL;
    unsigned int doIO_ = bookkeep_args->doIO;
    int cur_scan_idx = 0; // indicates how many fast scan bk thread has done, resets to zero when reaching rescan_thresh
    int cur_slow_idx = 0;
    struct timespec ts;
    clock_t update_prev_refcnt_start, update_prev_refcnt_end, insert_record_start, insert_record_end, IO_start, IO_end;
    double update_prev_refcnt_time = 0.0, total_hold_GIL_time = 0.0, insert_record_time = 0.0, whole_IO_time = 0.0, total_hold_fast_update_refcnt_time = 0.0, total_insert_record_time = 0.0;
    int total_fast_num = 0, total_slow_num = 0;

    clock_t start_GC_list, end_GC_list;
    double time_GC_list = 0.0, total_time_GC_List = 0.0;
    int rescan_thresh = bookkeep_args->rescan_thresh;
    if (rescan_thresh == 0)
    {
        perror("rescan_thresh cannot be 0\n");
        return NULL;
    }
    // cur_heats_table *last_few_scans_arr[rescan_thresh]; // # slow: 1: # fast: rescan_thresh - 1
    // cur_heats_table_locked_table *last_few_scans_arr_locked[rescan_thresh];
    uintptr_t prev_changed_max = 0;
    uintptr_t prev_changed_min = ULONG_MAX;
    uintptr_t no_93_upper = 100000000000000;
    while (!terminate_flag)
    {
        update_prev_refcnt_start = clock();
        /* slow trace: capture all `live` objects
         * fast trace: for `live` objs, capture refcnt changes */
        if (cur_scan_idx == 0) // slow
        {                      // slow trace, bookkeep_args->gen: [-1, 0, 1, 2, 3, 4],
                               // -1: means all generations; 3: gen 0 + gen 1; 4: gen 1 + gen 2
        reset_slow:
            curHeats = cur_heats_table_init(0); // only init curHeats at slow scan
            total_slow_num += 1;
            op_gc_table *cur_op_gc_table;
            op_gc_table_locked_table *cur_op_gc_locked_table;
            cur_op_gc_table = op_gc_table_init(0);
            fprintf(stderr, "slow peeking...\n");
            gstate = PyGILState_Ensure();
            start_GC_list = clock();

            // if (cur_slow_idx == 0)
            // { // trace all gens
            //     gc_get_objects_impl_op_gc(-1, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 1)
            // { // trace gen0
            //     gc_get_objects_impl_op_gc(0, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 2)
            // { // trace gen1
            //     gc_get_objects_impl_op_gc(0, cur_op_gc_table);
            //     // append_moved_objs(cur_op_gc_table); // causes segfaults in PyIter_Next() in cascade, because of freed objs are not updated?
            // }
            // else if (cur_slow_idx == 3)
            // { // trace gen2
            //     gc_get_objects_impl_op_gc(3, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 4)
            // { // trace gen0 + gen 1
            //     gc_get_objects_impl_op_gc(3, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 5)
            // { // trace gen1 + gen2
            //     gc_get_objects_impl_op_gc(4, cur_op_gc_table);
            // }

            // uncomment this to scan all GC lists
            gc_get_objects_impl_op_gc(-1, cur_op_gc_table);

            end_GC_list = clock();

            // cascade tracing
            fprintf(stderr, "updating recursive...\n");
            cur_op_gc_locked_table = op_gc_table_lock_table(cur_op_gc_table); // for iterating, remember to free this
            uintptr_t each_op;
            op_gc_table_iterator *op_gc_it = op_gc_table_locked_table_begin(cur_op_gc_locked_table);
            op_gc_table_iterator *op_gc_end = op_gc_table_locked_table_end(cur_op_gc_locked_table);
            // Temperature dummy_temp = {0, 0, 0};
            Temperature dummy_temp = {
                .prev_refcnt = 0, // Initialize prev_refcnt
                .diffs = {0},     // Initialize all elements of diffs to 0
                .cur_sizeof = 0   // Example: Initialize cur_sizeof to the size of the Temperature struct
            };
            for (; !op_gc_table_iterator_equal(op_gc_it, op_gc_end); op_gc_table_iterator_increment(op_gc_it))
            {
                each_op = *op_gc_table_iterator_key(op_gc_it);
                cur_heats_table_insert(curHeats, &each_op, &dummy_temp);
                PyObject *container_op = (PyObject *)each_op;
                // if (_Py_IsImmortal(container_op))
                // // {
                // PyObject_Print(container_op, stderr, 1);
                // fprintf(stderr, " in container list\n");
                // }
                update_recursive(container_op, curHeats); // inserts dummy_temp here
            }
            PyGILState_Release(gstate);
            op_gc_table_iterator_free(op_gc_end);
            op_gc_table_iterator_free(op_gc_it);
            op_gc_table_locked_table_free(cur_op_gc_locked_table);
            op_gc_table_free(cur_op_gc_table);

            // now, update prev_refcnt for cascade traced obj
            // fprintf(stderr, "updating prev_refcnt...\n");
            // update_prev_refcnt_slow_trace(curHeats_locked, curHeats); // probaly no need to capture prev_refcnt here.. so I commented out
            time_GC_list = (double)(end_GC_list - start_GC_list) / CLOCKS_PER_SEC;
            total_time_GC_List += time_GC_list;
            if (cur_heats_table_size(curHeats) < 10)
            {
                fprintf(stderr, "rolling back from slow\n"); // this block probably shouldn't be reached
                // cur_heats_table_free(curHeats);
                ++cur_slow_idx;
                usleep(bookkeep_args->sample_dur);
                goto reset_slow;
            }
            // curHeats_locked = cur_heats_table_lock_table(curHeats);
            fprintf(stderr, "cur_slow_idx: %d, # GC list: %ld\n", cur_slow_idx, cur_heats_table_size(curHeats));
            outter_key_wrapper.cur_slow_idx = cur_slow_idx;
            if (++cur_slow_idx == 4)
            {
                cur_slow_idx = 0;
            }
            // insert to allHeats to keep track of time
            clock_gettime(CLOCK_MONOTONIC, &ts);
            outter_key_wrapper.ts = ts;
            uintptr_t curHeats_casted = (uintptr_t)curHeats;
            all_heats_table_insert(allHeats, &outter_key_wrapper, &curHeats_casted);
        }
        else
        { // fast
            // if (cur_heats_table_locked_table_size(last_few_scans_arr_locked[cur_scan_idx - 1]) == 0)
            // {
            //     fprintf(stderr, "rolling back from fast\n");
            //     usleep(bookkeep_args->sample_dur);
            //     goto reset_slow;
            // }
            total_fast_num += 1;
            cur_heats_table_iterator *curHeats_it, *curHeats_end;
            fprintf(stderr, "fast peeking...\n");
            uintptr_t foundInner;
            Temperature *temp_ptr;
            // Temperature *dummy_temp = (Temperature *)malloc(sizeof(Temperature));                           // this also works
            // prevHeats_it = cur_heats_table_locked_table_begin(last_few_scans_arr_locked[cur_scan_idx - 1]); // iterate the previous curHeats
            // prevHeats_end = cur_heats_table_locked_table_end(last_few_scans_arr_locked[cur_scan_idx - 1]);
            // SoA version:
            curHeats_locked = cur_heats_table_lock_table(curHeats);
            curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
            curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
            cur_heats_table_locked_table_unlock(curHeats_locked);
            if (cur_scan_idx == 1) // update all traced objs' prev_refcnt, from casecad trace
            {
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_iterator_increment(curHeats_it))
                {
                    foundInner = *cur_heats_table_iterator_key(curHeats_it);
                    PyObject *each_op = (PyObject *)foundInner;
                    temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                    temp_ptr->prev_refcnt = each_op->ob_refcnt; // this should update the value in place
                    // cur_heats_table_insert(curHeats, &foundInner, &dummy_temp);
                }
            }
            else if (cur_scan_idx == 2)
            { // only capture those within the diff range
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_iterator_increment(curHeats_it))
                {
                    foundInner = *cur_heats_table_iterator_key(curHeats_it);
                    /* Reason being outside: no matter where the objs lies in,
                        we update the prev_refcnt for accuracy,
                        but need to check performance difference */
                    if (foundInner > prev_changed_min && foundInner < prev_changed_max)
                    {
                        PyObject *each_op = (PyObject *)foundInner;
                        temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                        temp_ptr->prev_refcnt = each_op->ob_refcnt;
                        // cur_heats_table_insert(curHeats, &foundInner, &dummy_temp);
                    }
                    else
                    {
                        cur_heats_table_erase(curHeats, &foundInner);
                    }
                }
            }
            else
            {
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_iterator_increment(curHeats_it))
                {
                    foundInner = *cur_heats_table_iterator_key(curHeats_it);
                    // if (foundInner > prev_changed_min && foundInner < prev_changed_max)
                    // {
                    PyObject *each_op = (PyObject *)foundInner;
                    temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                    temp_ptr->prev_refcnt = each_op->ob_refcnt;
                    // }
                }
            }
            cur_heats_table_iterator_free(curHeats_end);
            cur_heats_table_iterator_free(curHeats_it);
            cur_heats_table_locked_table_free(curHeats_locked);
        }
        update_prev_refcnt_end = clock();
        update_prev_refcnt_time = (double)(update_prev_refcnt_end - update_prev_refcnt_start) / CLOCKS_PER_SEC;
        fprintf(stderr, "trace %d, # all: %ld, update prev_cnt time: %.3f second\n", cur_scan_idx, cur_heats_table_size(curHeats), update_prev_refcnt_time);
        if (cur_scan_idx == 0)
        {
            total_hold_GIL_time += update_prev_refcnt_time;
        }
        else
        {
            total_hold_fast_update_refcnt_time += update_prev_refcnt_time;
        }
        // outter_key_wrapper.scan_idx = cur_scan_idx;
        // Py_BEGIN_ALLOW_THREADS
        if (cur_scan_idx == 0)
        {
        }
        else
        {
            usleep(bookkeep_args->sample_dur);
        }
        // Py_END_ALLOW_THREADS

        if (cur_scan_idx != 0)
        { // fast trace
            int num_changed = 0;
            insert_record_start = clock();

            cur_heats_table_iterator *curHeats_it, *curHeats_end;
            curHeats_locked = cur_heats_table_lock_table(curHeats);
            curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
            curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
            cur_heats_table_locked_table_unlock(curHeats_locked);
            uintptr_t each_op;
            Temperature *temp_ptr;
            if (cur_scan_idx == 1)
            {
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
                     cur_heats_table_iterator_increment(curHeats_it))
                {
                    each_op = *cur_heats_table_iterator_key(curHeats_it);
                    PyObject *op = (PyObject *)each_op;
                    temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                    // temp.cur_sizeof = 0;
                    temp_ptr->diffs[0] = op->ob_refcnt - temp_ptr->prev_refcnt; // ie, cur_scan_idx - 1
                    if (temp_ptr->diffs[0] != 0)
                    {
                        num_changed += 1;
                        if (each_op > prev_changed_max)
                        {
                            prev_changed_max = each_op;
                        }
                        else if (each_op < prev_changed_min && each_op > no_93_upper)
                        {
                            prev_changed_min = each_op;
                        }
                    }
                    // cur_heats_table_update(curHeats, &each_op, temp_ptr);
                }
                fprintf(stderr, "prev_changed_min: %ld, prev_changed_max: %ld, range: %ld\n", prev_changed_min, prev_changed_max, prev_changed_max - prev_changed_min);
            }
            else
            { // TODO: enabling this part will skip updating boundary after fast 1: better performance for constant working set workloads
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
                     cur_heats_table_iterator_increment(curHeats_it))
                {
                    each_op = *cur_heats_table_iterator_key(curHeats_it);
                    PyObject *op = (PyObject *)each_op;
                    temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                    temp_ptr->diffs[cur_scan_idx - 1] = op->ob_refcnt - temp_ptr->prev_refcnt;
                    if (temp_ptr->diffs[cur_scan_idx - 1] != 0)
                    {
                        num_changed += 1;
                    }
                    // cur_heats_table_update(curHeats, &each_op, temp_ptr); // is this necessary? NO
                }
            }
            insert_record_end = clock();
            insert_record_time = (double)(insert_record_end - insert_record_start) / CLOCKS_PER_SEC;
            fprintf(stderr, "insert record time: %.3f seconds, changed: %d\n", insert_record_time, num_changed);
            total_insert_record_time += insert_record_time;
            // usleep(bookkeep_args->sample_dur);

            // // /* now do this again */
            // curHeats_locked = cur_heats_table_lock_table(curHeats); // need to lock it again? probably not
            // curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
            // cur_heats_table_locked_table_unlock(curHeats_locked);
            // if (cur_scan_idx == 1)
            // {
            //     for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
            //          cur_heats_table_iterator_increment(curHeats_it))
            //     {
            //         each_op = *cur_heats_table_iterator_key(curHeats_it);
            //         PyObject *op = (PyObject *)each_op;
            //         temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
            //         temp_ptr->diff = (long)(temp_ptr->most_recent_refcnt - temp_ptr->prev_refcnt);
            //         // temp.diff = (long)(op->ob_refcnt - op->prev_refcnt);
            //         if (temp_ptr->diff != 0)
            //         {
            //             if (each_op > prev_changed_max)
            //             {
            //                 prev_changed_max = each_op;
            //             }
            //             else if (each_op < prev_changed_min && each_op > no_93_upper)
            //             {
            //                 prev_changed_min = each_op;
            //             }
            //         }
            //         cur_heats_table_update(curHeats, &each_op, temp_ptr);
            //     }
            // }
            // else
            // {
            //     for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
            //          cur_heats_table_iterator_increment(curHeats_it))
            //     {
            //         each_op = *cur_heats_table_iterator_key(curHeats_it);
            //         PyObject *op = (PyObject *)each_op;
            //         temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
            //         temp_ptr->diff = (long)(temp_ptr->most_recent_refcnt - temp_ptr->prev_refcnt);
            //         // temp.diff = (long)(op->ob_refcnt - op->prev_refcnt);
            //         cur_heats_table_update(curHeats, &each_op, temp_ptr);
            //     }
            // }
            cur_heats_table_iterator_free(curHeats_end);
            cur_heats_table_iterator_free(curHeats_it);
            cur_heats_table_locked_table_free(curHeats_locked);
        }
        // if (cur_scan_idx == 0)
        // {
        //     uintptr_t curHeats_casted = (uintptr_t)curHeats;
        //     all_heats_table_insert(allHeats, &outter_key_wrapper, &curHeats_casted);
        // }
        // last_few_scans_arr[cur_scan_idx] = curHeats;
        // last_few_scans_arr_locked[cur_scan_idx] = curHeats_locked;
        // fprintf(stderr, "fast %d 33333\n", cur_scan_idx);
        // if (doIO_ && cur_scan_idx != 0)
        // {
        //     clock_t start_IO, end_IO;
        //     double IO_time;
        //     cur_heats_table_iterator *curHeats_it, *curHeats_end;
        //     start_IO = clock();
        //     uintptr_t foundInner;
        //     fprintf(stderr, "doing IO...\n");
        //     curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
        //     curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
        //     for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
        //          cur_heats_table_iterator_increment(curHeats_it))
        //     {
        //         foundInner = *cur_heats_table_iterator_key(curHeats_it);
        //         Temperature temp = *cur_heats_table_iterator_mapped(curHeats_it); // later we need to add outter for loop to loop allHeats, so that the time stamp can get from there, but shouldn't matter anyway??
        //         fprintf(bookkeep_args->fd, "%ld.%ld\t%ld\t%ld\t%u\t%d\n",
        //                 outter_key_wrapper.ts.tv_sec, outter_key_wrapper.ts.tv_nsec,
        //                 foundInner, temp.diff, temp.cur_sizeof, outter_key_wrapper.scan_idx);
        //     }
        //     cur_heats_table_iterator_free(curHeats_end);
        //     cur_heats_table_iterator_free(curHeats_it);
        //     end_IO = clock();
        //     IO_time = (double)(end_IO - start_IO) / CLOCKS_PER_SEC;
        //     fprintf(stderr, "IO time: %.3f seconds\n", IO_time);
        // }
        if (++cur_scan_idx == rescan_thresh) // we first increment, then compare. Thus when cur_scan_idx == 4, we reset the idx to 0 before next loop
        {
            // cur_heats_table_locked_table_free(last_few_scans_arr_locked[0]); // if we do IO at last, here we only need to free slow scan curHheats
            // cur_heats_table_free(last_few_scans_arr[0]);
            // for (int i = 0; i < cur_scan_idx; i++)
            // {
            //     // cur_heats_table_locked_table_free(last_few_scans_arr_locked[i]);
            //     // cur_heats_table_free(last_few_scans_arr[i]);
            //     last_few_scans_arr[i] = NULL;
            //     last_few_scans_arr_locked[i] = NULL;
            // }
            // cur_heats_table_free(curHeats);
            cur_scan_idx = 0;
        }
    }

    // dump traces
    if (doIO_)
    {
        fprintf(stderr, "doing IO...\n");
        fflush(stderr);
        all_heats_table_iterator *allHeats_it, *allHeats_end;
        cur_heats_table_iterator *curHeats_it, *curHeats_end;
        IO_start = clock();
        uintptr_t found_each_cur_heats;
        uintptr_t foundInner;
        allHeats_locked = all_heats_table_lock_table(allHeats);
        allHeats_it = all_heats_table_locked_table_begin(allHeats_locked);
        allHeats_end = all_heats_table_locked_table_end(allHeats_locked);
        for (; !all_heats_table_iterator_equal(allHeats_it, allHeats_end); all_heats_table_iterator_increment(allHeats_it))
        {
            outter_key_wrapper = *all_heats_table_iterator_key(allHeats_it);
            found_each_cur_heats = *all_heats_table_iterator_mapped(allHeats_it);
            curHeats = (cur_heats_table *)found_each_cur_heats;
            curHeats_locked = cur_heats_table_lock_table(curHeats);
            // fprintf(stderr, "inner table size is %lu\n", cur_heats_table_locked_table_size(curHeats_locked));
            // fflush(stderr);
            curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
            curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
            Temperature *temp_ptr;
            for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
                 cur_heats_table_iterator_increment(curHeats_it))
            {
                foundInner = *cur_heats_table_iterator_key(curHeats_it);
                temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                fprintf(bookkeep_args->fd, "%ld.%ld\t%ld",
                        outter_key_wrapper.ts.tv_sec, outter_key_wrapper.ts.tv_nsec, foundInner);
                for (int i = 0; i < rescan_thresh - 1; i++)
                {
                    fprintf(bookkeep_args->fd, "\t%ld", temp_ptr->diffs[i]);
                }
                fprintf(bookkeep_args->fd, "\t%d\n", outter_key_wrapper.cur_slow_idx);
            }

            cur_heats_table_iterator_free(curHeats_end);
            cur_heats_table_iterator_free(curHeats_it);
            cur_heats_table_locked_table_free(curHeats_locked);
            cur_heats_table_free(curHeats);
        }
        all_heats_table_iterator_free(allHeats_it);
        all_heats_table_iterator_free(allHeats_end);
        IO_end = clock();
        whole_IO_time = (double)(IO_end - IO_start) / CLOCKS_PER_SEC;
        fprintf(stderr, "flushing time: %.3f seconds\n", whole_IO_time);
    }
    double avg_hold_GIL_time = total_hold_GIL_time / total_slow_num;
    fprintf(stderr, "total slow num: %d, avg hold GIL time: %.3f, total hold GIL time: %.3f\n",
            total_slow_num, avg_hold_GIL_time, total_hold_GIL_time);
    double avg_update_time = total_hold_fast_update_refcnt_time / total_fast_num;
    double avg_insert_time = total_insert_record_time / total_fast_num;
    fprintf(stderr, "total_fast_num: %d, avg fast update time(no GIL): %.3f, avg fast insert time(no GIL): %.3f\n",
            total_fast_num, avg_update_time, avg_insert_time);
    // fprintf(stderr, "total_traverse_GC_list_time: %.3f\n", total_time_GC_List);

    all_heats_table_locked_table_free(allHeats_locked);
    all_heats_table_free(allHeats);
    terminate_flag = 0;
    fprintf(stderr, "finish bookkeeping, shutdown\n");
    return NULL;
}

extern volatile short terminate_flag_refchain;
#ifdef Py_TRACE_REFS

static inline void refchain_2_kv_vec(PyInterpreterState *interp)
{
    PyObject *refchain = &interp->object_state.refchain;
    PyObject *op;
    unsigned long live_obj_count = 0;
    clock_t start_refchain_vec = clock();
    for (op = refchain->_ob_next; op != refchain; op = op->_ob_next)
    {
        // uintptr_t container_op = (uintptr_t)op;
        kv_push(PyObject *, local_ptr_vec, op);
        live_obj_count++;
    }
    double refchain_vec_time = (double)(clock() - start_refchain_vec) / CLOCKS_PER_SEC;
    fprintf(stderr, "refchain_2_kv_vec time: %.3f, live_obj_count: %lu\n", refchain_vec_time, live_obj_count);
}
void *thread_trace_from_refchain(void *arg)
{
    global_bookkeep_args = (BookkeepArgs *)arg;
    int fast_scan_idx = -1;
    uintptr_t no_93_upper = 100000000000000;
    int rescan_thresh = global_bookkeep_args->rescan_thresh;
    unsigned int doIO_ = global_bookkeep_args->doIO;
    struct timespec ts;
    clock_t cur_fast_start;
    while (!terminate_flag_refchain)
    {
        fprintf(stderr, "peeking refchain...\n");
        kv_init(local_ptr_vec);
        PyGILState_STATE gstate = PyGILState_Ensure();
        PyThreadState *tstate = _PyThreadState_GET();
        PyInterpreterState *interp = tstate->interp;
        refchain_2_kv_vec(interp);
        PyGILState_Release(gstate);
        // gen_temps_refchain();
        // cur_fast_start = clock();
        // double cur_fast_time = ((double)(clock() - cur_fast_start)) / CLOCKS_PER_SEC;
        unsigned long cur_size = kv_size(local_ptr_vec);
        fprintf(stderr, "cur_size: %lu\n", cur_size);
        if (doIO_)
        {
            fprintf(stderr, "flushing...\n");
            PyGILState_STATE gstate_io = PyGILState_Ensure();
            struct timespec ts;
            clock_gettime(CLOCK_MONOTONIC, &ts);
            for (int i = 0; i < cur_size; i++)
            {
                // bool print_obj = false;
                // for (int j = 0; j < rescan_thresh; j++)
                // {
                //     short cur_his = all_temps[i].diffs[j];
                //     if (cur_his != 0)
                //     {
                //         print_obj = true;
                //         break;
                //     }
                //     // fprintf(global_bookkeep_args->fd, "%hd\t", all_temps[i].diffs[j]);
                // }
                PyObject *container = kv_A(local_ptr_vec, i);
                uintptr_t found_inner = (uintptr_t)container;
                {
                    fprintf(global_bookkeep_args->fd, "%ld.%ld\t%ld", ts.tv_sec, ts.tv_nsec, found_inner);
                    // fprintf(global_bookkeep_args->fd, "\t%s", Py_TYPE(found_inner)->tp_name);
                    for (int j = 0; j < rescan_thresh; j++)
                    {
                        fprintf(global_bookkeep_args->fd, "\t0");
                    }
                    // fprintf(global_bookkeep_args->fd, "\t");
                    // PyObject_Print(all_temps[i].op, global_bookkeep_args->fd, 0);
                    fprintf(global_bookkeep_args->fd, "\n");
                }
            }
            fflush(global_bookkeep_args->fd);
            PyGILState_Release(gstate_io);
        }
        usleep(global_bookkeep_args->sample_dur);
        kv_destroy(local_ptr_vec);
    }
    return NULL;
}

#endif /*PY_TRACE_REFS*/

void *use_pref_cnt_modified(void *arg)
{
    int cur_scan_idx = 0;
    int cur_slow_idx = 0;
    BookkeepArgs *bookkeep_args = (BookkeepArgs *)arg;
    unsigned int doIO_ = bookkeep_args->doIO;
    struct timespec ts;
    PyGILState_STATE gstate;
    all_heats_table *allHeats = all_heats_table_init(0);
    all_heats_table_locked_table *allHeats_locked = NULL;
    ts_blob outter_key_wrapper;
    cur_heats_table *curHeats = NULL;
    cur_heats_table_locked_table *curHeats_locked = NULL;
    uintptr_t prev_changed_max = 0;
    uintptr_t prev_changed_min = ULONG_MAX;
    uintptr_t no_93_upper = 100000000000000;
    int rescan_thresh = bookkeep_args->rescan_thresh;
    int total_fast_num = 0, total_slow_num = 0;
    clock_t IO_start, IO_end;
    struct timeval update_prev_refcnt_start, update_prev_refcnt_end;
    double update_prev_refcnt_time = 0.0, total_hold_GIL_time = 0.0, whole_IO_time = 0.0, total_capture_hotness_time = 0.0;
    int actual_sleep_dur = 0;
    op_gc_table *cur_op_gc_table, *changed_op;
    while (!terminate_flag_refchain)
    {
        unsigned int elapsedTime_microsec;
        gettimeofday(&update_prev_refcnt_start, NULL);
        // int num_changed = 0;
        if (cur_scan_idx == 0)
        {
        reset_slow:
            changed_op = op_gc_table_init(0);   // to store objs refcnt get changed
            curHeats = cur_heats_table_init(0); // only init curHeats at slow scan
            total_slow_num += 1;
            op_gc_table_locked_table *cur_op_gc_locked_table;
            cur_op_gc_table = op_gc_table_init(0);
            fprintf(stderr, "slow peeking...\n");
            gstate = PyGILState_Ensure();
            // if (cur_slow_idx == 0)
            // { // trace all gens
            //     gc_get_objects_impl_op_gc(-1, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 1)
            // { // trace gen0
            //     gc_get_objects_impl_op_gc(0, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 2)
            // { // trace gen0
            //     gc_get_objects_impl_op_gc(0, cur_op_gc_table);
            //     // append_moved_objs(cur_op_gc_table); // causes segfaults in PyIter_Next() in cascade, because of freed objs are not updated?
            // }
            // else if (cur_slow_idx == 3)
            // { // trace gen0 + gen1
            //     gc_get_objects_impl_op_gc(3, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 4)
            // { // trace gen0 + gen 1
            //     gc_get_objects_impl_op_gc(3, cur_op_gc_table);
            // }
            // else if (cur_slow_idx == 5)
            // { // trace gen1 + gen2
            //     gc_get_objects_impl_op_gc(4, cur_op_gc_table);
            // }
            // uncomment this to scan all GC lists
            gc_get_objects_impl_op_gc(-1, cur_op_gc_table);
            cur_op_gc_locked_table = op_gc_table_lock_table(cur_op_gc_table);
            uintptr_t foundInner;
            op_gc_table_iterator *op_gc_it = op_gc_table_locked_table_begin(cur_op_gc_locked_table);
            op_gc_table_iterator *op_gc_end = op_gc_table_locked_table_end(cur_op_gc_locked_table);
            // for utlist
            // int utlist_count;
            // heat_node *elt;
            // heat_node *heat_node_head = NULL;
            // clock_t start_utlist = clock();
            // end utlist
            for (; !op_gc_table_iterator_equal(op_gc_it, op_gc_end); op_gc_table_iterator_increment(op_gc_it))
            {
                Temperature dummy_temp = {
                    .prev_refcnt = 0, // Initialize prev_refcnt
                    .diffs = {0},     // Initialize all elements of diffs to 0
                    .cur_sizeof = 0   // Example: Initialize cur_sizeof to the size of the Temperature struct
                };
                foundInner = *op_gc_table_iterator_key(op_gc_it);
                PyObject *container_op = (PyObject *)foundInner;
                // update cur_size and insert to curHeats
                // dummy_temp.cur_sizeof = _PySys_GetSizeOf(container_op);
                cur_heats_table_insert(curHeats, &foundInner, &dummy_temp);

                // container_op->hotness = 0; // added for hotness
                update_recursive(container_op, curHeats);

                // heat_node *each_node = malloc(sizeof(heat_node));
                // each_node->temp = &dummy_temp;
                // each_node->op = foundInner;
                // DL_APPEND(heat_node_head, each_node);
                // update_recursive_utlist(container_op, &heat_node_head);
            }
            // for utlist
            // DL_FOREACH(heat_node_head, elt)
            // fprintf(stderr, "op: %ld\n", elt->op);
            // clock_t end_utlist = clock();
            // double time_taken_utlist = ((double)(end_utlist - start_utlist)) / CLOCKS_PER_SEC;
            // DL_COUNT(heat_node_head, elt, utlist_count);
            // fprintf(stderr, "utlist time: %.3f seconds, size is %d\n", time_taken_utlist, utlist_count);
            // end utlist
            PyGILState_Release(gstate);
            op_gc_table_iterator_free(op_gc_end);
            op_gc_table_iterator_free(op_gc_it);
            op_gc_table_locked_table_free(cur_op_gc_locked_table);
            op_gc_table_free(cur_op_gc_table);
            if (cur_heats_table_size(curHeats) < 10)
            {
                fprintf(stderr, "rolling back from slow, this block shouldn't be reached\n");
                ++cur_slow_idx;
                usleep(bookkeep_args->sample_dur);
                goto reset_slow;
            }
            outter_key_wrapper.cur_slow_idx = cur_slow_idx;
            if (++cur_slow_idx == 4)
            {
                cur_slow_idx = 0;
            }
            // insert to allHeats to keep track of time
            clock_gettime(CLOCK_MONOTONIC, &ts);
            outter_key_wrapper.ts = ts;
            uintptr_t curHeats_casted = (uintptr_t)curHeats;
            all_heats_table_insert(allHeats, &outter_key_wrapper, &curHeats_casted);
        }
        else
        { // fast
            uint8_t dummy_val_changed = 0;
            total_fast_num += 1;
            uintptr_t foundInner;
            Temperature *temp_ptr;
            cur_heats_table_iterator *curHeats_it, *curHeats_end;
            curHeats_locked = cur_heats_table_lock_table(curHeats);
            curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
            curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
            cur_heats_table_locked_table_unlock(curHeats_locked);
            if (cur_scan_idx == 1)
            {
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_iterator_increment(curHeats_it))
                {
                    foundInner = *cur_heats_table_iterator_key(curHeats_it);
                    PyObject *op = (PyObject *)foundInner;
                    temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                    // temp_ptr->diffs[0] = op->hotness; // pay attention that temp_ptr->diffs[0] is not hotness, but acting as `prev_refcnt`
                    // temp_ptr->diffs[0] = op->ob_refcnt - temp_ptr->prev_refcnt;
                    // update boundary
                    // if (temp_ptr->diffs[0] != 0)
                    // {
                    //     num_changed += 1;
                    //     if (foundInner > prev_changed_max)
                    //     {
                    //         prev_changed_max = foundInner;
                    //     }
                    //     else if (foundInner < prev_changed_min && foundInner > no_93_upper)
                    //     {
                    //         prev_changed_min = foundInner;
                    //     }
                    // }
                    temp_ptr->prev_refcnt = op->hotness; // immediately upodate pref_refcnt here
                }
            }
            else if (cur_scan_idx == 2)
            {
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
                     cur_heats_table_iterator_increment(curHeats_it))
                {
                    foundInner = *cur_heats_table_iterator_key(curHeats_it);
                    PyObject *op = (PyObject *)foundInner;
                    temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                    // temp_ptr->diffs[1] = op->ob_refcnt - temp_ptr->prev_refcnt;
                    temp_ptr->diffs[1] = op->hotness - temp_ptr->prev_refcnt;
                    if (temp_ptr->diffs[1] != 0)
                    {
                        op_gc_table_insert(changed_op, &foundInner, &dummy_val_changed);
                        // num_changed += 1;
                        if (foundInner > prev_changed_max)
                        {
                            prev_changed_max = foundInner;
                        }
                        else if (foundInner < prev_changed_min && foundInner > no_93_upper)
                        {
                            prev_changed_min = foundInner;
                        }
                    }
                    temp_ptr->prev_refcnt = op->hotness;
                }
            }
            else if (cur_scan_idx == 3)
            {
                // gstate = PyGILState_Ensure();
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
                     cur_heats_table_iterator_increment(curHeats_it))
                {
                    foundInner = *cur_heats_table_iterator_key(curHeats_it);
                    if (foundInner > prev_changed_min && foundInner < prev_changed_max)
                    {
                        PyObject *op = (PyObject *)foundInner;
                        // fprintf(stderr, "%zu\t%ld\n", _PySys_GetSizeOf(op), _PySys_GetSizeOf(op));
                        temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                        // temp_ptr->diffs[2] = op->ob_refcnt - temp_ptr->prev_refcnt;
                        temp_ptr->diffs[2] = op->hotness - temp_ptr->prev_refcnt;
                        if (temp_ptr->diffs[2] != 0)
                        {
                            //     num_changed += 1;
                            op_gc_table_insert(changed_op, &foundInner, &dummy_val_changed);
                        }
                        temp_ptr->prev_refcnt = op->hotness;
                    }
                    else
                    { // filter out those outside boundary
                        cur_heats_table_erase(curHeats, &foundInner);
                    }
                }
                // PyGILState_Release(gstate);
            }
            else
            {
                for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end); cur_heats_table_iterator_increment(curHeats_it))
                {
                    foundInner = *cur_heats_table_iterator_key(curHeats_it);
                    PyObject *op = (PyObject *)foundInner;
                    temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                    // temp_ptr->diffs[cur_scan_idx - 1] = op->ob_refcnt - temp_ptr->prev_refcnt;
                    temp_ptr->diffs[cur_scan_idx - 1] = op->hotness - temp_ptr->prev_refcnt;
                    if (temp_ptr->diffs[cur_scan_idx - 1] != 0)
                    {
                        op_gc_table_insert(changed_op, &foundInner, &dummy_val_changed);
                        // num_changed += 1;
                    }
                    temp_ptr->prev_refcnt = op->hotness;
                }
            }
            cur_heats_table_iterator_free(curHeats_end);
            cur_heats_table_iterator_free(curHeats_it);
            cur_heats_table_locked_table_free(curHeats_locked);
            fprintf(stderr, "changed size: %ld\n", op_gc_table_size(changed_op));
        }
        // update_prev_refcnt_end = clock();
        gettimeofday(&update_prev_refcnt_end, NULL);
        // update_prev_refcnt_time = (double)(update_prev_refcnt_end - update_prev_refcnt_start) / CLOCKS_PER_SEC;
        elapsedTime_microsec = (update_prev_refcnt_end.tv_sec - update_prev_refcnt_start.tv_sec) * 1000000;
        elapsedTime_microsec += (update_prev_refcnt_end.tv_usec - update_prev_refcnt_start.tv_usec);

        update_prev_refcnt_time = (update_prev_refcnt_end.tv_sec - update_prev_refcnt_start.tv_sec) + (update_prev_refcnt_end.tv_usec - update_prev_refcnt_start.tv_usec) / 1000000.0;
        // fprintf(stderr, "elapsedTime_microsec: %u\n", elapsedTime_microsec);
        if (cur_scan_idx == 0)
        {
            fprintf(stderr, "slow: %.3f second\n", update_prev_refcnt_time);
            total_hold_GIL_time += update_prev_refcnt_time;
        }
        else
        {
            total_capture_hotness_time += update_prev_refcnt_time;
        }
        if (cur_scan_idx != 0) // only sleep after each fast cycle
        {
            fprintf(stderr, "fast %d, # all: %ld, bk record time: %.3f second, ", cur_scan_idx, cur_heats_table_size(curHeats), update_prev_refcnt_time);
            actual_sleep_dur = bookkeep_args->sample_dur - elapsedTime_microsec;
            if (actual_sleep_dur > 0)
            {
                fprintf(stderr, "actual sleep time: %u us\n", actual_sleep_dur);
                usleep(actual_sleep_dur);
            }
        }
        if (++cur_scan_idx == rescan_thresh)
        {
            cur_scan_idx = 0;
            op_gc_table_free(changed_op);
        }
    } // while not terminated
    if (doIO_)
    {
        assert(bookkeep_args->fd != NULL);
        fprintf(stderr, "doing IO...\n");
        fflush(stderr);
        all_heats_table_iterator *allHeats_it, *allHeats_end;
        cur_heats_table_iterator *curHeats_it, *curHeats_end;
        IO_start = clock();
        uintptr_t found_each_cur_heats;
        uintptr_t foundInner;
        allHeats_locked = all_heats_table_lock_table(allHeats);
        allHeats_it = all_heats_table_locked_table_begin(allHeats_locked);
        allHeats_end = all_heats_table_locked_table_end(allHeats_locked);
        gstate = PyGILState_Ensure();
        for (; !all_heats_table_iterator_equal(allHeats_it, allHeats_end); all_heats_table_iterator_increment(allHeats_it))
        {
            outter_key_wrapper = *all_heats_table_iterator_key(allHeats_it);
            found_each_cur_heats = *all_heats_table_iterator_mapped(allHeats_it);
            curHeats = (cur_heats_table *)found_each_cur_heats;
            curHeats_locked = cur_heats_table_lock_table(curHeats);
            // fprintf(stderr, "inner table size is %lu\n", cur_heats_table_locked_table_size(curHeats_locked));
            // fflush(stderr);
            curHeats_it = cur_heats_table_locked_table_begin(curHeats_locked);
            curHeats_end = cur_heats_table_locked_table_end(curHeats_locked);
            Temperature *temp_ptr;
            for (; !cur_heats_table_iterator_equal(curHeats_it, curHeats_end);
                 cur_heats_table_iterator_increment(curHeats_it))
            {
                foundInner = *cur_heats_table_iterator_key(curHeats_it);
                temp_ptr = cur_heats_table_iterator_mapped(curHeats_it);
                fprintf(bookkeep_args->fd, "%ld.%ld\t%ld",
                        outter_key_wrapper.ts.tv_sec, outter_key_wrapper.ts.tv_nsec, foundInner);
                bool if_changed = false;
                for (int i = 0; i < rescan_thresh - 1; i++)
                {
                    if (temp_ptr->diffs[i] != 0)
                    {
                        if_changed = true;
                    }
                    fprintf(bookkeep_args->fd, "\t%ld", temp_ptr->diffs[i]);
                }
                // fprintf(bookkeep_args->fd, "\t%d\n", outter_key_wrapper.cur_slow_idx);
                // fprintf(bookkeep_args->fd, "\t%ld\n", temp_ptr->cur_sizeof);
                // if (Py_Type(foundInner) && if_changed)
                // {
                //     temp_ptr->cur_sizeof = _PySys_GetSizeOf((PyObject *)foundInner);
                //     fprintf(bookkeep_args->fd, "\t%zu\n", temp_ptr->cur_sizeof);
                // }
                // else
                // {
                fprintf(bookkeep_args->fd, "\t0\n");
                // }
            }
            cur_heats_table_iterator_free(curHeats_end);
            cur_heats_table_iterator_free(curHeats_it);
            cur_heats_table_locked_table_free(curHeats_locked);
            cur_heats_table_free(curHeats);
        }
        PyGILState_Release(gstate);
        all_heats_table_iterator_free(allHeats_it);
        all_heats_table_iterator_free(allHeats_end);
        IO_end = clock();
        whole_IO_time = (double)(IO_end - IO_start) / CLOCKS_PER_SEC;
        fprintf(stderr, "flushing time: %.3f seconds\n", whole_IO_time);
    }

    double avg_hold_GIL_time = total_hold_GIL_time / total_slow_num;
    fprintf(stderr, "total slow num: %d, avg hold GIL time: %.3f, total hold GIL time: %.3f\n",
            total_slow_num, avg_hold_GIL_time, total_hold_GIL_time);
    double avg_update_time = total_capture_hotness_time / total_fast_num;
    fprintf(stderr, "total_fast_num: %d, avg capture hotness time: %.3f, total record hotness time: %.3f\n",
            total_fast_num, avg_update_time, total_capture_hotness_time);

    all_heats_table_locked_table_free(allHeats_locked);
    all_heats_table_free(allHeats);
    // op_gc_table_free(changed_op);
    terminate_flag_refchain = 0;
    fprintf(stderr, "finish bookkeeping, shutdown\n");
    return NULL;
}

void swap(OBJ_TEMP *a, OBJ_TEMP *b)
{
    OBJ_TEMP temp = *a;
    *a = *b;
    *b = temp;
}

// Dutch National Flag Algorithm, 3-way partitioning [0: low]: cold, [low: mid]: warm, [mid: old_num_op]: hot
void sort_dnf(int rescan_thresh, int *low, int *mid, int *high)
{
    fprintf(stderr, "before low: %d, mid: %d, high: %d\n", *low, *mid, *high);
    while (*mid <= *high)
    {
        if ((all_temps[*mid].diffs[rescan_thresh] & HOTNESS_MASK) < 1)
        {
            swap(&all_temps[*low], &all_temps[*mid]);
            (*low)++;
            (*mid)++;
        }
        else if ((all_temps[*mid].diffs[rescan_thresh] & HOTNESS_MASK) <= 7)
        {
            (*mid)++;
        }
        else
        {
            swap(&all_temps[*mid], &all_temps[*high]);
            (*high)--;
        }
    }
    fprintf(stderr, "after low: %d, mid: %d, high: %d\n", *low, *mid, *high);
}

// static void mergeIntervals(PyObj_range **intervalsPtr, int *size)
// {
//     clock_t t = clock();
//     PyObj_range *intervals = *intervalsPtr;
//     if (*size <= 1)
//         return;
//     qsort(intervals, *size, sizeof(PyObj_range), compareIntervals); // sort by start addr, this is slow

//     fprintf(stderr, "qsort time: %.3f\n", (double)(clock() - t) / CLOCKS_PER_SEC);
//     t = clock();
//     int mergedIndex = 0;
//     for (int i = 0; i < *size; i++)
//     {
//         intervals[i].start = (intervals[i].start / PAGE_SIZE) * PAGE_SIZE;
//         intervals[i].end = ((intervals[i].end + PAGE_SIZE - 1) / PAGE_SIZE) * PAGE_SIZE;
//         if (intervals[mergedIndex].end >= intervals[i].start)
//         {
//             intervals[mergedIndex].end = intervals[mergedIndex].end > intervals[i].end ? intervals[mergedIndex].end : intervals[i].end;
//         }
//         else
//         {
//             intervals[mergedIndex] = intervals[i];
//             mergedIndex++;
//         }
//     }
//     fprintf(stderr, "arrange time: %.3f\n", (double)(clock() - t) / CLOCKS_PER_SEC);
//     t = clock();
//     *size = mergedIndex + 1;
//     PyObj_range *newIntervals = realloc(intervals, (*size) * sizeof(PyObj_range));
//     if (newIntervals == NULL)
//     {
//         fprintf(stderr, "realloc failed\n");
//         free(intervals);
//         *intervalsPtr = NULL; // Set the original pointer to NULL to avoid dangling pointer
//     }
//     else
//     {
//         *intervalsPtr = newIntervals;
//     }
//     fprintf(stderr, "realloc time: %.3f\n", (double)(clock() - t) / CLOCKS_PER_SEC);
// }
static void mergeIntervals(PyObj_range **intervalsPtr, int *size)
{
    qsort(*intervalsPtr, *size, sizeof(PyObj_range), compareIntervals);

    PyObj_range *new_ranges = (PyObj_range *)malloc(*size * sizeof(PyObj_range));
    if (!new_ranges)
    {
        printf("Malloc new_ranges failed\n");
        return;
    }

    new_ranges[0].start = (*intervalsPtr)[0].start & PAGE_MASK;
    new_ranges[0].end = ((*intervalsPtr)[0].end + PAGE_SIZE - 1) & PAGE_MASK;

    int mergedIndex = 0;

    for (int i = 1; i < *size; ++i)
    {
        uintptr_t alignedStart = (*intervalsPtr)[i].start & PAGE_MASK;
        uintptr_t alignedEnd = ((*intervalsPtr)[i].end + PAGE_SIZE - 1) & PAGE_MASK;

        if (alignedStart <= new_ranges[mergedIndex].end)
        {
            new_ranges[mergedIndex].end = max(new_ranges[mergedIndex].end, alignedEnd);
        }
        else
        {
            mergedIndex++;
            new_ranges[mergedIndex].start = alignedStart;
            new_ranges[mergedIndex].end = alignedEnd;
        }
    }

    *size = mergedIndex + 1;
    PyObj_range *newIntervals = (PyObj_range *)realloc(new_ranges, (*size) * sizeof(PyObj_range));
    if (newIntervals == NULL)
    {
        free(new_ranges);
    }
    else
    {
        free(*intervalsPtr);
        *intervalsPtr = newIntervals;
    }
    // for (int i = 0; i < *size; i++)
    // {
    //     uintptr_t diff = newIntervals->end - newIntervals->start;
    //     if (diff > PAGE_SIZE)
    //     {
    //         fprintf(stderr, "obj size is larger than a PAGE!%ld\n", diff);
    //     }
    //     // fprintf(stderr, "start: %ld, end: %ld\n", newIntervals[i].start, newIntervals[i].end);
    // }
}

int isBoundaryPresent(uintptr_t boundary, void **boundaries, int size)
{
    for (int i = 0; i < size; ++i)
    {
        if ((uintptr_t)boundaries[i] == boundary)
        {
            return 1;
        }
    }
    return 0;
}
// void **getPageBoundaries(PyObj_range *intervals, int intervalSize, int *boundarySize)
// {
//     void **boundaries = malloc(intervalSize * 2 * sizeof(void *)); // Allocate maximum possible size
//     *boundarySize = 0;
//     for (int i = 0; i < intervalSize; i++)
//     {
//         for (uintptr_t j = intervals[i].start; j < intervals[i].end; j += PAGE_SIZE)
//         {
//             // if (j % PAGE_SIZE != 0)
//             // {
//             //     fprintf(stderr, "j not aligned %ld\n", j);
//             // }
//             // if (!isBoundaryPresent(j, boundaries, *boundarySize))
//             // if (!check_pages2move(j))
//             {
//                 // fprintf(stderr, "%ld\n", j);
//                 boundaries[*boundarySize] = (void *)j;
//                 (*boundarySize)++;
//                 // insert_pages2move(j);
//             }
//             // else
//             // {
//             //     fprintf(stderr, "already inserted %ld\n", j);
//             // }
//         }
//     }
//     // free_pages2move();
//     void **ret = realloc(boundaries, *boundarySize * sizeof(void *));
//     return ret;
// }

void **getPageBoundaries(PyObj_range *intervals, int intervalSize, int *boundarySize)
{
    *boundarySize = 0;

    // First pass: calculate the total number of page boundaries
    for (int i = 0; i < intervalSize; i++)
    {
        uintptr_t start_page = intervals[i].start / PAGE_SIZE;
        // uintptr_t end_page = (intervals[i].end + PAGE_SIZE - 1) / PAGE_SIZE; // Round up to include the last page
        uintptr_t end_page = intervals[i].end / PAGE_SIZE;
        *boundarySize += (end_page - start_page);
    }

    // Allocate memory for the total number of page boundaries
    void **boundaries = malloc(*boundarySize * sizeof(void *));
    if (!boundaries)
    {
        // Handle malloc failure
        *boundarySize = 0;
        return NULL;
    }

    // Second pass: populate the boundaries array
    int index = 0;
    for (int i = 0; i < intervalSize; i++)
    {
        for (uintptr_t j = intervals[i].start; j < intervals[i].end; j += PAGE_SIZE)
        {
            boundaries[index++] = (void *)(j - j % PAGE_SIZE);
        }
    }

    // No need to realloc, as we've allocated the exact amount of memory needed
    return boundaries;
}

double do_migration(void **pages, int num_pages, int dest_node)
{
    int *nodes = calloc(num_pages, sizeof(int));
    int *status = calloc(num_pages, sizeof(int));
    if (dest_node == 0)
    {
        // promotion, to DRAM
        for (int i = 0; i < num_pages; i++)
        {
            nodes[i] = 0; // DRAM
        }
    }
    else
    {
        // demotion, to CXL
        for (int i = 0; i < num_pages; i++)
        {
            nodes[i] = 1; // CXL
        }
    }
    struct timespec start, end;
    double elapsed;
    clock_gettime(CLOCK_MONOTONIC, &start);

    long ret = move_pages(0, num_pages, pages, nodes, status, MPOL_MF_MOVE);

    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = end.tv_sec - start.tv_sec;
    elapsed += (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    fprintf(stderr, "migrate time: %.3f\n", elapsed);
    for (int i = 0; i < num_pages; ++i)
    {
        if (status[i] < 0)
        {
            fprintf(stderr, "Page %lu not moved: %d\n", i, status[i]);
        }
    }
    free(pages);
    free(nodes);
    free(status);
    return elapsed;
}

// main merging & migration function
static double calculate_merge(int num_objs, int rescan_thresh)
{
    if (PyGILState_Check() || old_num_op == 0)
    {
        fprintf(stderr, "Unlikely, GIL held by slow or empty traced op, try merging later\n");
        usleep(250000); // 0.25s
        return;
    }
    // allocate the maximum possible size
    PyObj_range *pyobj_ranges = malloc(num_objs * sizeof(PyObj_range));
    if (!pyobj_ranges)
    {
        fprintf(stderr, "malloc failed\n");
        return -1.0;
    }

    int actual_size = 0;
    for (int i = 0; i < num_objs; i++)
    {
        // if (all_temps[i].diffs[rescan_thresh] != -1) // naive way to filter hot objs
        {
            pyobj_ranges[actual_size].start = (uintptr_t)all_temps[i].op;
            pyobj_ranges[actual_size].end = (uintptr_t)(all_temps[i].cur_sizeof) + pyobj_ranges[actual_size].start;
            actual_size++;
        }
    }
    // realloc to the actual size
    if (actual_size > 0)
    {
        PyObj_range *temp = realloc(pyobj_ranges, actual_size * sizeof(PyObj_range));
        if (temp == NULL)
        {
            free(pyobj_ranges); // Handle realloc failure
            fprintf(stderr, "realloc failed\n");
            return -1.0; // Or appropriate error handling
        }
        pyobj_ranges = temp;
    }
    else
    {
        fprintf(stderr, "all of them are COLD, failed\n");
        free(pyobj_ranges); // Handle realloc failure
        return -1.0;
    }

    int range_size = actual_size;
    fprintf(stderr, "Before merge size: %d\n", range_size);

    clock_t t = clock();
    mergeIntervals(&pyobj_ranges, &range_size); // TODO: this is too slow
    fprintf(stderr, "merging time: %.3f\n", (double)(clock() - t) / CLOCKS_PER_SEC);
    fprintf(stderr, "After merge/align page size: %d\n", range_size);

    int num_pages = 0;
    void **pages = getPageBoundaries(pyobj_ranges, range_size, &num_pages);
    fprintf(stderr, "num_pages: %d\n", num_pages);
    free(pyobj_ranges);
    if (0)
    { // debugging, make sure all pages are aligned
        for (int i = 0; i < num_pages; i++)
        {
            if ((uintptr_t)(pages[i]) % PAGE_SIZE != 0)
            {
                fprintf(stderr, "not aligned %ld\n", pages[i]);
            }
        }
    }
    do_migration(pages, num_pages, 0);

    return 0.0;
}

void **align_obj_2_page_bd(unsigned int num_op, uintptr_t *op_hotness_arr, int *num_pages)
{
    if (!op_hotness_arr)
        return NULL;

    for (unsigned int i = 0; i < num_op; i++)
    {
        (op_hotness_arr)[i] &= PAGE_MASK;
    }
    sortRawAddr(op_hotness_arr, num_op);
    size_t j = 0;
    for (size_t i = 1; i < num_op; ++i)
    {
        if ((op_hotness_arr)[j] != (op_hotness_arr)[i])
        {
            (op_hotness_arr)[++j] = (op_hotness_arr)[i];
        }
    }
    void **pages_to_dram = calloc(j + 1, sizeof(void *));
    for (size_t i = 0; i < j + 1; ++i)
    {
        pages_to_dram[i] = (void *)(op_hotness_arr)[i];
    }
    free(op_hotness_arr);

    *num_pages = j + 1;
    fprintf(stderr, "after aligning: %zu pages\n", j + 1);
    return pages_to_dram;
}

bool is_old_num_remain_still()
{
    double one_percent_diff_flunc = 0.01 * prev_num_op;
    double lower_bound = prev_num_op - one_percent_diff_flunc;
    double upper_bound = prev_num_op + one_percent_diff_flunc;
    if (old_num_op >= lower_bound && old_num_op <= upper_bound)
    {
        fprintf(stderr, "old_num_op not changed too much");
        return true;
    }
}

bool very_first_demote = true;
khash_t(ptrset_dup) * last_demote_pages;
double prev_c_w_percent = 0.0;
double try_trigger_migration_revised(int rescan_thresh)
{
    struct timespec start, end;
    double elapsed;

    cur_hot_in_all = (double)max_num_hot / old_num_op;
    fprintf(stderr, "--------\ncur_hot_in_all in try_trigger_migration: %.3f\n", cur_hot_in_all);
    unsigned int actual_cold_num = 0, actual_hot_num = 0;
    uintptr_t *cold_arr = NULL, *hot_arr = NULL;
    double cur_migration_time = 0.0;
    int cold_warm_idx = 0, warm_hot_idx = 0, high = old_num_op - 1;
    clock_gettime(CLOCK_MONOTONIC, &start);
    // cppDefaultSortAsc(all_temps, old_num_op);
    sort_dnf(rescan_thresh, &cold_warm_idx, &warm_hot_idx, &high);
    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = end.tv_sec - start.tv_sec;
    elapsed += (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    fprintf(stderr, "DNF sort time: %.3f, for %lu\n", elapsed, old_num_op);

    // |   cold   |        warm     |        hot   |
    // (      domote candidates    ) (promote candidates)
    // 0    cold_warm_idx      warm_hot_idx   old_num_op
    unsigned int expected_num_cold = warm_hot_idx;
    unsigned int expected_num_hot = old_num_op - warm_hot_idx;
    // under the following situation, no need to demote
    fprintf(stderr, "expected_num_hot: %d, expected_num_cold: %d\n", expected_num_hot, expected_num_cold);
    double cur_c_w_percent = (double)expected_num_cold / old_num_op;

    if (fabs(cur_c_w_percent - prev_c_w_percent) < 0.01 && is_old_num_remain_still())
    {
        fprintf(stderr, "relax domotion\n");
    }
    else
    { // demote cold
        cold_arr = calloc(expected_num_cold, sizeof(uintptr_t));
        if (cold_arr == NULL)
        {
            fprintf(stderr, "calloc failed for cold\n");
            return 0;
        }
        // try to get demotion to CXL
        for (unsigned int i = 0; i < expected_num_cold; i++)
        {
            // (dropped out || empty hotness) && not in CXL -> demote candidate
            if (((all_temps[i].diffs[rescan_thresh] & (1 << DROP_OUT_OFF)) || !(all_temps[i].diffs[rescan_thresh] & HOTNESS_MASK)) && !(all_temps[i].diffs[rescan_thresh] & (1 << LOCATION_OFF)))
            {
                all_temps[i].diffs[rescan_thresh] |= (1 << LOCATION_OFF); // mark to CXL: 1
                cold_arr[i] = (uintptr_t)all_temps[i].op;
                actual_cold_num++;
            }
        }
        if (actual_cold_num < expected_num_cold)
        {
            cold_arr = realloc(cold_arr, actual_cold_num * sizeof(uintptr_t));
        }
        if (actual_cold_num > 0)
        {
            int num_cold_pages;
            void **pages_candidate = align_obj_2_page_bd(actual_cold_num, cold_arr, &num_cold_pages); // cold_arr is freed inside
            if (num_cold_pages > 0)
            {
                int ret;
                if (very_first_demote)
                {
                    for (int i = 0; i < num_cold_pages; i++)
                    {
                        kh_put(ptrset_dup, last_demote_pages, pages_candidate[i], &ret);
                    }
                    cur_migration_time += do_migration(pages_candidate, num_cold_pages, 1); // pages are freed inside
                    fprintf(stderr, "demoted pages in first place: %d\n", num_cold_pages);
                    very_first_demote = false;
                }
                else
                {
                    void **current_pages = calloc(num_cold_pages, sizeof(void *)); // this is fine since # real pages always <= current num_cold_pages
                    unsigned int last_demote_pages_size = kh_size(last_demote_pages);
                    assert(last_demote_pages_size != 0);
                    int num_reappear = 0;
                    for (int i = 0; i < num_cold_pages; i++)
                    {
                        if (kh_get(ptrset_dup, last_demote_pages, pages_candidate[i]) != kh_end(last_demote_pages))
                        {
                            current_pages[num_reappear] = pages_candidate[i];
                            num_reappear++;
                        }
                    }
                    if (num_reappear > 0)
                    {
                        current_pages = realloc(current_pages, num_reappear * sizeof(void *));
                        double dupped_ratio = (double)num_reappear / last_demote_pages_size;
                        if (dupped_ratio > 0.9)
                        {
                            cur_migration_time += do_migration(current_pages, num_reappear, 1); // pages are freed inside
                            fprintf(stderr, "demoted pages in lazy: %d, ratio: %.3f\n", num_reappear, dupped_ratio);
                        }
                    }
                    else
                    {
                        free(current_pages);
                        current_pages = NULL;
                    }
                    // update last_demote_pages
                    kh_destroy(ptrset_dup, last_demote_pages);
                    last_demote_pages = kh_init(ptrset_dup);
                    for (int i = 0; i < num_reappear; i++)
                    {
                        kh_put(ptrset_dup, last_demote_pages, current_pages[i], &ret);
                    }
                    free(pages_candidate);
                }
            }
        }
    }
    prev_c_w_percent = cur_c_w_percent; // remember to update prev_c_w_percent for next try_migration

    // promote hot
    hot_arr = calloc(expected_num_hot, sizeof(uintptr_t));
    if (hot_arr == NULL)
    {
        fprintf(stderr, "calloc failed for hot\n");
        return 0;
    }

    // try to get promotion to DRAM
    for (unsigned int i = expected_num_cold; i < old_num_op; i++)
    {
        if ((all_temps[i].diffs[rescan_thresh] & (1 << LOCATION_OFF))) // in CXL, need to promote
        {
            all_temps[i].diffs[rescan_thresh] &= ~(1 << LOCATION_OFF); // mark to DRAM: 0
            hot_arr[i - expected_num_cold] = (uintptr_t)all_temps[i].op;
            actual_hot_num++;
        }
    }
    if (actual_hot_num < expected_num_hot)
    {
        hot_arr = realloc(hot_arr, actual_hot_num * sizeof(uintptr_t));
    }

    if (actual_hot_num > 0)
    {
        int num_hot_pages;
        void **pages = align_obj_2_page_bd(actual_hot_num, hot_arr, &num_hot_pages); // hot_arr is freed inside
        fprintf(stderr, "promoted pages: %d\n", num_hot_pages);
        cur_migration_time += do_migration(pages, num_hot_pages, 0); // pages are freed inside
    }
    fprintf(stderr, "actual_hot_num: %d, actual_cold_num: %d\n", actual_hot_num, actual_cold_num);
    fprintf(stderr, "cur_migration_time: %.3f\n", cur_migration_time);
    return cur_migration_time;
}

double try_trigger_migration(int rescan_thresh)
{
    struct timespec start, end;
    double elapsed;
    clock_t cur_time = clock();
    double cur_migration_time = 0.0;
    long time_elapsed = cur_time - last_migrate_time;
    if (time_elapsed < migration_time_thresh)
    {
        fprintf(stderr, "skipping migration\n");
        return;
    }

    unsigned int expected_num_hot = old_num_op * HOT_THRESH / 100;
    clock_gettime(CLOCK_MONOTONIC, &start);
    // 3.1 corner case: if old_num_op is large but cur_fast_num_hot is small, that means a lot of writing, then we by default migrate all to DRAM
    int actual_num_hot = 0;
    if (old_num_op > 1000000)
    {
        fprintf(stderr, "old_num_op is large but cur_fast_num_hot is small, shouldn't touch\n");
        // for (int i = 0; i < old_num_op; i++)
        // {
        //     if (!(all_temps[i].diffs[rescan_thresh] & (1 << LOCATION_OFF)))
        //     {
        //         all_temps[i].diffs[rescan_thresh] |= (1 << LOCATION_OFF);
        //         op_hotness_arr[actual_num_hot] = (uintptr_t)all_temps[i].op;
        //         if (++actual_num_hot == expected_num_hot)
        //             break;
        //     }
        // }
    }
    else // 3.2 normal case: sort all_temps by hotness, then pick the top expected_num_hot op's
    {
        unsigned int num_cold = 0, num_hot = 0;
        // qsort(all_temps, old_num_op, sizeof(OBJ_TEMP), compareOpHotnessDesc);
        cppDefaultSortAsc(all_temps, old_num_op); // sort all all_temps by hotness, cold -> warm -> hot. TODO: optimize sort (only full sort in first, partial sort later)
        // cppParallelSort(all_temps, old_num_op);

        for (int i = 0; i < old_num_op; i++)
        {
            if (!(all_temps[i].diffs[rescan_thresh] & HOTNESS_MASK) && !(all_temps[i].diffs[rescan_thresh] & (1 << LOCATION_OFF))) // active op && location is not in DRAM
            {
                all_temps[i].diffs[rescan_thresh] |= (1 << LOCATION_OFF); // mark to CXL: 1
                // kv_push(uintptr_t, cold_vec, (uintptr_t)all_temps[i].op);
                // op_hotness_arr[actual_num_hot] = (uintptr_t)all_temps[i].op;
                // if (++actual_num_hot == expected_num_hot)
                // break;
            }
            // else
            //     break;
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = end.tv_sec - start.tv_sec;
    elapsed += (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    fprintf(stderr, "get hot op time: %.3f seconds, original size: %ld, after truncation: %ld\n", elapsed, old_num_op, actual_num_hot);
    if (actual_num_hot < expected_num_hot && actual_num_hot > 0)
    {
        fprintf(stderr, "not enough hot op's, only %d, need to realloc\n", actual_num_hot);
        // op_hotness_arr = realloc(op_hotness_arr, actual_num_hot * sizeof(uintptr_t));
    }

    return cur_migration_time;
}

int check_dram_free()
{
    long long total_dram_size, free_dram_size;
    total_dram_size = numa_node_size(DRAM_MASK, NULL);
    while (!terminate_flag_refchain)
    {
        if (numa_node_size(DRAM_MASK, &free_dram_size) <= 0)
        {
            fprintf(stderr, "Failed to get NUMA node size\n");
            return NULL;
        }
        double freePercentage = ((double)free_dram_size / total_dram_size) * 100.0;

        if (freePercentage < TRIGGER_SCAN_WM) // 35%
        {
            fprintf(stderr, "Start triggering scan\n");
            return 1;
        }
        double freeMB = free_dram_size / 1048576.0;
        fprintf(stderr, "freePercentage: %.2f, no need to offload, free_dram_size: %.2f\n", freePercentage, freeMB);
        usleep(500000);
    }
    return 0; // terminated by user
}
void *manual_trigger_scan(void *arg)
{
    if (numa_available() == -1 || numa_num_configured_nodes() < 2)
    {
        fprintf(stderr, "CXL offloading is not supported!\n");
        return NULL;
    }
    // global_op_set = kh_init(ptrset);
    // init_global_set_helper();
    // if (check_dram_free() == 0)
    // {
    //     return NULL;
    // }
    // else if return 1: start triggering scan

    enable_bk = 1;
    global_bookkeep_args = (BookkeepArgs *)arg;
    if (!global_bookkeep_args->live_time_thresh_arg)
    {
        get_live_time_thresh = 3500000;
    }
    else
    {
        get_live_time_thresh = global_bookkeep_args->live_time_thresh_arg;
    }
    migration_time_thresh = 2000000; // 2s, thus, trigger migration by default
    allow_slow = 1;
    fprintf(stderr, "trigger from manual\n");
    int rescan_thresh = global_bookkeep_args->rescan_thresh;
    rescan_thresh_glb = rescan_thresh;
    unsigned int doIO_ = global_bookkeep_args->doIO;
    Temperature *temp_ptr;
    heat_node *node_ptr, *tmp;
    int fast_scan_idx = -1;
    int utlist_count;
    uintptr_t foundInner;
    uintptr_t prev_changed_max = 0;
    uintptr_t prev_changed_min = ULONG_MAX;
    uintptr_t no_93_upper = 100000000000000;
    int trigger_drop_thresh = 50; // percentage
    bool do_drop_unhot_op;
    clock_t cur_fast_start, cur_fast_end;
    // all_heats_table *allHeats = all_heats_table_init(0);
    // all_heats_table_locked_table *allHeats_locked = NULL;
    long temp_diff = 0;
    double total_migration_time = 0;
    double total_fast_time = 0.0;
    int total_fast_num = 0;
    int reset_all_temps = 1;
    last_demote_pages = kh_init(ptrset_dup);
    // int scan_stat = 0;
    // while (!terminate_flag_refchain)
    // {
    //     usleep(200000);
    // } // what??
    // enable_sigsegv_handler();
    while (!terminate_flag_refchain)
    {
    rollback_slow_scan:
        zero_hot_num = 0;
        // if (check_dram_free() == 0)
        // {
        //     break;
        // }
        // pthread_mutex_lock(&mutex);
        // while (allow_fast != 1)
        // {
        //     fprintf(stderr, "waiting fast to be signaled\n");
        //     pthread_cond_wait(&cond_fast, &mutex); // wait for cond_fast to be signaled
        // }
        // allow_slow = 0;
        cur_fast_num_hot = 0; // reset for every fast scan
        not_in_global_set = 0;
        if (fast_scan_idx == -1)
        {
            reset_all_temps = try_trigger_slow_scan();
            if (reset_all_temps == -1)
            {
                usleep(2000000);
                goto rollback_slow_scan;
            }
            fast_scan_idx = 0; // start fast scan
        }
        // do fast in here
        // PyGILState_STATE gstate4_safety = PyGILState_Ensure();
        // goto skip_fast_scans;
        cur_fast_start = clock();
        if (fast_scan_idx == 0)
        {
            if (reset_all_temps == 1)
            {
                for (int i = 0; i < old_num_op; i++)
                {
                    if (!check_in_global_helper((uintptr_t)all_temps[i].op))
                    {
                        not_in_global_set++;
                        continue; // for the sake of accessing op->hotness
                    }
                    // if (sigsetjmp(jump_buffer, 1) == 0)
                    // {
                    all_temps[i].prev_refcnt = all_temps[i].op->hotness;
                    // }
                }
            }
            else if (reset_all_temps == 2)
            {
                fprintf(stderr, "previous part need record, new part need only update prev_recnt\n");
                record_temp(fast_scan_idx, rescan_thresh, prev_num_op);
                for (int i = prev_num_op; i < old_num_op; i++)
                {
                    if (!check_in_global_helper((uintptr_t)all_temps[i].op))
                    {
                        not_in_global_set++;
                        continue;
                    }
                    // if (sigsetjmp(jump_buffer, 1) == 0)
                    // {
                    all_temps[i].prev_refcnt = all_temps[i].op->hotness;
                    // }
                }
            }
            else
            { // use old_num_op and record all
                record_temp(fast_scan_idx, rescan_thresh, old_num_op);
            }
        }
        else if (fast_scan_idx == 1)
        {
            if (reset_all_temps == 1 || reset_all_temps == 2)
            {
                for (int i = 0; i < old_num_op; i++)
                {
                    if (!check_in_global_helper((uintptr_t)all_temps[i].op))
                    {
                        not_in_global_set++;
                        continue;
                    }
                    // if (sigsetjmp(jump_buffer, 1) == 0)
                    // {
                    all_temps[i].diffs[1] = (abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt) > SHRT_MAX) ? -1 : (short)abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt);
                    if (all_temps[i].diffs[1] != 0)
                    { // update lower and upper bound
                        foundInner = (uintptr_t)all_temps[i].op;
                        if (foundInner > prev_changed_max)
                        {
                            prev_changed_max = foundInner;
                        }
                        else if (foundInner < prev_changed_min && foundInner > no_93_upper)
                        {
                            prev_changed_min = foundInner;
                        }
                        all_temps[i].diffs[rescan_thresh] += 1;
                        cur_fast_num_hot++;
                    }
                    else
                    {
                        zero_hot_num++;
                    }
                    all_temps[i].prev_refcnt = all_temps[i].op->hotness;
                    // }
                }
            }
            else
            { // 0
                record_temp(fast_scan_idx, rescan_thresh, old_num_op);
            }
        }
        else if (fast_scan_idx == 2)
        {
            if (reset_all_temps == 1 || reset_all_temps == 2)
            {
                for (int i = 0; i < old_num_op; i++)
                {
                    foundInner = (uintptr_t)all_temps[i].op;
                    if (foundInner > prev_changed_min && foundInner < prev_changed_max)
                    {
                        if (!check_in_global_helper((uintptr_t)all_temps[i].op))
                        {
                            not_in_global_set++;
                            continue;
                        }
                        // if (sigsetjmp(jump_buffer, 1) == 0)
                        // {
                        all_temps[i].diffs[2] = (abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt) > SHRT_MAX) ? -1 : (short)abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt);
                        all_temps[i].prev_refcnt = all_temps[i].op->hotness;
                        // calculate hotness
                        if (all_temps[i].diffs[2] != 0)
                        {
                            all_temps[i].diffs[rescan_thresh] += 1;
                            cur_fast_num_hot++;
                        }
                        else
                        {
                            zero_hot_num++;
                        }
                        if (all_temps[i].diffs[2] != all_temps[i].diffs[1] && all_temps[i].diffs[2] != 0 && all_temps[i].diffs[1] != 0)
                        {
                            all_temps[i].diffs[rescan_thresh] += 1;
                        }
                        // }
                    }
                    else
                    {
                        all_temps[i].diffs[rescan_thresh] |= (1 << DROP_OUT_OFF); // mark as 1 to drop out
                    }
                }
            }
            else
            { // 0: use old_num_op
                record_temp(fast_scan_idx, rescan_thresh, old_num_op);
            }
        }
        else if (fast_scan_idx != rescan_thresh) // shall never reach last index
        {
            record_temp(fast_scan_idx, rescan_thresh, old_num_op);
        }
        cur_fast_end = clock();
        fprintf(stderr, "cur_fast_num_hot: %ld, zero_hot_num: %d, not_in_global_set: %ld\n", cur_fast_num_hot, zero_hot_num, not_in_global_set);
        // PyGILState_Release(gstate4_safety);
        double cur_fast_time = ((double)(cur_fast_end - cur_fast_start)) / CLOCKS_PER_SEC;
        fprintf(stderr, "fast %d, cur_fast_time: %.3f\n", fast_scan_idx, cur_fast_time);
        total_fast_time += cur_fast_time;
        if (fast_scan_idx != -1)
            total_fast_num++;
        fprintf(stderr, "total_fast_num: %d, total_fast_time: %.3f\n", total_fast_num, total_fast_time);
        max_num_hot = max(max_num_hot, cur_fast_num_hot);
        if (fast_scan_idx != 0 || (fast_scan_idx == 0 && reset_all_temps != 1))
        {
            double deleted_in_all = (double)not_in_global_set / old_num_op;
            if (deleted_in_all > 0.9)
            {
                fprintf(stderr, "most op are deleted, rollback to slow\n");
                usleep(2000000);
                fast_scan_idx = -2; // will be incremented to -1 and go back to slow scan
            }
            else
            {
                cur_hot_in_all = (double)max_num_hot / old_num_op;
                fprintf(stderr, "cur_hot_in_all: %.3f\n", cur_hot_in_all);
                if (cur_hot_in_all < 0.01) // limited # changed op, OR, similar # hot, relax fast trace
                {
                    fprintf(stderr, "small # changed, rollback to slow...\n");
                    usleep(2000000);
                    fast_scan_idx = -2;
                }
                // else if (fabs(cur_hot_in_all - last_hot_in_all) < 0.01)
                // {
                //     fprintf(stderr, "similar # changed relaxing fast...\n");
                //     usleep(2000000);
                //     fast_scan_idx = -2;
                // }
            }
            last_hot_in_all = cur_hot_in_all;
        }
        if (very_first_bk)
            very_first_bk = false;

        if (++fast_scan_idx >= rescan_thresh)
        {
            // if (reset_all_temps)
            //     reset_all_temps = 0;
            fast_scan_idx = -1; // roll back to slow scan
            fprintf(stderr, "a bunch of fasts finished, try trigger migration...\n");

            // if (PyGILState_Check())
            // {
            //     fprintf(stderr, "GIL is held\n");
            // }
            // else
            // {
            //     fprintf(stderr, "GIL not held\n");
            // }
            // total_migration_time += try_trigger_migration(rescan_thresh);
            total_migration_time += try_trigger_migration_revised(rescan_thresh);
            max_num_hot = 0;
            if (doIO_)
            {
                fprintf(stderr, "flushing...\n");
                PyGILState_STATE gstate = PyGILState_Ensure();
                struct timespec ts;
                clock_gettime(CLOCK_MONOTONIC, &ts);
                for (int i = 0; i < old_num_op; i++)
                {
                    bool print_obj = false;
                    for (int j = 0; j < rescan_thresh; j++)
                    {
                        short cur_his = all_temps[i].diffs[j];
                        if (cur_his != 0)
                        {
                            print_obj = true;
                            break;
                        }
                        // fprintf(global_bookkeep_args->fd, "%hd\t", all_temps[i].diffs[j]);
                    }
                    uintptr_t found_inner = (uintptr_t)all_temps[i].op;
                    if (check_in_global_helper(found_inner))
                    // if (sigsetjmp(jump_buffer, 1) == 0)
                    {
                        fprintf(global_bookkeep_args->fd, "%ld.%ld\t%ld", ts.tv_sec, ts.tv_nsec, found_inner);
                        // fprintf(global_bookkeep_args->fd, "\t%s", Py_TYPE(found_inner)->tp_name);
                        for (int j = 0; j < rescan_thresh; j++)
                        {
                            fprintf(global_bookkeep_args->fd, "\t%hd", all_temps[i].diffs[j]);
                        }
                        // fprintf(global_bookkeep_args->fd, "\t");
                        // PyObject_Print(all_temps[i].op, global_bookkeep_args->fd, 0);
                        fprintf(global_bookkeep_args->fd, "\n");
                    }
                }
                fflush(global_bookkeep_args->fd);
                PyGILState_Release(gstate);
            }
        }
        // allow_slow = 1;
        // pthread_cond_signal(&cond_slow); // Signal fast scan, if any
        // pthread_mutex_unlock(&mutex);
    skip_fast_scans:
        usleep(global_bookkeep_args->sample_dur);
        // usleep(1000000);
    }
    terminate_flag_refchain = 0;
    enable_bk = 0;
    fprintf(stderr, "total_slow_num: %d, total_slow_time: %.3f, total_migration_time: %.3f\n", total_num_slow, total_cur_cascading_time, total_migration_time);
    free(all_temps);
    kh_destroy(ptrset_dup, last_demote_pages);
    free_global();
    // free_libcuckoo();
    // destroy_global_set_helper();
    // pthread_mutex_destroy(&mutex);
    // pthread_cond_destroy(&cond_slow);
    // pthread_cond_destroy(&cond_fast);
}

void *inspect_survived_objs(void *arg)
{
    enable_bk = 1;

    // BookkeepArgs *bookkeep_args = (BookkeepArgs *)arg;
    global_bookkeep_args = (BookkeepArgs *)arg;
    // if (!bookkeep_args->live_time_thresh_arg)
    // {
    //     get_live_time_thresh = 2500000; // default: 2.5s
    // }
    // else
    // {
    //     get_live_time_thresh = bookkeep_args->live_time_thresh_arg;
    // }
    // allow_slow = 1;
    // int rescan_thresh = bookkeep_args->rescan_thresh;
    // unsigned int doIO_ = bookkeep_args->doIO;
    // // PyGILState_STATE gstate;
    // // int rescan_thresh = bookkeep_args->rescan_thresh;
    // // then do fast scans
    // // TODO: need to maintain a lock/signal to inform when to start/stop slow scan, during fast cycles
    // int total_fast_num = 0;
    // Temperature *temp_ptr;
    // heat_node *node_ptr, *tmp;
    // int fast_scan_idx = 0;
    // int utlist_count;
    // uintptr_t foundInner;
    // uintptr_t prev_changed_max = 0;
    // uintptr_t prev_changed_min = ULONG_MAX;
    // uintptr_t no_93_upper = 100000000000000;
    // int trigger_drop_thresh = 50; // percentage
    // bool do_drop_unhot_op;
    // clock_t cur_fast_start, cur_fast_end;
    // all_heats_table *allHeats = all_heats_table_init(0);
    // all_heats_table_locked_table *allHeats_locked = NULL;
    ts_blob outter_key_wrapper;
    all_heats_table *allHeats = all_heats_table_init(0);
    struct timespec ts;
    while (!terminate_flag_refchain)
    {
        if (gen_idx != 1)
        {
            usleep(100000); // 0.1s
            continue;
        }
        // PyGILState_STATE gstate = PyGILState_Ensure();
        kv_init(local_ptr_vec);
        double cur_cascading_time = try_cascading_old(total_num_slow);
        // PyGILState_Release(gstate);
        // if (gen_idx == 2)
        {
            total_cur_cascading_time += cur_cascading_time;
            total_num_slow++;
        }
        // malloc temperatur metadata
        gen_temps();

        // clock_gettime(CLOCK_MONOTONIC, &ts);
        // outter_key_wrapper.cur_slow_idx = 0;
        // outter_key_wrapper.ts = ts;
        usleep(global_bookkeep_args->sample_dur);
        kv_destroy(local_ptr_vec);
    }
    // if (global_bookkeep_args->doIO)
    fprintf(stderr, "total_num_slow: %d, total_cur_cascading_time: %.3f\n", total_num_slow, total_cur_cascading_time);

    terminate_flag_refchain = 0;
    enable_bk = 0;
    // free_global(); // TODO: need to move to other place
}
