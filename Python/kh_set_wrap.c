#include "../kh_set_wrap.h"

#if INTPTR_MAX == INT64_MAX
KHASH_SET_INIT_INT64(ptrset)
#elif INTPTR_MAX == INT32_MAX
KHASH_SET_INIT_INT32(ptrset)
#else
#error "Unsupported pointer size"
#endif

khash_t(ptrset) * global_op_set;
void init_global_set_helper()
{
    global_op_set = kh_init(ptrset);
}

void destroy_global_set_helper()
{
    kh_destroy(ptrset, global_op_set);
}
void insert_global_set_helper(PyObject *op)
{
    int ret;
    kh_put(ptrset, global_op_set, op, &ret);
}

int found_in_kset_helper(PyObject *op)
{
    khint_t k = kh_get(ptrset, global_op_set, op);
    return k != kh_end(global_op_set);
}

void erase_item_global_set_helper(PyObject *op)
{
    khint_t k;
    k = kh_get(ptrset, global_op_set, op);
    if (k != kh_end(global_op_set))
    {
        kh_del(ptrset, global_op_set, k);
    }
}

unsigned int get_global_kh_size()
{
    return kh_size(global_op_set);
}