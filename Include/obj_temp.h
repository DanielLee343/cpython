#include <Python.h>
// #include "object.h"
#include <stddef.h>
typedef struct
{
    PyObject *op;      // 8
    long prev_refcnt;  // 8
    short diffs[20];   // 40
    size_t cur_sizeof; // 8
} OBJ_TEMP;

#ifndef OBM_TEMP_H
#define OBM_TEMP_H
#ifdef __cplusplus
extern "C"
{
#endif

    void cppDefaultSortAsc(OBJ_TEMP *all_temps, size_t n);
    void cppDefaultSortDesc(OBJ_TEMP *all_temps, size_t n);
    void cppParallelSort(OBJ_TEMP *all_temps, size_t n);
    void sortRawAddr(uintptr_t *ptr, size_t n);

#ifdef __cplusplus
}
#endif

#endif /* OBM_TEMP_H */
