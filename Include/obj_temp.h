#ifndef OBM_TEMP_H
#define OBM_TEMP_H
#include <Python.h>
#include <stddef.h>
#include <stdint.h>
typedef struct
{
    PyObject *op;             // 8
    unsigned int prev_refcnt; // 4
    uint8_t diffs[8];         // 8
    // size_t cur_sizeof; // 8, but no need for now
} OBJ_TEMP;

#ifdef __cplusplus
extern "C"
{
#endif

    void cppDefaultSortAsc(OBJ_TEMP *all_temps, size_t n);
    void cppDefaultSortDesc(OBJ_TEMP *all_temps, size_t n);
    void cppParallelSort(OBJ_TEMP *all_temps, size_t n);
    void sortRawAddr(uintptr_t *ptr, size_t n);
    void record_temp_cpp(int scan_idx, int rescan_thresh, unsigned int num_total);
    // void register_sigsegv(void);
    // void enable_sigsegv_handler(void);

#ifdef __cplusplus
}
#endif

#endif /* OBM_TEMP_H */
