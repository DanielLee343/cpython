#ifndef OBM_TEMP_H
#define OBM_TEMP_H
#include <Python.h>
#include <stddef.h>
#include <stdint.h>

#define PAGE_SIZE_ 4096
#define PAGE_MASK_ (~(PAGE_SIZE_ - 1))
#define NUM_SLOTS 8
typedef struct
{
    PyObject *op;             // 8
    unsigned int prev_refcnt; // 4
    uint8_t diffs[NUM_SLOTS]; // 8
    // uint8_t diff; // 1 ( + 3)
} OBJ_TEMP;       // total 12

#ifdef __cplusplus
extern "C"
{
#endif

    void cppDefaultSortAsc(OBJ_TEMP *all_temps, size_t n);
    void cppDefaultSortDesc(OBJ_TEMP *all_temps, size_t n);
    void sortRawAddr(uintptr_t *ptr, size_t n);
    void sortRawAddr_masked(uintptr_t *ptr, size_t n);
    // void register_sigsegv(void);
    // void enable_sigsegv_handler(void);

#ifdef __cplusplus
}
#endif

#endif /* OBM_TEMP_H */
