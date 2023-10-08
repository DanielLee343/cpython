#ifndef PTR_SZIEOF_H
#define PTR_SZIEOF_H
#include <stdint.h>
// void insert(void *ptr, unsigned int value);
// unsigned int find(void *ptr);
#define CUCKOO_TABLE_NAME ptr_szidx_table
#define CUCKOO_KEY_TYPE uintptr_t
#define CUCKOO_MAPPED_TYPE size_t
#include <libcuckoo-c/cuckoo_table_template.h>

#endif // PTR_SZIEOF_H
