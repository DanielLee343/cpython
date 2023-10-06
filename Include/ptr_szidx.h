#ifndef CUCKOO_WRAPPER_H
#define CUCKOO_WRAPPER_H

// void insert(void *ptr, unsigned int value);
// unsigned int find(void *ptr);
#define CUCKOO_TABLE_NAME ptr_szidx_table
#define CUCKOO_KEY_TYPE void *
#define CUCKOO_MAPPED_TYPE unsigned int
#include <libcuckoo-c/cuckoo_table_template.h>

#endif // CUCKOO_WRAPPER_H
