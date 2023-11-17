#ifndef CUR_HEATS_H
#define CUR_HEATS_H
#include <stdint.h>
typedef struct
{
    intptr_t prev_refcnt;
    // intptr_t most_recent_refcnt;
    intptr_t diff;
    unsigned int cur_sizeof;
} Temperature;

#define CUCKOO_TABLE_NAME cur_heats_table
#define CUCKOO_KEY_TYPE uintptr_t
#define CUCKOO_MAPPED_TYPE Temperature
#include <libcuckoo-c/cuckoo_table_template.h>

#endif // CUR_HEATS_H
