#pragma once
#ifndef CUR_HEATS_H
#define CUR_HEATS_H
#include <stdint.h>

#define NUM_FASTS 23 // TODO: how to align this
typedef struct
{ // Consider bit-wise optimization here to save memory? Since diff is not gonna be very big
    intptr_t prev_refcnt;
    // intptr_t most_recent_refcnt;
    // intptr_t diff_5; // most recent fast diff
    // intptr_t diff_4;
    // intptr_t diff_3;
    // intptr_t diff_2;
    // intptr_t diff_1; // 1st fast diff
    intptr_t diffs[NUM_FASTS];
    unsigned long cur_sizeof;
} Temperature;

#define CUCKOO_TABLE_NAME cur_heats_table
#define CUCKOO_KEY_TYPE uintptr_t
#define CUCKOO_MAPPED_TYPE Temperature
#include <libcuckoo-c/cuckoo_table_template.h>

#endif // CUR_HEATS_H
