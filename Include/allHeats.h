#ifndef ALL_HEATS_H
#define ALL_HEATS_H
#include <time.h>
#include <stdint.h>
typedef struct { struct timespec ts; } ts_blob;
// typedef struct { char blob[255]; } mapped_blob;

#define CUCKOO_TABLE_NAME all_heats_table
#define CUCKOO_KEY_TYPE ts_blob
#define CUCKOO_MAPPED_TYPE uintptr_t
#include <libcuckoo-c/cuckoo_table_template.h>

#endif // ALL_HEATS_H

// #undef CUCKOO_TABLE_NAME
// #undef CUCKOO_KEY_TYPE
// #undef CUCKOO_MAPPED_TYPE

// typedef struct
// {
//     long diff;
//     unsigned int cur_sizeof;
// } Temperature;

// #define CUCKOO_TABLE_NAME cur_heats_table
// #define CUCKOO_KEY_TYPE void *
// #define CUCKOO_MAPPED_TYPE Temperature
// #include <libcuckoo-c/cuckoo_table_template.h>