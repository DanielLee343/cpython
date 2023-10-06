extern "C" {
// #include "../Include/ptr_szidx.h"
#include "Python.h"

}

// #include <libcuckoo/cuckoohash_map.hh>

// libcuckoo::cuckoohash_map<void *, unsigned int> ptr_szidx_pair;
// typedef libcuckoo::cuckoohash_map<void *, int> CurTimeObjHeat;
// typedef libcuckoo::cuckoohash_map<struct timespec, void *> RefTrackHeatmapHash;

// static RefTrackHeatmapHash outter_table;
// extern "C" {
    

//     void insert(void *ptr, unsigned int value) {
//         ptr_szidx_pair.insert(ptr, value);
//     }
//     unsigned int find(void *ptr) {
//         unsigned int value;
//         if (ptr_szidx_pair.find(ptr, value)) {
//             return value;
//         }
//         return -1;
//     }
//     void insert_outter(struct timespec ts, void *newCurTime) {
//         outter_table.insert(ts, newCurTime);
//     }
// }

#include <libcuckoo-c/cuckoo_table_template.cc>