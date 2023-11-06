extern "C" {
#include "../Include/allHeats.h"
// #include "Python.h"
}
#include <cstring>
#include <functional>

namespace std {
template <> struct hash<ts_blob> {
//   size_t operator()(const ts_blob &kb) const { printf("---%zu---\n", *(size_t *)kb.ts); return *(size_t *)kb.ts; }
  size_t operator()(const ts_blob &kb) const { 
    // printf("---%zu---\n", (size_t)kb.ts.tv_nsec); 
    return (size_t)kb.ts.tv_nsec; }
//   uintptr_t operator()(const ts_blob &kb) const { return (uintptr_t)kb.ts; }
};
template <> struct equal_to<ts_blob> {
  bool operator()(const ts_blob &lhs, const ts_blob &rhs) const {
    // return memcmp(lhs.ts, rhs.ts, sizeof(lhs.ts)) == 0;
    // return (lhs.ts->tv_nsec == rhs.ts->tv_nsec) && (lhs.ts->tv_sec == rhs.ts->tv_sec);
    return (lhs.ts.tv_nsec == rhs.ts.tv_nsec) && (lhs.ts.tv_sec == rhs.ts.tv_sec);
    // return &()
  }
};
}
#include <libcuckoo-c/cuckoo_table_template.cc>