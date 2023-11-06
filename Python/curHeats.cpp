extern "C" {
#include "../Include/curHeats.h"
// #include "Python.h"
}
// #include <cstring>
// #include <functional>

// namespace std {
// template <> struct hash<Temperature> {
//   size_t operator()(const Temperature &kb) const { return *(size_t *)kb.ts; }
// };
// template <> struct equal_to<Temperature> {
//   bool operator()(const Temperature &lhs, const Temperature &rhs) const {
//     if(lhs.diff == rhs.diff && )
//     return true;
//   }
// };
// }
#include <libcuckoo-c/cuckoo_table_template.cc>