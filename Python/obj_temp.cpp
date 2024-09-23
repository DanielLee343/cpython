#include "../Include/obj_temp.h"
#include <algorithm> // for std::sort
#include <cstddef>   // for size_t
#include <cstdlib>
#include <stdexcept>
#include <csignal>
#include <setjmp.h>

OBJ_TEMP *all_temps = NULL;

extern "C" void cppDefaultSortAsc(OBJ_TEMP *all_temps, size_t n)
{
    std::sort(all_temps, all_temps + n, [](const OBJ_TEMP &a, const OBJ_TEMP &b)
              {
                  //   return (b.diff & 0x7F) > (a.diff & 0x7F); // Sort asc
                  return (b.diffs[NUM_SLOTS - 1] & 0x7F) > (a.diffs[NUM_SLOTS - 1] & 0x7F); // Sort asc
              });
}

extern "C" void cppDefaultSortDesc(OBJ_TEMP *all_temps, size_t n)
{
    std::sort(all_temps, all_temps + n, [](const OBJ_TEMP &a, const OBJ_TEMP &b)
              {
                  //   return (b.diff & 0x7F) < (a.diff & 0x7F); // Sort desc
                  return (b.diffs[NUM_SLOTS - 1] & 0x7F) < (a.diffs[NUM_SLOTS - 1] & 0x7F); // Sort desc
              });
}

// sort the addr in ascending order
extern "C" void sortRawAddr(uintptr_t *ptr, size_t n)
{
    std::sort(ptr, ptr + n, [](const uintptr_t &a, const uintptr_t &b)
              {
                  return a < b; // Sort asc
              });
}

extern "C" void sortRawAddr_masked(uintptr_t *ptr, size_t n)
{
    std::sort(ptr, ptr + n, [](const uintptr_t &a, const uintptr_t &b)
              {
                  return (a & PAGE_MASK_) < (b & PAGE_MASK_); // Sort by page boundaries
              });
}
