#include "../Include/obj_temp.h"
#include <algorithm> // for std::sort
#include <cstddef>   // for size_t

// for parallel sort
#include <execution>

extern int rescan_thresh_glb;

extern "C" void cppDefaultSortAsc(OBJ_TEMP *all_temps, size_t n)
{
    std::sort(all_temps, all_temps + n, [](const OBJ_TEMP &a, const OBJ_TEMP &b)
              {
                  return (b.diffs[rescan_thresh_glb] & 0x3FFF) > (a.diffs[rescan_thresh_glb] & 0x3FFF); // Sort asc
              });
}

extern "C" void cppDefaultSortDesc(OBJ_TEMP *all_temps, size_t n)
{
    std::sort(all_temps, all_temps + n, [](const OBJ_TEMP &a, const OBJ_TEMP &b)
              {
                  return (b.diffs[rescan_thresh_glb] & 0x3FFF) < (a.diffs[rescan_thresh_glb] & 0x3FFF); // Sort desc
              });
}

// extern "C" void cppTopKSort(OBJ_TEMP *all_temps, size_t n)
// {
//     std::sort(all_temps, all_temps + n, [](const OBJ_TEMP &a, const OBJ_TEMP &b)
//               {
//                   return (b.diffs[rescan_thresh_glb] & 0x3FFF) < (a.diffs[rescan_thresh_glb] & 0x3FFF); // Sort desc, only top k
//               });
// }

extern "C" void cppParallelSort(OBJ_TEMP *all_temps, size_t n)
{
    std::sort(
        std::execution::par, all_temps, all_temps + n, [](const OBJ_TEMP &a, const OBJ_TEMP &b)
        { return (b.diffs[rescan_thresh_glb] & 0x3FFF) < (a.diffs[rescan_thresh_glb] & 0x3FFF); });
}

// sort the addr in ascending order
extern "C" void sortRawAddr(uintptr_t *ptr, size_t n)
{
    std::sort(ptr, ptr + n, [](const uintptr_t &a, const uintptr_t &b)
              {
                  return a < b; // Sort asc
              });
}