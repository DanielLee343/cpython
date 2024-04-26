#include "../Include/obj_temp.h"
#include <algorithm> // for std::sort
#include <cstddef>   // for size_t
#include <cstdlib>
#include <stdexcept>
#include <csignal>
// for parallel sort
#include <execution>
#include <setjmp.h>

extern int rescan_thresh_glb;
OBJ_TEMP *all_temps = NULL;

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


// void sigsegv_handler_cpp(int sig)
// {
//     fprintf(stderr, "Caught SIGSEGV: segmentation fault\n");
//     _exit(0);
// }

// extern "C" void register_sigsegv()
// {
//     signal(SIGSEGV, sigsegv_handler_cpp);
// }

// extern "C" void record_temp_cpp(int scan_idx, int rescan_thresh, unsigned int num_total)
// {
//     unsigned int skipped = 0;
//     int prev_scan_idx = (scan_idx == 0) ? (rescan_thresh - 1) : (scan_idx - 1);
//     for (int i = 0; i < num_total; i++)
//     {
//         // if (all_temps[i].diffs[rescan_thresh] & (1 << DROP_OUT_OFF)) // falls out of sampling range, skip
//         // {
//         //     skipped++;
//         // }
//         // else
//         // if (!check_in_global_helper((uintptr_t)all_temps[i].op))
//         // {
//         //     not_in_global_set++;
//         //     continue;
//         // }
//         // all_temps[i].diffs[scan_idx] = (abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt) > SHRT_MAX) ? -1 : (short)abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt);
//         try
//         {
//             all_temps[i].diffs[scan_idx] = static_cast<short>(all_temps[i].op->hotness);
//             // all_temps[i].diffs[scan_idx] = (std::abs(static_cast<long int>(all_temps[i].op->hotness - all_temps[i].prev_refcnt)) > SHRT_MAX) ? -1 : static_cast<short>(std::abs(static_cast<long int>(all_temps[i].op->hotness - all_temps[i].prev_refcnt)));
//             all_temps[i].prev_refcnt = all_temps[i].op->hotness;
//         }
//         catch (const std::exception &e)
//         {
//             fprintf(stderr, "Error1: %s\n", e.what());
//         }
//         catch (...)
//         {
//             fprintf(stderr, "Error2\n");
//         }
//         // all_temps[i].diffs[scan_idx] = (short)abs(all_temps[i].op->hotness - all_temps[i].prev_refcnt);
//         if (all_temps[i].diffs[scan_idx] != 0)
//         {
//             all_temps[i].diffs[rescan_thresh] += 1;
//             // cur_fast_num_hot++;
//         }
//         // else
//         // {
//         //     zero_hot_num++;
//         // }
//         if (all_temps[i].diffs[scan_idx] != all_temps[i].diffs[prev_scan_idx] && all_temps[i].diffs[prev_scan_idx] != 0 && all_temps[i].diffs[scan_idx] != 0) // TODO: for better cache hit, we can check diffs[scan_idx+1]
//         {
//             all_temps[i].diffs[rescan_thresh] += 1;
//         }
//     }
// }