#include <unordered_set>
#include <unordered_map>
#include <tuple>
#include "../Include/myset.h"
#include "../Include/obj_temp.h"
#include <cstdint>
#include <cstdio>
#include <mutex>
#include <Python.h>
// #include <numa.h>
// #include <numaif.h>
// numa_set_preferred(0);
std::mutex global_set_mutex;
static std::unordered_set<uintptr_t> global_unordered_set;
static std::unordered_map<uintptr_t, bool> global_unordered_map;
static std::unordered_map<void *, int> pages_loc_hotness;
static std::unordered_map<uintptr_t, std::pair<short, bool>> map_pair;
extern OBJ_TEMP *all_temps;
extern unsigned int old_num_op;

// global_unordered_set
extern "C" void insert_into_global(uintptr_t value)
{
    global_unordered_set.insert(value);
}

extern "C" int check_in_global(uintptr_t value)
{
    return global_unordered_set.find(value) != global_unordered_set.end();
}

extern "C" int check_in_global_safe(uintptr_t value)
{
    std::lock_guard<std::mutex> guard(global_set_mutex);
    return global_unordered_set.find(value) != global_unordered_set.end();
}

extern "C" void free_global()
{
    global_unordered_set.clear();
}

extern "C" unsigned int get_global_size()
{
    return global_unordered_set.size();
}

extern "C" void erase_from_global(uintptr_t value)
{
    global_unordered_set.erase(value);
}

extern "C" void erase_from_global_safe(uintptr_t value)
{
    std::lock_guard<std::mutex> guard(global_set_mutex);
    global_unordered_set.erase(value);
}

extern "C" void print_global_addr(FILE *fd, int round)
{
    for (auto it = global_unordered_set.begin(); it != global_unordered_set.end(); ++it)
    {
        fprintf(fd, "%d\t%ld\n", round, *it);
    }
}
extern "C" int valid_global_set()
{
    return (&global_unordered_set != nullptr);
}

extern "C" void reset_all_temps_func()
{
    free(all_temps);
    unsigned int new_num_op = global_unordered_set.size();
    all_temps = (OBJ_TEMP *)calloc(new_num_op, sizeof(OBJ_TEMP));
    unsigned int i = 0;
    for (auto it = global_unordered_set.begin(); it != global_unordered_set.end(); ++it)
    {
        all_temps[i].op = (PyObject *)*it;
        all_temps[i].prev_refcnt = 0;
        i++;
    }
    old_num_op = new_num_op;
}

// global_unordered_map
extern "C" void insert_into_map(uintptr_t value, bool val)
{
    global_unordered_map[value] = val;
}

extern "C" int check_in_map(uintptr_t value)
{
    return global_unordered_map.find(value) != global_unordered_map.end();
}

extern "C" void erase_from_map(uintptr_t value)
{
    global_unordered_map.erase(value);
}

extern "C" void free_map()
{
    global_unordered_map.clear();
}

extern "C" unsigned int get_map_size()
{
    return global_unordered_map.size();
}

extern "C" void reset_all_temps_map()
{
    free(all_temps);
    unsigned int new_num_op = global_unordered_map.size();
    all_temps = (OBJ_TEMP *)calloc(new_num_op, sizeof(OBJ_TEMP));
    unsigned int i = 0;
    for (auto it = global_unordered_map.begin(); it != global_unordered_map.end(); ++it)
    {
        all_temps[i].op = (PyObject *)(it->first);
        all_temps[i].prev_refcnt = 0;
        if (it->second)
            all_temps[i].diffs[7] |= (1 << 7);
        else
            all_temps[i].diffs[7] &= ~(1 << 7);
        i++;
    }
    old_num_op = new_num_op;
}
// pages_loc_hotness
// map_pair
extern "C" void insert_into_pages(uintptr_t page_addr, short hotness, bool location)
{
    auto it = map_pair.find(page_addr);
    if (it != map_pair.end())
    {
        // if (is_hot)
        it->second.first += hotness;
    }
    else
    {
        // map_pair.emplace(page_addr, std::make_pair(is_hot ? hotness : 0, false));
        // map_pair[page_addr] = std::make_pair(is_hot ? hotness : 0, false);
        map_pair[page_addr] = std::make_pair(hotness, location); // false (0): DRAM, true (1): CXL
    }
}
extern "C" void insert_into_pages_only_exists(uintptr_t page_addr, short hotness)
{
    auto it = map_pair.find(page_addr);
    if (it != map_pair.end())
    {
        it->second.first += hotness;
    }
}

// extern "C" void set_location_pages(void *page, bool location)
// {
//     if (location)
//         pages_loc_hotness[page] |= (1 << 14); // set to 1 --> CXL
//     else
//         pages_loc_hotness[page] &= ~(1 << 14); // set to 0 --> DRAM
// }

// extern "C" bool get_location_pages(void *page)
// {
//     return pages_loc_hotness[page] & (1 << 14);
// }

extern "C" int check_in_pages(uintptr_t page)
{
    // return pages_loc_hotness.find(page) != pages_loc_hotness.end();
    return map_pair.find(page) != map_pair.end();
}

extern "C" void erase_from_pages(uintptr_t page)
{
    // pages_loc_hotness.erase(page);
    map_pair.erase(page);
}

extern "C" void free_pages()
{
    // pages_loc_hotness.clear();
    map_pair.clear();
}

extern "C" unsigned int get_pages_size()
{
    // return pages_loc_hotness.size();
    return map_pair.size();
}

int process_signed_value(int input)
{
    // Mask out the 2nd MSB and the remaining bits except the LSB 14
    int mask = 0x3FFFFFFF; // 0011 1111 1111 1111 in binary
    int processed_value = input & mask;

    // If MSB is set (negative number), set MSB in processed value
    if (input & 0x80000000)
    {                                  // 1000 0000 0000 0000 in binary
        processed_value |= 0x80000000; // 1000 0000 0000 0000 in binary
    }

    return processed_value;
}

bool getSecondMSB(int num)
{
    return (num & (1 << 30)) >> 30; // Isolate the 2nd MSB and normalize it to 0 or 1
}

extern "C" void populate_mig_pages(void **demote_pages, void **promote_pages, int *demo_size, int *promo_size, short split)
{
    for (auto it = map_pair.begin(); it != map_pair.end(); ++it)
    {
        // int processed = process_signed_value(it->second);
        if (it->second.first < split && !it->second.second)
        {
            *demote_pages = (void *)it->first; // Store the pointer at the memory location
            demote_pages++;                    // Move the pointer to the next memory location
            (*demo_size)++;                    // Increment the value stored at the memory location
            // it->second.second = 1;
        }
        else if (it->second.first >= split && it->second.second)
        {
            *promote_pages = (void *)it->first; // Store the pointer at the memory location
            promote_pages++;                    // Move the pointer to the next memory location
            (*promo_size)++;                    // Increment the value stored at the memory location
            // it->second.second = 0;
        }
    }
}

extern "C" void populate_mig_pages_wo_checking(void **demote_pages, void **promote_pages, int *demo_size, int *promo_size, short split)
{
    for (auto it = map_pair.begin(); it != map_pair.end(); ++it)
    {
        // int processed = process_signed_value(it->second);
        if (it->second.first < split) // Most sig bit: 1(negative) cold dominant && in DRAM
        {
            *demote_pages = (void *)it->first; // Store the pointer at the memory location
            demote_pages++;                    // Move the pointer to the next memory location
            (*demo_size)++;                    // Increment the value stored at the memory location
            // it->second |= (1 << 30);   // mark in CXL
            // it->second.second = 1;
        }
        else if (it->second.first >= split) // Most sig bit: 0(positive) hot dominant && in CXL
        {
            *promote_pages = (void *)it->first; // Store the pointer at the memory location
            promote_pages++;                    // Move the pointer to the next memory location
            (*promo_size)++;                    // Increment the value stored at the memory location
            // it->second &= ~(1 << 30);   // mark in DRAM
            // it->second.second = 0;
        }
    }
}

extern "C" void get_page_hotness_bound(short *min_val, short *max_val)
{
    short min = SHRT_MAX;
    short max = SHRT_MIN;

    // for (const auto &it : pages_loc_hotness)
    for (const auto &it : map_pair)
    {
        // int processed = process_signed_value(it.second);
        auto value = it.second;
        if (it.second.first < min)
        {
            min = it.second.first;
        }
        if (it.second.first > max)
        {
            max = it.second.first;
        }
    }
    fprintf(stderr, "min: %hd, max: %hd\n", min, max);
    *min_val = min;
    *max_val = max;
}
extern "C" void print_all_pages_hotness()
{
    for (auto it = map_pair.begin(); it != map_pair.end(); ++it)
    {
        fprintf(stderr, "page: %p, hotness: %hd\n", it->first, it->second.first);
    }
    fflush(stderr);
}

extern "C" void reset_pages_hotness()
{
    fprintf(stderr, "resetting pages hotness\n");
    for (auto it = map_pair.begin(); it != map_pair.end(); ++it)
    {
        it->second.first = 0;
    }
}

extern "C" void set_location_pages(uintptr_t page, bool location)
{
    auto it = map_pair.find(page);
    if (it != map_pair.end())
    {
        if (location)
            it->second.second = 1;
        else
            it->second.second = 0;
        // clear hotness
        it->second.first = 0;
    }
}