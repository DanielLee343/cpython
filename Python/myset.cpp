#include <unordered_set>
#include <unordered_map>
// #include <sparsehash/sparse_hash_set>
#include "../Include/myset.h"
#include "../Include/obj_temp.h"
#include <cstdint>
#include <cstdio>
#include <mutex>
#include <Python.h>
#include <libcuckoo/cuckoohash_map.hh>
std::mutex global_set_mutex;
static std::unordered_set<uintptr_t> global_unordered_set;
static std::unordered_map<uintptr_t, bool> global_unordered_map;
extern OBJ_TEMP *all_temps;
extern unsigned int old_num_op;
// typedef google::sparse_hash_set<int> MyHashSet;
libcuckoo::cuckoohash_map<uintptr_t, int> libcuckoo_table;

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

extern "C" void reset_all_temps()
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

// libcuckoo hash table
// extern "C" void insert_into_libcuckoo(uintptr_t value)
// {
//     libcuckoo_table.insert(value, 0);
// }
// extern "C" int check_in_libcuckoo(uintptr_t value)
// {
//     int dummy;
//     return libcuckoo_table.find(value, dummy);
// }
// extern "C" void erase_from_libcuckoo(uintptr_t value)
// {
//     libcuckoo_table.erase(value);
// }
// extern "C" void free_libcuckoo()
// {
//     libcuckoo_table.clear();
// }

// extern "C" unsigned int get_libcuckoo_size()
// {
//     return libcuckoo_table.size();
// }

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