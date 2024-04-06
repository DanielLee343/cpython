#include <unordered_set>
#include <unordered_map>
// #include <sparsehash/sparse_hash_set>
#include "../Include/myset.h"
#include "../Include/obj_temp.h"
#include <cstdint>
#include <cstdio>

static std::unordered_set<uintptr_t> dedup_set;
static std::unordered_set<uintptr_t> global_unordered_set;
static std::unordered_set<uintptr_t> collected_set;
static std::unordered_map<uintptr_t, OBJ_TEMP> mp;
// typedef google::sparse_hash_set<int> MyHashSet;
// static MyHashSet sparse_set;
// dedup_set
extern "C" void insert_into_set(uintptr_t value)
{
    dedup_set.insert(value);
}

extern "C" int check_in_set(uintptr_t value)
{
    return dedup_set.find(value) != dedup_set.end();
}

extern "C" void free_set()
{
    dedup_set.clear();
}

extern "C" unsigned int get_set_size()
{
    return dedup_set.size();
}

extern "C" void print_addr(FILE *fd, int round)
{
    for (auto it = dedup_set.begin(); it != dedup_set.end(); ++it)
    {
        fprintf(fd, "%ld\t%d\n", *it, round);
    }
    fflush(fd);
}

// global_unordered_set

extern "C" void insert_into_global(uintptr_t value)
{
    global_unordered_set.insert(value);
}

extern "C" int check_in_global(uintptr_t value)
{
    return global_unordered_set.find(value) != dedup_set.end();
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

extern "C" void print_global_addr(FILE *fd, int round)
{
    for (auto it = global_unordered_set.begin(); it != global_unordered_set.end(); ++it)
    {
        fprintf(fd, "%d\t%ld\n", round, *it);
    }
}
// collected_set

extern "C" void insert_into_collected(uintptr_t value)
{
    collected_set.insert(value);
}

extern "C" int check_in_collected(uintptr_t value)
{
    return collected_set.find(value) != collected_set.end();
}
// add to unordered_map
extern "C" void try_add(uintptr_t op)
{
    auto it = mp.find(op);
    if (it == mp.end())
    {
        OBJ_TEMP temp = {0};
        temp.prev_refcnt = 1;
        temp.cur_sizeof = 0;
        // temp.refcnt_keep_increasing = 0;
        // Use move to move the stack-allocated OBJ_TEMP into the map
        mp.emplace(op, std::move(temp));
    }
    else
    {
        // it->second.refcnt_keep_increasing += 1;
    }
}
extern "C" void try_delete(uintptr_t op)
{
    mp.erase(op);
}