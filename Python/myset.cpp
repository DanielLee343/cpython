#include <unordered_set>
// #include <sparsehash/sparse_hash_set>
#include "../Include/myset.h"
#include <cstdint>
#include <cstdio>

static std::unordered_set<uintptr_t> dedup_set;
static std::unordered_set<uintptr_t> global_unordered_set;
static std::unordered_set<uintptr_t> collected_set;
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