#include <unordered_set>
// #include <sparsehash/sparse_hash_set>
#include "../Include/myset.h"
#include <cstdint>
#include <cstdio>

static std::unordered_set<uintptr_t> dedup_set;
static std::unordered_set<uintptr_t> global_unordered_set;
// typedef google::sparse_hash_set<int> MyHashSet;
// static MyHashSet sparse_set;

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
