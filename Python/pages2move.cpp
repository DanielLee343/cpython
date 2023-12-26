#include <unordered_set>
#include "../Include/pages2move.h"

static std::unordered_set<uintptr_t> pages2move;
// to record page boundaries

extern "C" void insert_pages2move(uintptr_t value)
{
    pages2move.insert(value);
}

extern "C" int check_pages2move(uintptr_t value)
{ // returns true if found
    return pages2move.find(value) != pages2move.end();
}

extern "C" void free_pages2move()
{
    pages2move.clear();
}