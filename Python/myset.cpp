#include <unordered_set>
#include "../Include/myset.h"

static std::unordered_set<int> mySet;

extern "C" void insert_into_set(uintptr_t value)
{
    mySet.insert(value);
}

extern "C" int check_in_set(uintptr_t value)
{ // returns true if found
    return mySet.find(value) != mySet.end();
}

extern "C" void free_set()
{
    mySet.clear();
}
