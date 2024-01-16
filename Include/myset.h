#pragma once
#ifndef MYSET_H
#define MYSET_H
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

    void insert_into_set(uintptr_t value);
    int check_in_set(uintptr_t value);
    void free_set();
    int get_set_size();

#ifdef __cplusplus
}
#endif

#endif // MYSET_H
