#pragma once
#ifndef MYSET_H
#define MYSET_H
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif
    // dedup_set
    void insert_into_set(uintptr_t value);
    int check_in_set(uintptr_t value);
    void free_set();
    unsigned int get_set_size();
    void print_addr(FILE *fd, int round);
    // global_unordered_set
    void insert_into_global(uintptr_t value);
    int check_in_global(uintptr_t value);
    void free_global();
    unsigned int get_global_size();
    void erase_from_global(uintptr_t value);
    void print_global_addr(FILE *fd, int round);
    void insert_into_collected(uintptr_t value);
    int check_in_collected(uintptr_t value);

#ifdef __cplusplus
}
#endif

#endif // MYSET_H
