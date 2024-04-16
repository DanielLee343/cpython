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
    int check_in_global_safe(uintptr_t value);
    void free_global();
    int valid_global_set();
    unsigned int get_global_size();
    void erase_from_global(uintptr_t value);
    void erase_from_global_safe(uintptr_t value);
    void print_global_addr(FILE *fd, int round);
    void pop_all_temps();

    void insert_into_collected(uintptr_t value);
    int check_in_collected(uintptr_t value);
    void try_add(uintptr_t op);
    void try_delete(uintptr_t op);

#ifdef __cplusplus
}
#endif

#endif // MYSET_H
