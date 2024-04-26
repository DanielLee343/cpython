#pragma once
#ifndef MYSET_H
#define MYSET_H
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif
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

    void insert_into_collected(uintptr_t value);
    int check_in_collected(uintptr_t value);

    void reset_all_temps();

    // libcuckoo
    // void insert_into_libcuckoo(uintptr_t value);
    // int check_in_libcuckoo(uintptr_t value);
    // void erase_from_libcuckoo(uintptr_t value);
    // void free_libcuckoo();
    // unsigned int get_libcuckoo_size();

    // global_unordered_map
    void insert_into_map(uintptr_t key, bool val);
    int check_in_map(uintptr_t value);
    void erase_from_map(uintptr_t value);
    void free_map();
    void reset_all_temps_map();
    unsigned int get_map_size();

#ifdef __cplusplus
}
#endif

#endif // MYSET_H
