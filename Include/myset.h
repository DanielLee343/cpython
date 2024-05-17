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

    void reset_all_temps_func();

    // global_unordered_map
    void insert_into_map(uintptr_t key, bool val);
    int check_in_map(uintptr_t value);
    void erase_from_map(uintptr_t value);
    void free_map();
    void reset_all_temps_map();
    unsigned int get_map_size();

    // pages_loc_hotness
    // void insert_into_pages(uintptr_t page_addr, short hotness);
    void insert_into_pages(uintptr_t page_addr, short hotness, bool location);
    void insert_into_pages_only_exists(uintptr_t page_addr, short hotness);
    int check_in_pages(uintptr_t page);
    void erase_from_pages(uintptr_t page);
    void free_pages();
    unsigned int get_pages_size();
    // bool get_location_pages(void *page);
    // void set_location_pages(void *page, bool location);
    void populate_mig_pages(void **demote_pages, void **promote_pages, int *demo_size, int *promo_size, short split);
    void populate_mig_pages_wo_checking(void **demote_pages, void **promote_pages, int *demo_size, int *promo_size, short split);
    void get_page_hotness_bound(short *min, short *max);
    void print_all_pages_hotness();
    void reset_pages_hotness();
    void set_location_pages(uintptr_t page, bool location);

#ifdef __cplusplus
}
#endif

#endif // MYSET_H
