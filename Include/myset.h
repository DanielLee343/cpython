#pragma once
#ifndef MYSET_H
#define MYSET_H
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

    void insert_into_set(uintptr_t value);
    int check_in_set(uintptr_t value);
    void free_set();
    unsigned int get_set_size();
    void print_addr(FILE *fd, int round);

#ifdef __cplusplus
}
#endif

#endif // MYSET_H
