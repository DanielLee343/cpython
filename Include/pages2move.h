#pragma once
#ifndef PAGES2MOVE_H
#define PAGES2MOVE_H
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

    void insert_pages2move(uintptr_t value);
    int check_pages2move(uintptr_t value);
    void free_pages2move();

#ifdef __cplusplus
}
#endif

#endif // PAGES2MOVE_H
