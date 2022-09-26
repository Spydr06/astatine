#ifndef ASTATINE_MODULE_H
#define ASTATINE_MODULE_H

#include "memory/arena.h"

typedef struct MODULE_STRUCT {
    const char* name;
    const char* origin;

    Arena_T memory;
} Module_T;

Module_T* init_module(const char* name, const char* origin);
void free_module(Module_T* module);

#endif /* ASTATINE_MODULE_H */
