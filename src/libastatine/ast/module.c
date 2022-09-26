#include "module.h"
#include "memory/arena.h"

#include <stdlib.h>

#define OR(a, b) ((a) ? (a) : (b))

Module_T* init_module(const char* name, const char* origin)
{
    Module_T* module = calloc(1, sizeof(Module_T));
    module->name = name;
    module->origin = OR(origin, "(null)");
    return module;
}

void free_module(Module_T* module)
{
    arena_destroy(&module->memory);
    free(module);
}
