#include "module.h"
#include "memory/arena.h"
#include "memory/list.h"

#include <stdlib.h>

#define OR(a, b) ((a) ? (a) : (b))

Module_T* init_module(const char* name, const char* origin)
{
    Module_T* module = calloc(1, sizeof(Module_T));
    module->name = name;
    module->origin = OR(origin, "(null)");
    module->objects = init_list();
    return module;
}

void free_module(Module_T* module)
{
    free_list(module->objects);
    arena_destroy(&module->memory);
    free(module);
}
