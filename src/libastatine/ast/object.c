#include "object.h"

Object_T* init_object(Module_T* module, Token_T* token)
{
    Object_T* obj = arena_calloc(&module->memory, 1, sizeof(Object_T));
    obj->token = token;
    return obj;
}
