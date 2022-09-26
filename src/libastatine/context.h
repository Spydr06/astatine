#ifndef ASTATINE_CONTEXT_H
#define ASTATINE_CONTEXT_H

#include <astatine.h>

#include "error/exception.h"
#include "memory/hashmap.h"
#include "memory/arena.h"

typedef struct ASTATINE_CONTEXT_STRUCT Context_T;

// Compile context struct, stores global variables, compiler state, etc.
struct ASTATINE_CONTEXT_STRUCT {
    const char* application; // calling application name (usually first cmdline argument)
    Exception_T error_exception;
    HashMap_T* modules;
};

#endif /* ASTATINE_CONTEXT_H */
