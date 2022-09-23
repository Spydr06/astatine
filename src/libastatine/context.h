#ifndef ASTATINE_CONTEXT_H
#define ASTATINE_CONTEXT_H

#include <astatine.h>

#include "list.h"
#include "error/exception.h"

typedef struct ASTATINE_CONTEXT_STRUCT Context_T;

// Compile context struct, stores global variables, compiler state, etc.
struct ASTATINE_CONTEXT_STRUCT {
    const char* application; // calling application name (usually first cmdline argument)

    List_T* passes;

    Exception_T error_exception;
};

#endif /* ASTATINE_CONTEXT_H */
