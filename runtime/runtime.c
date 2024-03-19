#include <stdio.h>

#include "runtime.h"

at_gc_t gc;

#ifndef UNIT_TESTS

int main(int argc, char** argv, char** envp) {
    gc_begin();

    at_val_t exit_code = Main_main(AT_INT(argc), AT_NIL, AT_NIL);
#ifdef DO_RUNTIME_TYPECHECKS
    if(exit_code.datatype != AT_VAL_INT)
        perror("Main_main: return type is not of type integer.");
#endif

    gc_end();

    return (int) exit_code.integer;
}

#else

#include "tests.h"

#endif
