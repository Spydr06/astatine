
#include "runtime.h"

at_garbage_collector_t gc;

#ifdef UNIT_TESTS
    #include "tests.h"
#else

#include <stdio.h>

int main(int argc, char** argv, char** envp) {
    gc_begin(&gc, &argc);

    at_val_t exit_code = Main_main(AT_INT(argc), AT_NIL, AT_NIL);
#ifdef DO_RUNTIME_TYPECHECKS
    if(exit_code.datatype != AT_VAL_INT)
        fprintf(stderr, "Main_main: return type is not of type integer.\n");
#endif

    gc_end(&gc);
    return (int) exit_code.integer;
}

#endif

