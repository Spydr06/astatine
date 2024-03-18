#include <stdio.h>

#include "runtime.h"

ATGC_T global_gc;

int main (int argc, char** argv, char** envp) {
    gc_begin(&global_gc);

    ATValue_T exit_code = Main_main(AT_INT(argc), AT_NIL, AT_NIL);
#ifdef DO_RUNTIME_TYPECHECKS
    if(exit_code.datatype != AT_VAL_INT)
        perror("Main_main: return type is not of type integer.");
#endif

    gc_end(&global_gc);

    return (int) exit_code.integer;
}


