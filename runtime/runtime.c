#include <stdio.h>
#include <string.h>

#include "runtime.h"

at_garbage_collector_t gc;

#ifndef UNIT_TESTS

int main(int argc, char** argv, char** envp) {
    gc_begin(&gc, &argc);

    at_val_t exit_code = Main_main(AT_INT(argc), AT_NIL, AT_NIL);
#ifdef DO_RUNTIME_TYPECHECKS
    if(exit_code.datatype != AT_VAL_INT)
        perror("Main_main: return type is not of type integer.");
#endif

    gc_end(&gc);
    return (int) exit_code.integer;
}

#else

#include "tests.h"

#endif

char* at_value_to_string(at_val_t value) {
    switch(value.datatype) {
        case AT_VAL_NIL:
            return "nil";
        case AT_VAL_BOOL:
            return value.boolean ? "true" : "false";
        case AT_VAL_INT: {
            char* buf = gc_malloc(&gc, 32 * sizeof(char));
            snprintf(buf, 32, "%ld", value.integer);
            return buf;
        }
        case AT_VAL_FLT: {
            char* buf = gc_malloc(&gc, 40 * sizeof(char));
            snprintf(buf, 40, "%g", value.floating);
            return buf;
        }
        case AT_VAL_PTR: {
            char* buf = gc_malloc(&gc, 20 * sizeof(char));
            snprintf(buf, 20, "%p", value.pointer);
            return buf;
        }
        case AT_VAL_LIST: {
            if(!value.list)
                return "[]";

            char* buf = gc_malloc(&gc, 21);
            size_t size = 1, alloc = 20;
            buf[0] = '[';
            buf[1] = '\0';

            for(at_list_t* next = value.list; next; next = next->next) {
                char* elem_str = at_value_to_string(next->value);
                size += strlen(elem_str) + 1;
                if(alloc < size)
                    buf = gc_realloc(&gc, buf, (alloc = size) + 1);
                strcat(buf, elem_str);
                if(next->next)
                    strcat(buf, ", ");
                else
                    strcat(buf, "]");
            }

            return buf;
        }
    }
}
