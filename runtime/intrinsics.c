#include "runtime.h"
#include <string.h>
#include <stdio.h>

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
/*        case AT_VAL_FUNC: {
            char* buf = gc_malloc(&gc, 20 * sizeof(char));
            snprintf(buf, 20, "%p", value.pointer);
            return buf;
        }*/
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

    return "";
}

at_val_t at_slit(const char* str) {
    at_val_t val = AT_LIST(NULL);
    
    at_list_t** next = &val.list;
    while(*str++) {
        (*next) = gc_malloc(&gc, sizeof(at_list_t)); 
        (*next)->value = AT_CHAR(*str);
        next = &(*next)->next;
    }

    return val;
}

at_val_t at_call(at_val_t val, at_val_t arg) {
#ifdef DO_RUNTIME_TYPECHECKS
    if(val.datatype != AT_VAL_FUNC)
        fprintf(stderr, "at_call: supplied function does not have type AT_VAL_FUNC.\n");
#endif

    
}

/*at_val_t at_add(at_val_t a, at_val_t b) {
    switch(a.datatype) {
        case AT_VAL_FLT:
            switch(b.datatype) {
                case AT_VAL_FLT:
                    return AT_FLT(a.floating + b.floating);
                case AT_VAL_INT:
                    return AT_FLT(a.floating + (double) b.integer);
                case AT_VAL_CHAR:
                    return AT_FLT(a.floating + (double) b.character);
                default:
                    
            }
    }
}*/
