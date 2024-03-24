#include "runtime.h"

at_val_t at_list_length(at_list_t* list) {
    uint64_t len = 0;
    while(list) {
        list = list->next;
        len++;
    }
    return AT_INT(len); 
}

at_list_t* at_list_append(at_list_t* list, at_list_t* append) {
    if(!list)
        return append;

    while(list->next)
        list = list->next;
    list->next = append;
    return list;
}

at_list_t* at_list_push(at_list_t* list, at_val_t value) {
    at_list_t* new = gc_malloc(&gc, sizeof(at_list_t));
    new->next = list;
    new->value = value;
    return new;
}

at_val_t at_list_head(at_list_t* list) {
    return list ? list->value : AT_NIL;
}

at_val_t at_list_tail(at_list_t* list) {
    return list && list->next ? AT_LIST(list->next) : AT_NIL;
}

