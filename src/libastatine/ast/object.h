#ifndef ASTATINE_OBJECT_H
#define ASTATINE_OBJECT_H

#include "lexer/token.h"
#include <stdint.h>

typedef enum {
    OBJ_FUNCTION,
    OBJ_GLOBAL,

    OBJ_KIND_LEN // number of object kinds
} ObjectKind_T;

typedef struct OBJECT_STRUCT Object_T;
struct OBJECT_STRUCT {
    ObjectKind_T kind;
    Token_T* token;
    const char* name;

    union {
        struct {
            bool is_foreign : 1;
        };
        uint8_t options;
    };
};

Object_T* init_object(Module_T* module, Token_T* token);

#endif /* ASTATINE_OBJECT_H */
