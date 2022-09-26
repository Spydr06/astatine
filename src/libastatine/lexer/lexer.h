#ifndef ASTATINE_LEXER_H
#define ASTATINE_LEXER_H

#include "ast/module.h"
#include "token.h"
#include "memory/hashmap.h"

typedef struct LEXER_STRUCT {
    Module_T* module;
    const char* source;
    char c;
    uint32_t line;
    uint64_t pos;
    HashMap_T* keywords;
} Lexer_T;

void init_lexer(Lexer_T* lexer, Module_T* module, const char* source);
Token_T* lexer_next_token(Lexer_T* lexer);

#endif /* ASTATINE_LEXER_H */
