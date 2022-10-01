#ifndef ASTATINE_PARSER_H
#define ASTATINE_PARSER_H

#include "lexer/lexer.h"

typedef struct {
    Lexer_T* lexer;
    Module_T* module;

    List_T* next_tokens;
    Token_T* current;
} Parser_T;

void init_parser(Parser_T* parser, Module_T* module, Lexer_T* lexer);
void free_parser(Parser_T* parser);
void parse_module(Parser_T* parser);

#endif /* ASTATINE_PARSER_H */
