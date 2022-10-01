#ifndef ASTATINE_TOKEN_H
#define ASTATINE_TOKEN_H

#include <stdint.h>
#include "ast/module.h"

typedef enum TOKEN_KIND {
    TOKEN_ID,     // identifier
    TOKEN_STRING, // string literal "..."
    TOKEN_CHAR,   // char literal   'x'
    TOKEN_INT,    // integer literal
    TOKEN_FLOAT,  // floating-point literal

    TOKEN_RARROW,  // ->
    TOKEN_LARROW,  // <-

    TOKEN_PLUS,    // +
    TOKEN_MINUS,   // -
    TOKEN_STAR,    // *
    TOKEN_SLASH,   // /
    TOKEN_PERCENT, // %

    TOKEN_EQUALS,  // =
    
    TOKEN_DOUBLE_COLON, // ::
    TOKEN_COLON,        // :

    TOKEN_LPAREN,   // (
    TOKEN_RPAREN,   // )
    TOKEN_LBRACE,   // {
    TOKEN_RBRACE,   // }
    TOKEN_LBRACKET, // [
    TOKEN_RBRACKET, // ]

    TOKEN_KEYW_MODULE,  // module
    TOKEN_KEYW_WHERE,   // where
    TOKEN_KEYW_IMPORT,  // import
    TOKEN_KEYW_FROM,    // from
    TOKEN_KEYW_DO,      // do
    TOKEN_KEYW_END,     // end
    TOKEN_KEYW_FOREIGN, // foreign

    TOKEN_UNKNOWN, // unknown token
    TOKEN_ERROR,   // error token
    TOKEN_EOF      // end-of-file token
} TokenKind_T;

typedef struct TOKEN_STRUCT {
    TokenKind_T kind;
    uint32_t line;
    uint64_t position;
    char value[];
} Token_T;

Token_T* token_init(Module_T* module, TokenKind_T kind, uint32_t line, uint64_t position, uint64_t length, const char* value);
void pretty_print_token(Token_T* token);

#endif /* ASTATINE_TOKEN_H */
