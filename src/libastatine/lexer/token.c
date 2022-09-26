#include "token.h"

#include <string.h>
#include <stdio.h>

Token_T* token_init(Module_T* module, TokenKind_T kind, uint32_t line, uint64_t position, uint64_t length, const char* value)
{
    Token_T* tok = arena_malloc(&module->memory, sizeof(Token_T) + length + sizeof(char));
    memcpy(tok->value, value, length);

    tok->kind = kind;
    tok->line = line;
    tok->position = position;

    return tok;
}

static const char* token_kind_strings[] = {
    [TOKEN_ID]           = "IDENTIFIER",
    [TOKEN_STRING]       = "STRING",
    [TOKEN_CHAR]         = "CHAR",
    [TOKEN_INT]          = "INT",
    [TOKEN_FLOAT]        = "FLOAT",
    [TOKEN_PLUS]         = "PLUS",
    [TOKEN_MINUS]        = "MINUS",
    [TOKEN_STAR]         = "STAR",
    [TOKEN_SLASH]        = "SLASH",
    [TOKEN_PERCENT]      = "PERCENT",
    [TOKEN_EQUALS]       = "EQUALS",
    [TOKEN_LARROW]       = "LEFT ARROW",
    [TOKEN_RARROW]       = "RIGHT ARROW",
    [TOKEN_LPAREN]       = "LEFT PAREN",
    [TOKEN_RPAREN]       = "RIGHT PAREN",
    [TOKEN_LBRACE]       = "LEFT BRACE",
    [TOKEN_RBRACE]       = "RIGHT BRACE",
    [TOKEN_LBRACKET]     = "LEFT BRACKET",
    [TOKEN_RBRACKET]     = "RIGHT BRACKET",
    [TOKEN_COLON]        = "COLON",
    [TOKEN_DOUBLE_COLON] = "DOUBLE COLON", 
    [TOKEN_KEYW_MODULE]  = "KEYWORD MODULE",
    [TOKEN_KEYW_WHERE]   = "KEYWORD WHERE",
    [TOKEN_KEYW_IMPORT]  = "KEYWORD IMPORT",
    [TOKEN_KEYW_FROM]    = "KEYWORD FROM",
    [TOKEN_KEYW_DO]      = "KEYWORD DO",
    [TOKEN_KEYW_END]     = "KEYWORD END",
    [TOKEN_UNKNOWN]      = "UNKNOWN",
    [TOKEN_ERROR]        = "ERROR",
    [TOKEN_EOF]          = "EOF",
};

void debug_print_token(Token_T* token)
{
    static const char* fmt = "Token { kind: %s,\tline: %u,\tposition: %lu,\tvalue: `%s`\t}\n";
    printf(fmt, token_kind_strings[token->kind], token->line, token->position, token->value);
}
