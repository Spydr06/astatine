#include "parser.h"
#include "ast/object.h"
#include "lexer/lexer.h"
#include "lexer/token.h"
#include "memory/list.h"

#include <memory.h>
#include <stdarg.h>
#include <stdio.h>

static inline Token_T* parser_advance(Parser_T* parser)
{
    Token_T* tok;
    if(parser->next_tokens->size == 0)
        tok = lexer_next_token(parser->lexer);
    else
        tok = list_drop(parser->next_tokens);
    parser->current = tok;
    return tok;
}

static Token_T* parser_consume(Parser_T* parser, TokenKind_T kind, const char* fmt, ...)
{
    Token_T* current = parser->current;
    if(current->kind == kind)
    {
        parser_advance(parser);
        return current;
    }

    // FIXME: better error handling
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit(1);
}

static Token_T* parser_peek(Parser_T* parser, uint32_t amount)
{
    while(parser->next_tokens->size < amount)
        list_push(parser->next_tokens, lexer_next_token(parser->lexer));
    return parser->next_tokens->items[amount - 1];
}

static void parse_module_declaration(Parser_T* parser)
{
    parser_consume(parser, TOKEN_KEYW_MODULE, "expect module declaration at beginning of file");
    Token_T* name = parser_consume(parser, TOKEN_ID, "expect module name after `module`");
    parser->module->name = name->value;
    parser_consume(parser, TOKEN_KEYW_WHERE, "expect keyword `where` after module name");
}

static Object_T* parse_definition(Parser_T* parser)
{
    Token_T* name = parser_consume(parser, TOKEN_ID, "expect identifier");
    Object_T* object = init_object(parser->module, name);
    object->name = name->value;
    parser_consume(parser, TOKEN_DOUBLE_COLON, "expect `::` after identifier");

    return object;
}

static Object_T* parse_foreign_definition(Parser_T* parser)
{
    parser_consume(parser, TOKEN_KEYW_FOREIGN, "expect `foreign`");
    Object_T* object = parse_definition(parser);
    object->is_foreign = true;
    return object;
}

void init_parser(Parser_T* parser, Module_T* module, Lexer_T* lexer)
{
    memset(parser, 0, sizeof(Parser_T));
    parser->module = module;
    parser->lexer = lexer;
    parser->next_tokens = init_list();
    parser->current = parser_advance(parser);
}

void free_parser(Parser_T* parser)
{
    free_list(parser->next_tokens);
}

void parse_module(Parser_T* parser)
{
    parse_module_declaration(parser);
    while(parser->current->kind != TOKEN_EOF)
    {
        switch(parser->current->kind)
        {
        case TOKEN_ID:
        {
            Object_T* obj = parse_definition(parser);
            if(obj)
                list_push(parser->module->objects, obj);
        } break;

        case TOKEN_KEYW_FOREIGN:
        {
            Object_T* obj = parse_foreign_definition(parser);
            if(obj)
                list_push(parser->module->objects, obj);
        } break;

        default:
            //FIXME: proper error handling
            fprintf(stderr, "unexpected token `%s`\n", parser->current->value);
            exit(1);
        }
    }
}


