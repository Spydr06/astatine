#include "lexer.h"
#include "lexer/token.h"
#include "memory/hashmap.h"

#include <ctype.h>
#include <memory.h>
#include <stddef.h>
#include <stdint.h>

#define LEN(arr) (sizeof(arr) / (sizeof(*arr)))

static const char* SYMBOLS[TOKEN_EOF + 1] = {
    [TOKEN_LARROW]   = "<-",
    [TOKEN_RARROW]   = "->",
    [TOKEN_DOUBLE_COLON] = "::",
    [TOKEN_COLON] = ":",
    [TOKEN_PLUS]     = "+",
    [TOKEN_MINUS]    = "-",
    [TOKEN_STAR]     = "*",
    [TOKEN_SLASH]    = "/",
    [TOKEN_PERCENT]  = "%",
    [TOKEN_EQUALS]   = "=",
    [TOKEN_LPAREN]   = "(",
    [TOKEN_RPAREN]   = ")",
    [TOKEN_LBRACKET] = "[",
    [TOKEN_RBRACKET] = "]",
    [TOKEN_LBRACE]   = "{",
    [TOKEN_RBRACE]   = "}",
};

static const char* KEYWORDS[TOKEN_EOF + 1] = {
    [TOKEN_KEYW_MODULE] = "module",
    [TOKEN_KEYW_WHERE] = "where",
    [TOKEN_KEYW_IMPORT] = "import",
    [TOKEN_KEYW_FROM] = "from",
    [TOKEN_KEYW_DO] = "do",
    [TOKEN_KEYW_END] = "end",
    [TOKEN_KEYW_FOREIGN] = "foreign",
};

void init_lexer(Lexer_T* lexer, Module_T* module, const char* source)
{
    memset(lexer, 0, sizeof(Lexer_T));
    lexer->module = module;
    lexer->source = source;
    lexer->line = 1;
    lexer->c = lexer->source[0];
}

static inline char lexer_advance(Lexer_T* lexer)
{
    if(lexer->source[lexer->pos] == '\n')
        lexer->line++;
    
    if(lexer->source[lexer->pos] == '\0')
        return lexer->c;

    return lexer->c = lexer->source[++lexer->pos];
}

static inline Token_T* lexer_consume(Lexer_T* lexer, Token_T* token)
{
    lexer_advance(lexer);
    return token;
}

static inline char lexer_peek(Lexer_T* lexer, int64_t offset)
{
    return lexer->source[lexer->pos + offset];
}

static inline void lexer_skip_whitespace(Lexer_T* lexer)
{
    while(lexer->c == '\t' || lexer->c == ' ' || lexer->c == '\r' || lexer->c == '\n')
        lexer_advance(lexer);
}

static void lexer_skip_comment(Lexer_T* lexer)
{
repeat:
    if(lexer->c == '-' && lexer_peek(lexer, 1) == '-')
    {
        while(lexer->c != '\n' && lexer->c != '\0')
            lexer_advance(lexer);
        lexer_skip_whitespace(lexer);
        goto repeat;
    }
    else if(lexer->c == '-' && lexer_peek(lexer, 1) == '[')
    {
        while(lexer->c != '\0' && lexer->c != ']' && lexer_peek(lexer, 1) != '-')
            lexer_advance(lexer);
        lexer_skip_whitespace(lexer);
        goto repeat;
    }
}

static TokenKind_T find_keyword(const char* ptr, size_t len)
{
    for(TokenKind_T i = 0; i < TOKEN_EOF; i++)
    {
        if(KEYWORDS[i] && strncmp(KEYWORDS[i], ptr, len) == 0)
            return i;
    }
    return TOKEN_ID;
}

static Token_T* lexer_get_id(Lexer_T* lexer)
{

    uint64_t start_pos = lexer->pos;
    while(lexer->c != '\0' && (isalnum(lexer->c) || lexer->c == '_'))
        lexer_advance(lexer);
    uint64_t end_pos = lexer->pos;

    TokenKind_T kind = find_keyword(&lexer->source[start_pos], end_pos - start_pos);
    return token_init(lexer->module, kind, lexer->line, start_pos, end_pos - start_pos, &lexer->source[start_pos]);
}

static Token_T* lexer_get_decimal(Lexer_T* lexer)
{
    uint64_t start_pos = lexer->pos;
    while(isdigit(lexer->c)) lexer_advance(lexer);

    uint64_t end_pos = lexer->pos;
    if(lexer->c != '.')
        return token_init(lexer->module, TOKEN_INT, lexer->line, start_pos, end_pos - start_pos, &lexer->source[start_pos]);
    
    lexer_advance(lexer);
    while(isdigit(lexer->c)) lexer_advance(lexer);

    return token_init(lexer->module, TOKEN_FLOAT, lexer->line, start_pos, lexer->pos - start_pos, &lexer->source[start_pos]);
}

static Token_T* lexer_get_int(Lexer_T* lexer, int32_t base)
{
    uint64_t start_pos = lexer->pos;
    lexer_advance(lexer); // skip `0`
    lexer_advance(lexer); // skip `x` `o` or `b`

    while(
        (lexer->c >= '0' && lexer->c <= '9') || 
        (base > 10 
            ? toupper(lexer->c) >= 'A' && toupper(lexer->c) < 'A' + base - 10 
            : false
        )
    ) lexer_advance(lexer);
    if(isalnum(lexer->c))
        return token_init(lexer->module, TOKEN_ERROR, lexer->line, start_pos, lexer->pos - start_pos, &lexer->source[start_pos]);

    return token_init(lexer->module, TOKEN_INT, lexer->line, start_pos, lexer->pos - start_pos, &lexer->source[start_pos]);
}

static Token_T* lexer_get_number(Lexer_T* lexer)
{
    switch(lexer->c)
    {
        case '0':
            switch(lexer_peek(lexer, 1))
            {
                case 'x':
                    return lexer_get_int(lexer, 16);
                case 'o':
                    return lexer_get_int(lexer, 8);
                case 'b':
                    return lexer_get_int(lexer, 2);
                default:
                    if(isalpha(lexer->c))
                        return lexer_consume(lexer, token_init(lexer->module, TOKEN_ERROR, lexer->line, lexer->pos - 1, 2, &lexer->source[lexer->pos - 1]));
            }
            // fall through

        default:
            return lexer_get_decimal(lexer);
    }
}

static Token_T* lexer_get_string(Lexer_T* lexer)
{
    uint64_t start_pos = lexer->pos;
    lexer_advance(lexer);
    while(lexer->c != '\0' && lexer->c != '\n' && lexer->c != '"')
        lexer_advance(lexer);
    
    switch(lexer->c)
    {
        case '"':
            lexer_advance(lexer);
            return token_init(lexer->module, TOKEN_STRING, lexer->line, start_pos, lexer->pos - start_pos, &lexer->source[start_pos]);
        
        default:
            return token_init(lexer->module, TOKEN_ERROR, lexer->line, start_pos, lexer->pos - start_pos, &lexer->source[start_pos]);
    }
}

static Token_T* lexer_get_char(Lexer_T* lexer)
{
    uint64_t start_pos = lexer->pos;
    lexer_advance(lexer);

    switch(lexer->c)
    {
        case '\\':
            lexer_advance(lexer);
            lexer_advance(lexer);
            break;

        default:
            lexer_advance(lexer);
            break;
    }

    if(lexer->c != '\'')
        return token_init(lexer->module, TOKEN_ERROR, lexer->line, start_pos, lexer->pos - start_pos, &lexer->source[start_pos]);

    return lexer_consume(lexer, token_init(lexer->module, TOKEN_CHAR, lexer->line, start_pos, lexer->pos - start_pos, &lexer->source[start_pos]));
}

static Token_T* lexer_get_eof(Lexer_T* lexer)
{
    return token_init(lexer->module, TOKEN_EOF, lexer->line, lexer->pos, 3, "EOF");
}

static Token_T* lexer_get_symbol(Lexer_T* lexer)
{
    for(size_t i = 0; i < LEN(SYMBOLS); i++)
    {
        const char* s = SYMBOLS[i];
        if(!s)
            continue;
        
        if(strlen(s) == 1 && lexer->c == s[0])
            return lexer_consume(lexer, 
                token_init(lexer->module, i, lexer->line, lexer->pos, 1, s)
            );
        if(strlen(s) == 2 && lexer->c == s[0] && lexer_peek(lexer, 1) == s[1])
            return lexer_consume(lexer, 
                lexer_consume(lexer, 
                    token_init(lexer->module, i, lexer->line, lexer->pos, 2, s)
                )
            );
        if(strlen(s) == 3 && lexer->c == s[0] && lexer_peek(lexer, 1) == s[1] && lexer_peek(lexer, 2) == s[2])
            return lexer_consume(lexer, 
                lexer_consume(lexer, 
                    lexer_consume(lexer, 
                        token_init(lexer->module, i, lexer->line, lexer->pos, 3, s)
                    )
                )
            );
    }

    return lexer_consume(lexer, 
        token_init(lexer->module, TOKEN_UNKNOWN, lexer->line, lexer->pos, 1, &lexer->c)
    );
}

Token_T* lexer_next_token(Lexer_T* lexer)
{
    lexer_skip_whitespace(lexer);
    lexer_skip_comment(lexer);

    if(isalpha(lexer->c) || lexer->c == '_')
        return lexer_get_id(lexer);
    else if(isdigit(lexer->c))
        return lexer_get_number(lexer);
    else if(lexer->c == '"')
        return lexer_get_string(lexer);
    else if(lexer->c == '\'')
        return lexer_get_char(lexer);
    else if(lexer->c == '\0')
        return lexer_get_eof(lexer);
    else
        return lexer_get_symbol(lexer);

    return NULL;
}
