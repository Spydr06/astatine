#include <gtest/gtest.h>

extern "C" {
    #include <astatine.h>
    #include <lexer/lexer.h>
    #include <lexer/token.h>
}

#define LEXER_TEST_CASE(code, lexer, body)               \
    Module_T* module = init_module("Test", "unit test"); \
    Lexer_T lexer = {};                                  \
    init_lexer(&lexer, module, code);                    \
    {                                                    \
        body                                             \
    }                                                    \
    free_module(module)                                  \

TEST(LibAstatine, lexing_identifiers)
{
    const char* code = R"(
        module Main where
        import puts from IO
    )";

    TokenKind_T expected_tokens[] = {
        TOKEN_KEYW_MODULE,
        TOKEN_ID,
        TOKEN_KEYW_WHERE,
        TOKEN_KEYW_IMPORT,
        TOKEN_ID,
        TOKEN_KEYW_FROM,
        TOKEN_ID
    };

    LEXER_TEST_CASE(code, lexer, {
        Token_T* tok = NULL;
        int64_t i = 0;
        for(; (tok = lexer_next_token(&lexer)) && tok->kind != TOKEN_EOF; i++) {
            EXPECT_EQ(tok->kind, expected_tokens[i]);   
        }
        EXPECT_EQ(i, sizeof(expected_tokens) / sizeof(*expected_tokens));
    });
}

TEST(LibAstatine, lexing_symbols)
{
    const char* code = R"(
        + - * / % =
        :: :
        () {} []
    )";

    TokenKind_T expected_tokens[] = {
        TOKEN_PLUS,
        TOKEN_MINUS,
        TOKEN_STAR,
        TOKEN_SLASH,
        TOKEN_PERCENT,
        TOKEN_EQUALS,
        TOKEN_DOUBLE_COLON,
        TOKEN_COLON,
        TOKEN_LPAREN,
        TOKEN_RPAREN,
        TOKEN_LBRACE,
        TOKEN_RBRACE,
        TOKEN_LBRACKET,
        TOKEN_RBRACKET,
    };

    LEXER_TEST_CASE(code, lexer, {
        Token_T* tok = NULL;
        int64_t i = 0;
        for(; (tok = lexer_next_token(&lexer)) && tok->kind != TOKEN_EOF; i++) {
            EXPECT_EQ(tok->kind, expected_tokens[i]);   
        }
        EXPECT_EQ(i, sizeof(expected_tokens) / sizeof(*expected_tokens));
    });
}

TEST(LibAstatine, lexing_numbers)
{
    const char* code = R"(
        999
        3.141569
        0x0123456789ABCDEFabcdef
        0b101101
        0o770246
    )";

    TokenKind_T expected_tokens[] = {
        TOKEN_INT,
        TOKEN_FLOAT,
        TOKEN_INT,
        TOKEN_INT,
        TOKEN_INT
    };

    LEXER_TEST_CASE(code, lexer, {
        Token_T* tok = NULL;
        int64_t i = 0;
        for(; (tok = lexer_next_token(&lexer)) && tok->kind != TOKEN_EOF; i++) {
            EXPECT_EQ(tok->kind, expected_tokens[i]);   
        }
        EXPECT_EQ(i, sizeof(expected_tokens) / sizeof(*expected_tokens));
    });
}

TEST(LibAstatine, lexing_strings)
{
    const char* code = R"(
        "Hello, World"
        'a'
        '\n'
        '\''
    )";

    TokenKind_T expected_tokens[] = {
        TOKEN_STRING,
        TOKEN_CHAR,
        TOKEN_CHAR,
        TOKEN_CHAR
    };

    LEXER_TEST_CASE(code, lexer, {
        Token_T* tok = NULL;
        int64_t i = 0;
        for(; (tok = lexer_next_token(&lexer)) && tok->kind != TOKEN_EOF; i++) {
            EXPECT_EQ(tok->kind, expected_tokens[i]);   
        }
        EXPECT_EQ(i, sizeof(expected_tokens) / sizeof(*expected_tokens));
    });
}