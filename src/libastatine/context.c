#include "context.h"
#include "ast/debug.h"
#include "ast/module.h"
#include "memory/arena.h"
#include "memory/hashmap.h"
#include "passes.h"
#include "lexer/lexer.h"
#include "parser/parser.h"

#include <errno.h>
#include <stdlib.h>
#include <memory.h>
#include <stdio.h>

Context_T* astatine_initialize(const char* application)
{
    Context_T* context = calloc(1, sizeof(Context_T));

    context->application = application;
    context->modules = hashmap_init();

    return context;
}

void astatine_deinitialize(Context_T* context)
{
    List_T* modules = hashmap_values(context->modules);
    for(size_t i = 0; i < modules->size; i++)
        free_module(modules->items[i]);
    free_list(modules);
    hashmap_free(context->modules);
    free(context);
}

int32_t astatine_compile_module(Context_T* context, const char* source, const char* origin)
{
    if(!context || !source)
    {
        errno = EINVAL;
        return -1;
    }

    printf("%s:\n%s\n", origin, source);

    Module_T* module = init_module("<placeholder>", origin);

    Lexer_T lexer = {0};
    init_lexer(&lexer, module, source);

    Parser_T parser = {0};
    init_parser(&parser, module, &lexer);
    parse_module(&parser);
    free_parser(&parser);

    pretty_print_module(module);

    hashmap_put(context->modules, (char*) module->name, module);
    return 0;
}
