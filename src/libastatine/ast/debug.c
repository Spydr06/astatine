#include "debug.h"
#include "lexer/token.h"

#include <stdio.h>

static const char* NODES[NODE_KIND_LEN] = {
    [NODE_ID] = "IDENTIFIER",
    [NODE_UNKNOWN] = "<UNKNOWN>",
};

static const char* OBJECTS[OBJ_KIND_LEN] = {
    [OBJ_FUNCTION] = "FUNCTION",
    [OBJ_GLOBAL] = "GLOBAL VARIABLE",
};

void pretty_print_object_(Object_T* obj, uint32_t indention)
{
    printf(
        "%*sObject {\n"
        "%*s  kind: %s (%d),\n"
        "%*s  token: ",
        indention, "",
        indention, "", OBJECTS[obj->kind], obj->kind,
        indention, ""
    );

    pretty_print_token(obj->token);

    printf(
        "%*s  name: `%s`,\n"
        "%*s  options: %08x,\n"
        "%*s}",
        indention, "", obj->name,
        indention, "", obj->options,
        indention, ""
    );
}

void pretty_print_object(Object_T* obj)
{
    pretty_print_object_(obj, 0);
}

void pretty_print_node_(Node_T* node, uint32_t indention)
{
    printf(
        "%*sNode {\n"
        "%*s  kind: %s (%d)\n"
        "%*s}\n", 
        indention, "",
        indention, "", NODES[node->kind], node->kind,
        indention, ""
    );
}

void pretty_print_node(Node_T* node)
{
    pretty_print_node_(node, 0);
}

void pretty_print_module(Module_T* module)
{
    printf(
        "Module {\n"
        "  name: %s,\n"
        "  origin: %s,\n"
        "  memory: Arena {},\n"
        "  objects: List {\n"
        "    size: %lu\n"
        "    items: [\n",
        module->name, module->origin,
        module->objects->size
    );

    for(size_t i = 0; i < module->objects->size; i++)
    {
        pretty_print_object_(module->objects->items[i], 6);
        printf("%s", module->objects->size - i > 1 ? ",\n" : "\n");
    }

    printf(
        "    ]\n"
        "  },\n"
        "}\n"
    );
}
