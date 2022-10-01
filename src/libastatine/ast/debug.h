#ifndef ASTATINE_AST_DEBUG_H
#define ASTATINE_AST_DEBUG_H

#include "module.h"
#include "node.h"
#include "object.h"

void pretty_print_module(Module_T* module);
void pretty_print_node(Node_T* node);
void pretty_print_object(Object_T* node);

#endif /* ASTATINE_AST_DEBUG_H */
