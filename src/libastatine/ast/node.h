#ifndef ASTATINE_NODE_H
#define ASTATINE_NODE_H

typedef enum {
    NODE_ID,
    NODE_UNKNOWN,
    NODE_KIND_LEN // number of node kinds
} NodeKind_T;

typedef struct NODE_STRUCT Node_T;
struct NODE_STRUCT {
    NodeKind_T kind;
};

#endif /* ASTATINE_NODE_H */
