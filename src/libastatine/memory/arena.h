#ifndef ASTATINE_ARENA_H
#define ASTATINE_ARENA_H

#include <stdint.h>
#include <stddef.h>

typedef struct REGION_STRUCT Region_T;
typedef struct ARENA_STRUCT {
    Region_T *begin, *end;
} Arena_T;

void* arena_malloc(Arena_T* arena, size_t size);
void* arena_calloc(Arena_T* arena, size_t size, size_t elements);
void arena_destroy(Arena_T* arena);

#endif /* ASTATINE_ARENA_H */
