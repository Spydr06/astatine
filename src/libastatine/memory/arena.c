#include "arena.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <memory.h>

#define REGION_DEFAULT_CAPACITY (8 * 1024)

struct REGION_STRUCT {
    Region_T* next;
    size_t count;
    size_t capacity;
    uintptr_t data[];
};

static Region_T* new_region(size_t capacity)
{
    size_t size_bytes = sizeof(Region_T) + sizeof(uintptr_t) * capacity;

    Region_T* r = malloc(size_bytes);
    assert(r);
    r->next = NULL;
    r->count = 0;
    r->capacity = capacity;

    return r;
}

static void free_region(Region_T* r)
{
    free(r);
}

void* arena_malloc(Arena_T* arena, size_t size_bytes)
{
    size_t size = (size_bytes + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);
    
    if(arena->end == NULL) 
    {
        assert(arena->begin == NULL);
        size_t capacity = REGION_DEFAULT_CAPACITY;
        if(capacity < size)
            capacity = size;
        arena->end = new_region(capacity);
        arena->begin = arena->end;
    }

    while(arena->end->count + size > arena->end->capacity && arena->end->next != NULL)
        arena->end = arena->end->next;
    
    if (arena->end->count + size > arena->end->capacity) 
    {
        assert(arena->end->next == NULL);
        size_t capacity = REGION_DEFAULT_CAPACITY;
        if (capacity < size)
            capacity = size;
        arena->end->next = new_region(capacity);
        arena->end = arena->end->next;
    }

    void *result = &arena->end->data[arena->end->count];
    arena->end->count += size;
    return result;
}

void* arena_calloc(Arena_T* arena, size_t size, size_t elements)
{
    void* ptr = arena_malloc(arena, size * elements);
    memset(ptr, 0, size * elements);
    return ptr;
}

void arena_destroy(Arena_T* a)
{
    Region_T* r = a->begin;
    while(r)
    {
        Region_T* r0 = r;
        r = r->next;
        free_region(r0);
    }

    a->begin = NULL;
    a->end = NULL;
}
