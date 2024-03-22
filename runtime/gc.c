#include "runtime.h"

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <setjmp.h>

// The garbage collector is largely inspired by https://github.com/mkirchner/gc
// Check out mkirchner's repo and license for more information!

// temporary allocation markings
#define GC_TAG_NONE 0
#define GC_TAG_ROOT 1 /* not automatically collected */
#define GC_TAG_MARK 2 /* part of mark-sweep */

typedef struct AT_ALLOCATION {
    void* ptr;                  // mem ptr
    size_t size;                // allocated size
    uint8_t tag;                // tag for mark-and-sweep
    void (*dtor)(void*);        // destructor
    struct AT_ALLOCATION* next;
} at_allocation_t;

static at_allocation_t* gc_allocation_new(void* ptr, size_t size, void (*dtor)(void*)) {
    at_allocation_t* a = (at_allocation_t*) malloc(sizeof(at_allocation_t));
    a->ptr = ptr;
    a->size = size;
    a->tag = GC_TAG_NONE;
    a->dtor = dtor;
    a->next = NULL;
    return a;
}

static void gc_allocation_delete(at_allocation_t* a) {
    free(a);
}

// hashmap that holds the allocation objects
struct AT_ALLOCATION_MAP {
    size_t capacity;
    size_t min_capacity;
    double downsize_factor;
    double upsize_factor;
    double sweep_factor;
    size_t sweep_limit;
    size_t size;
    at_allocation_t** allocs;
};

inline static double gc_allocation_map_load_factor(at_allocation_map_t* map) {
    return (double) map->size / (double) map->capacity;
}

inline static bool is_prime(size_t n) {
    /* https://stackoverflow.com/questions/1538644/c-determine-if-a-number-is-prime */
    if (n <= 3)
        return n > 1;     // as 2 and 3 are prime
    else if (n % 2 == 0 || n % 3 == 0)
        return false;     // check if n is divisible by 2 or 3
    else {
        for (size_t i = 5; i * i <= n; i += 6) {
            if (n % i == 0 || n % (i + 2) == 0)
                return false;
        }
        return true;
    }
}

inline static size_t next_prime(size_t n) {
    while(!is_prime(n)) ++n;
    return n;
}

static at_allocation_map_t* gc_allocation_map_new(size_t min_capacity, size_t capacity, double sweep_factor, double downsize_factor, double upsize_factor) {
    at_allocation_map_t* map = (at_allocation_map_t*) malloc(sizeof(at_allocation_map_t));
    map->min_capacity = next_prime(min_capacity);
    map->capacity = next_prime(capacity);
    if(map->capacity < map->min_capacity)
        map->capacity = map->min_capacity;
    map->sweep_factor = sweep_factor;
    map->sweep_limit = (int) (sweep_factor * map->capacity);
    map->downsize_factor = downsize_factor;
    map->upsize_factor = upsize_factor;
    map->allocs = (at_allocation_t**) calloc(map->capacity, sizeof(at_allocation_t*));
    map->size = 0;

    DBG_PRINTF("created allocation map (cap=%ld, size=%ld)\n", map->capacity, map->size);
    
    return map;
}

static void gc_allocation_map_delete(at_allocation_map_t* map) {
    DBG_PRINTF("deleting allocation map (cap=%ld, size=%ld)\n", map->capacity, map->size);

    at_allocation_t *alloc, *tmp;
    for(size_t i = 0; i < map->capacity; i++)
        if((alloc = map->allocs[i]))
            while(alloc) {
                tmp = alloc;
                alloc = alloc->next;
                gc_allocation_delete(tmp);
            }

    free(map->allocs);
    free(map);
}

static inline size_t gc_hash(void* ptr) {
    return ((uintptr_t) ptr) >> 3;
}

static void gc_allocation_map_resize(at_allocation_map_t* map, size_t new_capacity) {
    if(new_capacity < map->min_capacity)
        return;

    DBG_PRINTF("resizing allocation map (cap=%ld, size=%ld) -> (cap=%ld)\n", map->capacity, map->size, new_capacity);

    at_allocation_t** resized_allocs = calloc(new_capacity, sizeof(at_allocation_t*));

    for(size_t i = 0; i < map->capacity; i++) {
        at_allocation_t* alloc = map->allocs[i];
        while(alloc) {
            at_allocation_t* next_alloc = alloc->next;
            size_t new_index = gc_hash(alloc->ptr) % new_capacity;
            alloc->next = resized_allocs[new_index];
            resized_allocs[new_index] = alloc;
            alloc = next_alloc;
        }
    }

    free(map->allocs);
    map->capacity = new_capacity;
    map->allocs = resized_allocs;
    map->sweep_limit = map->size + map->sweep_factor * (map->capacity - map->size);
}

static bool gc_allocation_map_resize_to_fit(at_allocation_map_t* map) {
    double load_factor = gc_allocation_map_load_factor(map);
    if(load_factor > map->upsize_factor) {
        DBG_PRINTF("load factor %0.3g > %0.3g !! upsizing\n", load_factor, map->upsize_factor);
        gc_allocation_map_resize(map, next_prime(map->capacity * 2));
        return true;
    }
    if(load_factor < map->downsize_factor) {
        DBG_PRINTF("load factor %0.3g < %0.3g !! downsizing\n", load_factor, map->downsize_factor);
        gc_allocation_map_resize(map, next_prime(map->capacity / 2));
        return true;
    }
    return false;
}

static at_allocation_t* gc_allocation_map_get(at_allocation_map_t* map, void* ptr) {
    size_t index = gc_hash(ptr) % map->capacity;
    at_allocation_t* cur = map->allocs[index];
    while(cur) {
        if(cur->ptr == ptr) {
            return cur;
        }
        cur = cur->next;
    }
    return NULL;
}

static at_allocation_t* gc_allocation_map_put(at_allocation_map_t* map, void* ptr, size_t size, void (*dtor)(void*)) {
    size_t index = gc_hash(ptr) % map->capacity;

    DBG_PRINTF("put request for allocation idx=%ld\n", index);

    at_allocation_t* alloc = gc_allocation_new(ptr, size, dtor);
    at_allocation_t* cur = map->allocs[index];
    at_allocation_t* prev = NULL;

    while(cur != NULL) {
        if(cur->ptr == ptr) {
            // found it
            alloc->next = cur->next;
            if(!prev) // position 0
                map->allocs[index] = alloc;
            else
                prev->next = alloc;
            gc_allocation_delete(cur);

            DBG_PRINTF("allocation map upsert at idx=%ld\n", index);
            return alloc;
        }

        prev = cur;
        cur = cur->next;
    }

    // insert at the front
    cur = map->allocs[index];
    alloc->next = cur;
    map->allocs[index] = alloc;
    map->size++;

    DBG_PRINTF("allocation map insert at idx=%ld\n", index);

    void* p = alloc->ptr;
    if(gc_allocation_map_resize_to_fit(map))
        alloc = gc_allocation_map_get(map, p);
    return alloc;
}

static void gc_allocation_map_remove(at_allocation_map_t* map, void* ptr, bool allow_resize) {
    size_t index = gc_hash(ptr) % map->capacity;
    at_allocation_t* cur = map->allocs[index];
    at_allocation_t* prev = NULL;
    at_allocation_t* next;

    while(cur != NULL) {
        next = cur->next;
        if(cur->ptr == ptr) {
            // found it
            if(!prev)
                map->allocs[index] = cur->next;
            else
                prev->next = cur->next;
            gc_allocation_delete(cur);
            map->size--;
        }
        else // move on
            prev = cur;
        cur = next;
    }

    if(allow_resize)
        gc_allocation_map_resize_to_fit(map);
}

static inline void* gc_mcalloc(size_t count, size_t size) {
    if(!count)
        return malloc(size);
    return calloc(count, size);
}

static inline bool gc_needs_sweep(at_garbage_collector_t* gc) {
    return gc->allocs->size > gc->allocs->sweep_limit;
}

static void* gc_allocate(at_garbage_collector_t* gc, size_t count, size_t size, void (*dtor)(void*)) {
    if(gc_needs_sweep(gc) && !gc->paused) {
#ifdef RUNTIME_DEBUG
        size_t freed_mem = gc_run(gc);
        DBG_PRINTF("garbage collection cleaned up %lu bytes.", freed_mem);
#else
        gc_run(gc);
#endif
    }

    void* ptr = gc_mcalloc(size, count);
    size_t alloc_size = count ? count * size : size;

    // if allocation fails, collect garbage and try again
    if(!ptr && !gc->paused && (errno == EAGAIN || errno == ENOMEM)) {
        gc_run(gc);
        ptr = gc_mcalloc(count, size);
        if(!ptr)
            return NULL;
    }
    
    DBG_PRINTF("allocated %zu bytes at %p\n", alloc_size, (void*) ptr);

    at_allocation_t* alloc = gc_allocation_map_put(gc->allocs, ptr, alloc_size, dtor);

    if(alloc) {
        DBG_PRINTF("managing %zu bytes at %p\n", alloc_size, (void*) alloc->ptr);
        return alloc->ptr;
    }
    else {
        free(ptr);
        return NULL;
    }
}

static void gc_make_root(at_garbage_collector_t* gc, void* ptr)
{
    at_allocation_t* alloc = gc_allocation_map_get(gc->allocs, ptr);
    if(alloc)
        alloc->tag |= GC_TAG_ROOT;
}

void gc_begin_ext(
        at_garbage_collector_t* gc,
        void* stack_bottom,
        size_t initial_capacity, size_t min_capacity, 
        double downsize_load_factor, double upsize_load_factor, double sweep_factor
    ) {
    double downsize_limit = downsize_load_factor > 0.0 ? downsize_load_factor : 0.2;
    double upsize_limit = upsize_load_factor > 0.0 ? upsize_load_factor : 0.8;
    sweep_factor = sweep_factor > 0.0 ? sweep_factor : 0.5;
    gc->paused = false;
    gc->stack_bottom = stack_bottom;
    initial_capacity = initial_capacity < min_capacity ? min_capacity : initial_capacity;
    gc->allocs = gc_allocation_map_new(min_capacity, initial_capacity, sweep_factor, downsize_limit, upsize_limit);

    DBG_PRINTF("created new garbage collector (cap=%ld, size=%ld)\n", gc->allocs->capacity, gc->allocs->size);
}

void gc_pause(at_garbage_collector_t* gc) {
    gc->paused = true;
}

void gc_resume(at_garbage_collector_t* gc) {
    gc->paused = false;
}

void* gc_malloc_ext(at_garbage_collector_t* gc, size_t size, void (*dtor)(void*)) {
    return gc_allocate(gc, 0, size, dtor);
}

void* gc_calloc_ext(at_garbage_collector_t* gc, size_t nmemb, size_t size, void (*dtor)(void*)) {
    return gc_allocate(gc, nmemb, size, dtor);
}

void* gc_realloc(at_garbage_collector_t* gc, void* p, size_t size) {
    at_allocation_t* alloc = gc_allocation_map_get(gc->allocs, p);
    if(p && !alloc) {
        // the user passed an unknown pointer
        errno = EINVAL;
        return NULL;
    }
    void* q = realloc(p, size);
    if(!q) {
        // realloc failed but p is still valid
        return NULL;
    }
    if(!p) {
        // allocation, not reallocation
        at_allocation_t* alloc = gc_allocation_map_put(gc->allocs, q, size, NULL);
        return alloc->ptr;
    }
    if(p == q) {
        // successful reallocation w/o copy
        alloc->size = size;
    } else {
        // successful reallocation w/ copy
        void (*dtor)(void*) = alloc->dtor;
        gc_allocation_map_remove(gc->allocs, p, true);
        gc_allocation_map_put(gc->allocs, q, size, dtor);
    }
    return q;
}

void* gc_make_static(at_garbage_collector_t* gc, void* ptr) {
    gc_make_root(gc, ptr);
    return ptr;
}

// TODO: gc_realloc

void gc_free(at_garbage_collector_t* gc, void* ptr)
{
    at_allocation_t* alloc = gc_allocation_map_get(gc->allocs, ptr);
    if(!alloc) {
        DBG_PRINTF("ignoring request to free unknown ptr %p\n", ptr);
        return;
    }

    gc_allocation_map_remove(gc->allocs, ptr, true);
    if(alloc->dtor)
        alloc->dtor(ptr);
    free(ptr);
}

static void gc_mark_alloc(at_garbage_collector_t* gc, void* ptr) {
    at_allocation_t* alloc = gc_allocation_map_get(gc->allocs, ptr);
    if(!alloc || alloc->tag & GC_TAG_MARK)
        return;

    DBG_PRINTF("marking allocation (ptr=%p)\n", ptr);
    alloc->tag |= GC_TAG_MARK;

    // iterate over allocation contents
    DBG_PRINTF("checking allocation (ptr=%p, size=%lu) contents\n", ptr, alloc->size);
    for(uint8_t* p = (uint8_t*) alloc->ptr; p <= (uint8_t*) alloc->ptr + alloc->size - sizeof(uint8_t*); p++) {
        DBG_PRINTF("checking allocation (ptr=%p) @%lu with value %p\n", ptr, p - ((uint8_t*) alloc->ptr), *(void**)p);
        gc_mark_alloc(gc, *(void**) p);
    }
}

static void gc_mark_stack(at_garbage_collector_t* gc) {
    DBG_PRINTF("marking the stack (gc@%p) in increments of %ld\n", (void*) gc, sizeof(uint8_t));
    void* stack_top = __builtin_frame_address(0);
    void* stack_bottom = gc->stack_bottom;

    // stack grows from top to bottom
    for(uint8_t* p = (uint8_t*) stack_top; p <= (uint8_t*) stack_bottom - sizeof(uint8_t*); p++) {
        gc_mark_alloc(gc, *(void**) p);
    }
}

static void gc_mark_roots(at_garbage_collector_t* gc) {
    DBG_PRINTF("marking roots\n");
    for(size_t i = 0; i < gc->allocs->capacity; i++) {
        at_allocation_t* chunk = gc->allocs->allocs[i];
        while(chunk) {
            if(chunk->tag & GC_TAG_ROOT) {
                DBG_PRINTF("marking root @ %p\n", chunk->ptr);
                gc_mark_alloc(gc, chunk->ptr);
            }
            chunk = chunk->next;
        }
    }
}

static void gc_mark(at_garbage_collector_t* gc) {
    DBG_PRINTF("initiating GC mark (gc@%p)\n", (void*) gc);
    gc_mark_roots(gc);

    // dump registers onto stack and scan the stack
    void (*volatile mark_stack)(at_garbage_collector_t*) = gc_mark_stack;
    jmp_buf ctx;
    memset(&ctx, 0, sizeof(jmp_buf));
    setjmp(ctx);
    mark_stack(gc);
}

static size_t gc_sweep(at_garbage_collector_t* gc) {
    DBG_PRINTF("initiating GC sweep (gc@%p)\n", (void*) gc);
    size_t total = 0;

    for(size_t i = 0; i < gc->allocs->capacity; i++) {
        at_allocation_t* chunk = gc->allocs->allocs[i];
        at_allocation_t* next = NULL;

        while(chunk) {
            if(chunk->tag & GC_TAG_MARK) {
                DBG_PRINTF("found used allocation %p (ptr=%p)\n", (void*) chunk, (void*) chunk->ptr);
                // unmark
                chunk->tag &= ~GC_TAG_MARK;
                chunk = chunk->next;
            }
            else {
                DBG_PRINTF("found unused allocation %p (%lu bytes @ ptr=%p)\n", (void*) chunk, chunk->size, chunk->ptr);
                // delete chunk
                total += chunk->size;
                if(chunk->dtor) 
                    chunk->dtor(chunk->ptr);
                free(chunk->ptr);

                next = chunk->next;
                gc_allocation_map_remove(gc->allocs, chunk->ptr, false);
                chunk = next;
            }
        }
    }

    gc_allocation_map_resize_to_fit(gc->allocs);
    return total;
}

static void gc_unroot_roots(at_garbage_collector_t* gc) {
    DBG_PRINTF("Unmarking roots\n");
    
    for(size_t i = 0; i < gc->allocs->capacity; ++i) {
        at_allocation_t* chunk = gc->allocs->allocs[i];
        while(chunk) {
            if(chunk->tag & GC_TAG_ROOT)
                chunk->tag &= ~GC_TAG_ROOT;
            chunk = chunk->next;
        }
    }
}

size_t gc_end(at_garbage_collector_t *gc) {
    gc_unroot_roots(gc);
    size_t collected = gc_sweep(gc);
    gc_allocation_map_delete(gc->allocs);
    return collected;
}

size_t gc_run(at_garbage_collector_t* gc)
{
    DBG_PRINTF("initiating GC run (gc@%p)\n", (void*) gc);
    gc_mark(gc);
    return gc_sweep(gc);
}


