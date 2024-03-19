#include "runtime.h"
#include "assert.h"

static void push_free_list(at_gc_header_t* bp);
static at_gc_header_t* morecore(size_t num_units);

void gc_begin(void)
{
    gc.freep = &gc.base;
    gc.usedp = NULL;
}

void gc_end(void)
{

}

// find chunk from the free list and put it in the used list
void* gc_malloc(size_t size)
{
    size_t num_units = (size + sizeof(at_gc_header_t) - 1) / sizeof(at_gc_header_t) + 1;
    at_gc_header_t* prevp = gc.freep;

    for(at_gc_header_t* p = prevp->next;; prevp = p, p = p->next) {
        if(p->size >= num_units) { // big enough
            if(p->size == num_units) // exact size
                prevp->next = p->next;
            else {
                p->size -= num_units;
                p += p->size;
                p->size = num_units;
            }

            gc.freep = prevp;

            // add p to the used list
            if(gc.usedp == NULL)
                gc.usedp = p->next = p;
            else {
                p->next = gc.usedp->next;
                gc.usedp->next = p;
            }

            return (void*) (p + 1);
        }
        if(p == gc.freep) { // not enough memory
            p = morecore(num_units);
            assert(p != NULL);
            if(p == NULL) // request for memory failed
                return NULL;
        }
    }
}

void* gc_calloc(size_t nmemb, size_t size)
{

}

void gc_free(void* ptr)
{

}

void gc_collect(void)
{

}

static void push_free_list(at_gc_header_t* bp)
{
    at_gc_header_t* p;
    for(p = gc.freep; !(bp > p && bp < p->next); p = p->next)
        if(p >= p->next && (bp > p || bp < p->next))
            break;

    if(bp + bp->size == p->next) {
        bp->size += p->next->size;
        bp->next = p->next->next;
    }
    else
        bp->next = p->next;

    if(p + p->size == bp) {
        p->size += bp->size;
        p->next = bp->next;
    }
    else
        p->next = bp;

    gc.freep = p;
}

// request more memory from the kernel

#ifdef __linux__
#include <unistd.h>

static at_gc_header_t* morecore(size_t num_units) {
    void* vp;
    at_gc_header_t* up;

    if(num_units > GC_MIN_ALLOC)
        num_units = GC_MIN_ALLOC / sizeof(at_gc_header_t);

    if((vp = sbrk(num_units * sizeof(at_gc_header_t))) == (void*) -1)
        return NULL;

    up = (at_gc_header_t*) vp;
    up->size = num_units;
    push_free_list(up);
    return gc.freep;
}

#else
    #error "morecore() isn't implemented for this platform yet."
#endif
