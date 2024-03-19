#ifndef _ASTATINE_RUNTIME_H
#define _ASTATINE_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __GNUC__
#define PACKED __attribute__((packed))
#else
#define PACKED
#endif

#define QUOTE(t) #t

// runtime version constants

#define AT_RUNTIME_VERSION_MINOR 1
#define AT_RUNTIME_VERSION_MAJOR 0

#define AT_RUNTIME_VERSION_STRING QUOTE(AT_RUNTIME_VERSION_MAJOR) "." QUOTE(AT_RUNTIME_VERSION_MINOR)

// astatine type and value representation

#define AT_INT(lit) ((at_val_t){.integer=(int64_t)(lit), .datatype=AT_VAL_INT})
#define AT_FLT(lit) ((at_val_t){.floating=(float64_T)(lit), .datatype=AT_VAL_FLT})
#define AT_TRUE ((at_val_t){.boolean=true, .datatype=AT_VAL_BOOL})
#define AT_FALSE ((at_val_t){.boolean=false, .datatype=AT_VAL_FALSE})
#define AT_SLIT(lit) ((at_val_t){.string=(lit), .datatype=AT_VAL_STRING})
#define AT_NIL ((at_val_t){.datatype=AT_VAL_NIL})

typedef double float64_t;

typedef struct PACKED AT_LIST_STRUCT at_list_t;

typedef struct PACKED AT_VALUE_STRUCT {
    union {
        int64_t integer;
        float64_t floating;
        bool boolean;
        void* pointer;
        const char* string;
        at_list_t* list;
    };
    enum : uint8_t {
        AT_VAL_INT,
        AT_VAL_FLT,
        AT_VAL_BOOL,
        AT_VAL_PTR,
        AT_VAL_STRING,
        AT_VAL_LIST,
        AT_VAL_NIL
    } datatype;
} at_val_t;

struct PACKED AT_LIST_STRUCT {
    at_list_t* next;
    at_val_t value;
};

// garbage collector

// allocate blocks in page-sized chunks
#define GC_MIN_ALLOC 4096

// allocator header
typedef struct AT_GC_HEADER_STRUCT {
    uint32_t size;
    struct AT_GC_HEADER_STRUCT* next;
} at_gc_header_t;

typedef struct AT_GC_STRUCT {
    at_gc_header_t base;
    at_gc_header_t* freep;
    at_gc_header_t* usedp;
} at_gc_t;

// global garbage collector
extern at_gc_t gc;

void gc_begin(void);
void gc_end(void);

void* gc_malloc(size_t size);
void* gc_calloc(size_t nmemb, size_t size);
void gc_free(void* ptr);
void gc_collect(void);

#ifndef UNIT_TESTS
    // astatine program entry point
    extern at_val_t Main_main(at_val_t argc, at_val_t argv, at_val_t envp);
#endif

#endif /* _ASTATINE_RUNTIME_H */

