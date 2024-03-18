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

#define DO_RUNTIME_TYPECHECKS

#define QUOTE(t) #t

#define AT_RUNTIME_VERSION_MINOR 1
#define AT_RUNTIME_VERSION_MAJOR 0

#define AT_RUNTIME_VERSION_STRING QUOTE(AT_RUNTIME_VERSION_MAJOR) "." QUOTE(AT_RUNTIME_VERSION_MINOR)

#define AT_INT(lit) ((ATValue_T){.integer=(int64_t)(lit), .datatype=AT_VAL_INT})
#define AT_FLT(lit) ((ATValue_T){.floating=(float64_T)(lit), .datatype=AT_VAL_FLT})
#define AT_TRUE ((ATValue_T){.boolean=true, .datatype=AT_VAL_BOOL})
#define AT_FALSE ((ATValue_T){.boolean=false, .datatype=AT_VAL_FALSE})
#define AT_SLIT(lit) ((ATValue_T){.string=(lit), .datatype=AT_VAL_STRING})
#define AT_NIL ((ATValue_T){.datatype=AT_VAL_NIL})

typedef double float64_t;

typedef struct PACKED AT_LIST_STRUCT ATList_T;

typedef struct PACKED AT_VALUE_STRUCT {
    union {
        int64_t integer;
        float64_t floating;
        bool boolean;
        void* pointer;
        const char* string;
        ATList_T* list;
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
} ATValue_T;

struct PACKED AT_LIST_STRUCT {
    ATList_T* next;
    ATValue_T value;
};

typedef struct AT_GC_STRUCT {

} ATGC_T;

extern ATGC_T global_gc;

void gc_begin(ATGC_T* gc);
void gc_end(ATGC_T* gc);

void* gc_malloc(ATGC_T* gc, size_t size);
void* gc_calloc(ATGC_T* gc, size_t nmemb, size_t size);
void gc_free(ATGC_T* gc, void* ptr);
void gc_collect(ATGC_T* gc);


// astatine program entry point
extern ATValue_T Main_main(ATValue_T argc, ATValue_T argv, ATValue_T envp);

#endif /* _ASTATINE_RUNTIME_H */

