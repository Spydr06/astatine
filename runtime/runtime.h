#ifndef _ASTATINE_RUNTIME_H
#define _ASTATINE_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __linux__
    #define __USE_POSIX
    #include <stdio.h>
    #undef __USE_POSIX
#else
    #include <stdio.h>
#endif

#ifdef __GNUC__
#define PACKED __attribute__((packed))
#else
#define PACKED
#endif

#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

#define QUOTE(t) #t
#define TOSTRING(t) QUOTE(t)

#define LEN(a) (sizeof((a)) / sizeof(*(a)))

// runtime version constants

#define AT_RUNTIME_VERSION_MINOR 1
#define AT_RUNTIME_VERSION_MAJOR 0

#define AT_RUNTIME_VERSION_STRING QUOTE(AT_RUNTIME_VERSION_MAJOR) "." QUOTE(AT_RUNTIME_VERSION_MINOR)

// debug 
#ifdef RUNTIME_DEBUG
    #include <stdio.h>
    #define DBG_PRINTF(...) fprintf(stderr, "* \033[1;36mDEBUG\033[0m : " __FILE__ ":" TOSTRING(__LINE__) ": " __VA_ARGS__)
#else
    #define DBG_PRINTF(...)
#endif

#define STACKTRACE_MAX_SIZE 128

void NORETURN panic(const char* fmt, ...);
void dump_stacktrace(FILE* fp);
void install_handlers(void);

// astatine type and value representation

#define AT_INT(lit) ((at_val_t){.integer=(int64_t)(lit), .datatype=AT_VAL_INT})
#define AT_FLT(lit) ((at_val_t){.floating=(float64_t)(lit), .datatype=AT_VAL_FLT})
#define AT_CHAR(lit) ((at_val_t){.character=(unsigned char)(lit), .datatype=AT_VAL_CHAR})
#define AT_TRUE ((at_val_t){.boolean=true, .datatype=AT_VAL_BOOL})
#define AT_FALSE ((at_val_t){.boolean=false, .datatype=AT_VAL_BOOL})
#define AT_NIL ((at_val_t){.datatype=AT_VAL_NIL})
#define AT_EMPTY_LIST ((at_val_t){.list=NULL, .datatype=AT_VAL_LIST})
#define AT_LIST(_list) ((at_val_t){.list=(_list), .datatype=AT_VAL_LIST})

typedef double float64_t;

typedef struct AT_LIST at_list_t;
typedef struct AT_FUNC at_func_t;

typedef struct AT_VALUE {
    union {
        int64_t integer;
        float64_t floating;
        unsigned char character;
        bool boolean;
        void* pointer;
        at_func_t* func;
        at_list_t* list;
    };
    enum : uint8_t {
        AT_VAL_NIL = 0,
        AT_VAL_INT = 1,
        AT_VAL_FLT = 2,
        AT_VAL_CHAR = 3,
        AT_VAL_BOOL = 4,
        AT_VAL_FUNC = 5,
        AT_VAL_LIST = 6,
    } datatype;
} at_val_t;

// intrinsic functions
char* at_value_to_string(at_val_t value);

extern at_val_t at_slit(const char* str);
extern at_val_t at_add(at_val_t a, at_val_t b);

// linked lists

#define AT_L(_value, _next) (&((at_list_t){.next=(_next), .value=(_value)}))

struct AT_LIST {
    at_list_t* next;
    at_val_t value;
};

// list functions
extern at_val_t at_list_from_arr(size_t n, at_val_t* elems);

at_val_t at_list_length(at_list_t* list);

at_list_t* at_list_add(at_list_t* list, at_val_t value); // append single element to the end of a list
at_list_t* at_list_append(at_list_t* list, at_list_t* append); // append list to the end of another list

at_list_t* at_list_push(at_list_t* list, at_val_t value); // push single element to the start of the list

at_val_t at_list_head(at_list_t* list);
at_val_t at_list_tail(at_list_t* list);

// function values

#define AT_F(_fptr, _expected_args) ((at_func_t){.fptr=(at_val_t(*)())(_fptr), .curried_args=NULL, .expected_args=(_expected_args)})

struct AT_FUNC {
    at_val_t (*fptr)();
    at_list_t* curried_args;
    uint8_t expected_args;
};

// function functions

extern at_val_t at_call(at_val_t func, at_val_t arg);

// garbage collector

// Support for MSVC
#ifdef _MSC_VER
#define __builtin_frame_address(x) ((void)(x), _AddressOfReturnAddress())
#endif

typedef struct AT_ALLOCATION_MAP at_allocation_map_t;

typedef struct AT_GARBAGE_COLLECTOR {
    at_allocation_map_t* allocs;
    bool paused;
    void* stack_bottom;
    size_t min_size;
} at_garbage_collector_t;

// global garbage collector
extern at_garbage_collector_t gc;

#define gc_begin(gc, stack_base) (gc_begin_ext((gc), (stack_base), 1024, 1024, 0.2, 0.8, 0.5))
#define gc_begin_here(gc) gc_begin(gc, __builtin_frame_address(0))

void gc_begin_ext(at_garbage_collector_t* gc, void* stack_base, size_t initial_capacity, size_t min_capacity, double downsize_load_factor, double upsize_load_factor, double sweep_factor);
size_t gc_end(at_garbage_collector_t* gc);

void gc_pause(at_garbage_collector_t* gc);
void gc_resume(at_garbage_collector_t* gc); 
size_t gc_run(at_garbage_collector_t* gc);

#define gc_malloc(gc, size) (gc_malloc_ext((gc), (size), NULL))
#define gc_calloc(gc, nmemb, size) (gc_malloc_ext((gc), (nmemb), (size), NULL))

void* gc_malloc_ext(at_garbage_collector_t* gc, size_t size, void (*dtor)(void*));
void* gc_calloc_ext(at_garbage_collector_t* gc, size_t nmemb, size_t size, void (*dtor)(void*));
void* gc_realloc(at_garbage_collector_t* gc, void* ptr, size_t size);
void gc_free(at_garbage_collector_t* gc, void* ptr);

void* gc_make_static(at_garbage_collector_t* gc, void* ptr);

#ifndef UNIT_TESTS
    // astatine program entry point
    extern at_val_t Main_main(at_val_t argc, at_val_t argv, at_val_t envp);
    extern void __init_globals(void);
#endif

#endif /* _ASTATINE_RUNTIME_H */

