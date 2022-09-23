#ifndef ASTATC_FLAGS_H
#define ASTATC_FLAGS_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef enum FLAG_ENUM {
    FLAG_NONE = 0,
    FLAG_NUMBER,
    FLAG_STRING
} FlagType_T;

typedef struct FLAG_STRUCT {
    FlagType_T type;
    const char* name;
    const char* alias;
    const char* description;

    union {
        void (*none)(void);
        void (*number)(int32_t);
        void (*string)(const char*);
    } callback;
} Flag_T;

int32_t parse_flags(const Flag_T* flags, size_t num_flags, size_t argc, const char** argv, bool (*on_unknown)(const char**, size_t));

#endif /* ASTATC_FLAGS_H */
