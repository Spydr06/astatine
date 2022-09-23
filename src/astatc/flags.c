#include "flags.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>
#include <limits.h>
#include <errno.h>

static bool handle_flag(const Flag_T* flag, const char** arg, size_t* i)
{
    switch(flag->type)
    {
        case FLAG_NUMBER: {
            (*i)++;
            (*arg)++;

            intmax_t number = strtoimax(*arg, NULL, 10);
            if(number == INT_MAX && errno == ERANGE)
                return true;

            flag->callback.number(number);
        } break;
        
        case FLAG_STRING:
            (*i)++;
            (*arg)++;
            flag->callback.string(*arg);
            break;

        case FLAG_NONE:
            flag->callback.none();
            break;
    }

    return false;
}

int32_t parse_flags(const Flag_T* flags, size_t num_flags, size_t argc, const char** argv, bool (*on_unknown)(const char**, size_t))
{
    for(size_t i = 0; i < argc; i++)
    {
        const char* arg = argv[i];
        const Flag_T* flag = NULL;

        for(size_t j = 0; j < num_flags; j++)
        {
            if(strcmp(arg, flags[j].name) == 0 || (flags[j].alias && strcmp(arg, flags[j].alias) == 0))
                flag = &flags[j];
        }

        if(flag == NULL) // if no matching flag was found, return the index of the current argument
        {
            if(on_unknown(argv, i))
                return i;
            continue;
        }
        
        handle_flag(flag, &arg, &i);
    }

    return -1;
}
