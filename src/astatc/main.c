/*
    ASTATC - THE ASTATINE PROGRAMMING LANGUAGE COMPILER
    This is the main file and entry point to the compiler.

    This compiler and all components of Astatine, except external dependencies, are licensed under the MIT license.

    Creator:
        https://github.com/spydr06
    Official git repository:
        https://github.com/spydr06/astatine.git
*/

/*
    The MIT License (MIT) - This License applies to all parts of Astatine

    Copyright (c) 2022 Spydr06

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
*/

#include <astatine.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>

#include "common.h"
#include "flags.h"

#define ASTATC_GIT_REPOSITORY "https://github.com/spydr06/astatine"
#define ASTATC_GIT_DEVELOPER  "https://github.com/spydr06"

// text that gets shown if -i or --info is used
static const char info_text[] = 
        MAGENTA BOLD " ** astatc - The Astatine Programming Language Compiler **\n" RESET
        BOLD WHITE "Version:" RESET " %s\n"
        BOLD WHITE "Build:" RESET " %s\n"
       "\n"
       "Copyright (c) 2021 - 2022 Spydr06\n"
       "CSpydr is distributed under the MIT license.\n"
       "This is free software; see the source for copying conditions;\n"
       "you may redistribute it under the terms of the MIT license.\n"
       "This program has absolutely no warranty.\n"
       "\n"
    #ifdef ASTATC_GIT_REPOSITORY
       BOLD WHITE "    repository:     " RESET ASTATC_GIT_DEVELOPER "\n"
    #endif
    #ifdef ASTATC_GIT_DEVELOPER
       BOLD WHITE "    developer:      " RESET ASTATC_GIT_DEVELOPER "\n"
    #endif
       "\n"
       "Type -h or --help for the help page.\n";

static const char help_start_text[] = BOLD WHITE "Options:" RESET "\n";
static const char help_end_text[] = "\n"
#ifdef ASTATC_GIT_REPOSITORY
    "If you are unsure, what Astatine is (or how to use it), please check out the GitHub repository: \n" ASTATC_GIT_REPOSITORY "\n"
#endif
    ;

static const char version_text[] = MAGENTA BOLD " ** astatc - The Astatine Programming Language Compiler **\n" RESET
    BOLD WHITE "Version:" RESET " %s (libastatine v%s)\n"
    BOLD WHITE "Build:" RESET " %s\n"
    "\n"
    "For more information type -i; for help type -h.\n";

static void show_help(void);
static void show_info(void);
static void show_version(void);

static const Flag_T flags[] = {
    { 
        .name = "--help",
        .alias = "-h",
        .description = "Displays this help text.",
        .callback = {
            .none = show_help 
        }
    },
    {
        .name = "--info",
        .alias = "-i",
        .description = "Displays information text.",
        .callback = {
            .none = show_info
        }
    },
    {
        .name = "--version",
        .alias = "-v",
        .description = "Displays the Astatine version.",
        .callback = {
            .none = show_version
        }
    }
};

static const char* src_file = NULL;

#ifdef __GNUC__
__attribute((format(printf, 2, 3)))
#endif /* __GNUC__ */
static _Noreturn void fatal_error(const char* exec, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    fprintf(stderr, BOLD WHITE "%s: " RED "fatal error: " RESET, exec);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    fflush(stderr);
    
    va_end(ap);
    exit(EXIT_FAILURE);
}

static bool on_unknown_flag(const char** argv, size_t index)
{
    if(src_file)
        return true;
    
    src_file = argv[index];
    return false;
}

static void show_help(void)
{
    fprintf(stdout, help_start_text);

    for(size_t i = 0; i < LEN(flags); i++)
    {
        int32_t spacing = 15 - strlen(flags[i].name) - strlen(OR(flags[i].alias, ""));
        fprintf(stderr, "  %s %s%*s | %s\n", flags[i].name, OR(flags[i].alias, ""), spacing > 0 ? spacing : 0, "", OR(flags[i].description, ""));
    }

    fprintf(stdout, help_end_text);

    exit(EXIT_SUCCESS);
}

static void show_info(void)
{
    fprintf(stdout, info_text, PROJECT_VERSION, PROJECT_BUILD);

    exit(EXIT_SUCCESS);
}

static void show_version(void)
{
    fprintf(stdout, version_text, PROJECT_VERSION, astatine_version(), PROJECT_BUILD);

    exit(EXIT_SUCCESS);
}

static int64_t fsize(FILE* fp) {
    fseek(fp, 0, SEEK_END);
    int64_t size = ftell(fp);
    rewind(fp);
    return size;
}

static int32_t compile_file(const char* application, const char* filename)
{
    AstatineContext_T* context = astatine_initialize(application);
    if(!context)
    {
        fatal_error(application,
            "Failed to initialize astatine compiler runtime context.\n"
            "Check and update your installation and report an issue if this error persists.\n"
        );
    }

    FILE* fp = fopen(filename, "rb");
    if(!fp)
        fatal_error(application, "Error reading file `%s`: %s.", filename, strerror(errno));
    
    fseek(fp, 0, SEEK_END);
    size_t filesize = fsize(fp);

    char buffer[filesize + 1];
    fread(buffer, filesize, 1, fp);
    fclose(fp);

    buffer[filesize] = '\0';

    int32_t output = astatine_compile_module(context, buffer, filename);

    astatine_deinitialize(context);
    return output;
}

int main(int argc, const char* argv[]) {
    const char* application = *(argv++); // skip compiler exec

    int32_t err = parse_flags(flags, LEN(flags), argc - 1, argv, on_unknown_flag);
    if(err >= 0)
        fatal_error(application, "Unknown command line flag `%s`.", argv[err]);

    if(!src_file)
        fatal_error(application, "No input file.");

    return compile_file(application, src_file);
}
