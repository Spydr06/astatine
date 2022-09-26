/*
    ASTATINE.H - THE ASTATINE PROGRAMMING LANGUAGE INTERFACE
    This is the main header file and programming interface with the astatine programming language

    This library and all components of Astatine, except external dependencies, are licensed under the MIT license.

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

#ifndef ASTATINE_H
#define ASTATINE_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// astatine compiler runtime context
typedef struct ASTATINE_CONTEXT_STRUCT AstatineContext_T;

// astatine compiler pass function
typedef int32_t (*AstatinePassFn_T)(AstatineContext_T*);

// get the version and build-id of astatine 
extern const char* astatine_version(void);
extern const char* astatine_build(void);

// initializes the compiler runtime context
extern AstatineContext_T* astatine_initialize(const char* application);

// parse an astatine module
extern int32_t astatine_compile_module(AstatineContext_T* context, const char* source, const char* origin);

// deinitializes the compiler runtime context
extern void astatine_deinitialize(AstatineContext_T* context);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* ASTATINE_H */
