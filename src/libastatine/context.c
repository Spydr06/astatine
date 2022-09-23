#include "context.h"
#include "list.h"
#include "passes.h"

#include <errno.h>
#include <stdlib.h>
#include <memory.h>

Context_T* astatine_initialize(const char* application)
{
    Context_T* context = calloc(1, sizeof(Context_T));

    context->application = application;
    context->passes = init_list();

    return context;
}

void astatine_deinitialize(Context_T* context)
{
    free_list(context->passes);
    free(context);
}

void astatine_register_pass(Context_T* context, PassFn_T pass)
{
    if(list_contains(context->passes, (void*) pass))
        return; // pass is already in the list

    list_push(context->passes, (void*) pass);
}

size_t astatine_get_num_passes(AstatineContext_T* context)
{
    if(!context || !context->passes)
    {
        errno = EINVAL;
        return 0;
    }

    return context->passes->size;
}

void astatine_get_passes(Context_T* context, PassFn_T buffer[])
{
    if(!context || !context->passes)
    {
        errno = EINVAL;
        return;
    }

    memcpy(buffer, context->passes->items, sizeof(PassFn_T) * context->passes->size);
}

int32_t astatine_execute_passes(AstatineContext_T* context)
{
    if(!context)
    {
        errno = EINVAL;
        return -1;
    }

    try(context->error_exception)
    {
        for(size_t i = 0; i < context->passes->size; i++)
        {
            PassFn_T fn = (PassFn_T) context->passes->items[i];
            int32_t error = fn(context);
            if(error)
                throw(context->error_exception);
        }
    }
    catch {
        // TODO: error handling
    }

    return 0;
}
