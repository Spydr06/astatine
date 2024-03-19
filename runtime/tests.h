#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifdef __linux__
    #include <string.h>
    char* strsignal(int sig);

    #include <sys/types.h>
    #include <unistd.h>
    #include <sys/wait.h>
#endif

#include "runtime.h" 

#define PASS 0
#define FAIL 1

static int gc_init_deinit(void) {
    gc_begin();
    gc_end();
    return PASS;
}

static int gc_basic_malloc(void) {
    gc_begin();
    for(size_t i = 0; i < 10; i++)
        assert(gc_malloc(100) != NULL);
    gc_end();
    return PASS;
}

int main() {
    struct testcase {
        int (*func)(void);
        const char* desc;
    };

#define TESTCASE(name) {name, #name "()" }
    static const struct testcase tests[] = {
        TESTCASE(gc_init_deinit),
        TESTCASE(gc_basic_malloc),
        {NULL, NULL}
    };
#undef TESTCASE

    size_t failed = 0, passed = 0;
    for(const struct testcase* t = tests; t->func; t++)
    {
#ifdef __linux__
        pid_t pid = fork();
        assert(pid != -1);

        if(pid == 0)
            exit(t->func());
        
        int status;
        waitpid(pid, &status, 0);

        if(WIFEXITED(status)) {
            const int exit_code = WEXITSTATUS(status);
#else
            const int exit_code = t->func();
#endif
            if(exit_code)
                printf("* \033[1;31mFAILED\033[0m : %s (exit code: %d)\n", t->desc, exit_code);
            else
                printf("* \033[1;32mPASSED\033[0m : %s\n", t->desc);
            exit_code ? failed++ : passed++;
#ifdef __linux__
        }
        else if (WIFSIGNALED(status)) {
            printf("* \033[1;31mFAILED\033[0m : %s (child process terminated by signal: %s)\n", t->desc, strsignal(WTERMSIG(status)));
            failed++;
        }
        else {
            printf("* \033[1;31mFAILED\033[0m : %s (child process terminated abnormally)\n", t->desc);
            failed++;
        }
#endif

    }

    return failed;
}

