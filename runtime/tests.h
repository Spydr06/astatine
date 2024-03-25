#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

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

static int test_gc_init_deinit(void) {
    DBG_PRINTF("foo\n");
    gc_begin_here(&gc);
    gc_end(&gc);
    return PASS;
}

static int test_gc_basic_malloc(void) {
    gc_begin_here(&gc);
    for(size_t i = 0; i < 10; i++)
        assert(gc_malloc(&gc, 100) != NULL);
    gc_end(&gc);
    return PASS;
}

static int test_value_to_string(void) {
    gc_begin_here(&gc);

    struct { at_val_t val; const char* expect; } test_cases[] = {
        { AT_NIL, "nil" },
        { AT_TRUE, "true" },
        { AT_FALSE, "false" },
        { AT_INT(0), "0" },
        { AT_INT(69), "69" },
        { AT_INT(-420), "-420" },
        { AT_FLT(3.1415), "3.1415" },
        { AT_EMPTY_LIST, "[]" },
        { AT_LIST(AT_L(AT_INT(1), AT_L(AT_INT(2), AT_L(AT_INT(3), NULL)))), "[1, 2, 3]"}
    };

    int status = PASS;
    for(size_t i = 0; i < LEN(test_cases); i++) {
        const char* got = at_value_to_string(test_cases[i].val);
        if(strcmp(got, test_cases[i].expect) == 0)
            continue;

        fprintf(stderr, "\t\"%s\" did not match expected \"%s\"\n", got, test_cases[i].expect);
        status = FAIL;
    }

    gc_end(&gc);
    return status;
}

static int test_list_length(void) {
    gc_begin_here(&gc);

    struct { at_list_t* head; const char* expect; } test_cases[] = {
        { NULL, "0" },
        { AT_L(AT_NIL, NULL), "1" },
        { AT_L(AT_NIL, AT_L(AT_NIL, NULL)), "2" }
    };

    int status = PASS;
    for(size_t i = 0; i < LEN(test_cases); i++) {
        const char* got = at_value_to_string(at_list_length(test_cases[i].head));
        if(strcmp(got, test_cases[i].expect) == 0)
            continue;

        fprintf(stderr, "\t\"%s\" did not match expectted \"%s\"\n", got, test_cases[i].expect);
        status = FAIL;
    }

    gc_end(&gc);
    return status;
}

int main() {
    struct testcase {
        int (*func)(void);
        const char* desc;
    };

#define TESTCASE(name) {name, #name "()" }
    static const struct testcase tests[] = {
        TESTCASE(test_gc_init_deinit),
        TESTCASE(test_gc_basic_malloc),
        TESTCASE(test_value_to_string),
        TESTCASE(test_list_length),
        {NULL, NULL}
    };
#undef TESTCASE

    install_handlers();

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

