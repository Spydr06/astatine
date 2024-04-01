#include "runtime.h"
#include <stdarg.h>
#include <stdlib.h>

#ifdef __linux
    #define __USE_XOPEN2K8
    #include <string.h>
    #undef __USE_XOPEN2K8
#else
    #include <string.h>
#endif

#ifdef __GLIBC__
    #include <execinfo.h>
#endif

#ifdef __linux__
    #define __USE_POSIX
    #define __USE_POSIX199309
    #define __USE_MISC
    #define __USE_XOPEN_EXTENDED
    #include <signal.h>
#endif

at_garbage_collector_t gc;

#ifdef UNIT_TESTS
    #include "tests.h"
#else

#include <stdio.h>

int main(int argc, char** argv, char** envp) {
    install_handlers();
    __init_globals();
    gc_begin(&gc, &argc);

    at_val_t exit_code = Main_main(AT_INT(argc), AT_NIL, AT_NIL);
#ifdef DO_RUNTIME_TYPECHECKS
    if(exit_code.datatype != AT_VAL_INT)
        panic("Main_main: return type is not of type integer, got %s.", at_value_to_string(exit_code));
#endif

    gc_end(&gc);
    return (int) exit_code.integer;
}

#endif

void dump_stacktrace(FILE* fp) {
#ifdef __GLIBC__
    static void* stacktrace[STACKTRACE_MAX_SIZE];

    fflush(fp);
    size_t size = backtrace(stacktrace, LEN(stacktrace));
    
    if(fp == stderr || fp == stdout)
        fprintf(fp, "\033[1;38m");
    fprintf(fp, ">> Stack trace:\n");
    if(fp == stderr || fp == stdout)
        fprintf(fp, "\033[0m");
    backtrace_symbols_fd(stacktrace, size, fileno(fp));

    fflush(fp);
#endif
}

void NORETURN panic(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    fprintf(stderr, "\033[1;31mPANIC:\033[0m ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");

    dump_stacktrace(stderr);

    va_end(ap);
    exit(EXIT_FAILURE);
}

#ifdef __linux__
static void NORETURN sighandler(int sig) {
    panic("received signal %d: %s", sig, strsignal(sig));
}
#endif

void install_handlers(void) {
#ifdef __linux__
    struct sigaction sa;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sa.sa_handler = sighandler;

    const int err_signals[] = {
        SIGABRT, SIGBUS, SIGFPE, SIGILL, SIGQUIT, SIGSEGV, SIGSYS, SIGTRAP
    };

    // install for all signals
    for(size_t i = 0; i < LEN(err_signals); i++) {
        if(sigaction(err_signals[i], &sa, NULL) == -1)
            fprintf(stderr, "error setting up signal handler for signal %s\n", strsignal(err_signals[i]));
    }
#endif
}

