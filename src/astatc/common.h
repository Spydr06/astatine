#ifndef ASTATC_COMMON_H
#define ASTATC_COMMON_H

#define RESET   "\033[0m"
#define BOLD    "\033[1m"
#define RED     "\033[31m"
#define WHITE   "\033[37m"
#define MAGENTA "\033[35m"

#define ERROR_FMT(str) BOLD RED "[Error] " RESET RED str RESET "\n"

#define LEN(arr) (sizeof(arr) / sizeof(arr[0]))

#define OR(a, b) ((a) ? (a) : (b))

#endif /* ASTATC_COMMON_H */
