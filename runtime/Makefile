SOURCES := $(shell find -name '*.c')

CFLAGS += -Wall -Wextra -std=c99 -c
RUNTIME ?= runtime.o

.PHONY: all
all: $(RUNTIME)

$(RUNTIME): $(SOURCES)
	$(CC) $(CFLAGS) -MMD -MP $^ -o $@

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.d

