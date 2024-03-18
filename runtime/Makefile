SOURCES := $(shell find -name '*.c')
OBJECTS := $(patsubst %.c, %.o, $(SOURCES))

CFLAGS += -Wall -Wextra -std=c99 -fPIC -c
LDFLAGS += -relocatable

RUNTIME ?= runtime.o

.PHONY: all
all: $(RUNTIME)

$(RUNTIME): $(OBJECTS)
	$(LD) $(LDFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP $^ -o $@

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.d

