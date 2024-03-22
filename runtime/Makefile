SOURCES := $(shell find -name '*.c')
OBJECTS := $(patsubst %.c, %.o, $(SOURCES))

CFLAGS += -Wall -Wextra -std=c99 -fPIC -DDO_RUNTIME_TYPECHECKS -c
LDFLAGS += -relocatable

.PHONY: all
all: $(RUNTIME)

$(RUNTIME): $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

.PHONY: tests
tests: CFLAGS += -DUNIT_TESTS -DRUNTIME_DEBUG
tests: $(RUNTIME_TESTS_EXE)

$(RUNTIME_TESTS_EXE): $(OBJECTS)
	$(CC) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP $^ -o $@

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.d

