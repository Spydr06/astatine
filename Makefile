RUNTIME_DIR := runtime

RUNTIME_OBJ ?= runtime.o
RUNTIME_TESTS_EXE ?= runtime-tests

.PHONY: all
all: astatc runtime

.PHONY: tests
tests: runtime-tests

.PHONY: astatc
astatc:
	cabal build astatc

.PHONY: runtime
runtime:
	$(MAKE) -C $(RUNTIME_DIR) RUNTIME=$(shell realpath $(RUNTIME_OBJ))

.PHONY: runtime-tests
runtime-tests:
	$(MAKE) -C $(RUNTIME_DIR) RUNTIME_TESTS_EXE=$(shell realpath $(RUNTIME_TESTS_EXE)) tests

.PHONY: clean
clean:
	cabal clean
	$(MAKE) -C $(RUNTIME_DIR) clean
