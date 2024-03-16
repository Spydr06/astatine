RUNTIME_DIR := runtime

RUNTIME_OBJ := runtime.o

.PHONY: all
all: astatc runtime

.PHONY: astatc
astatc:
	cabal build astatc

.PHONY: runtime
runtime:
	$(MAKE) -C $(RUNTIME_DIR) RUNTIME=$(shell realpath $(RUNTIME_OBJ))

.PHONY: clean
clean:
	cabal clean
	$(MAKE) -C $(RUNTIME_DIR) clean
