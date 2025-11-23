# Makefile for LLVM prototype
# This is a standalone Makefile for building the prototype
# without modifying the main CEKF build system

LLVM_CONFIG = llvm-config-18
CC = clang

# Get LLVM flags
LLVM_VERSION = $(shell $(LLVM_CONFIG) --version | cut -d. -f1)
LLVM_CFLAGS = $(shell $(LLVM_CONFIG) --cflags)
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs core executionengine mcjit native)

# Directories
SRCDIR = ../src
GENDIR = ../generated

# Include paths
INCLUDES = -I$(SRCDIR) -I$(GENDIR)

# Compiler flags
CFLAGS = -Wall -Wextra -g $(LLVM_CFLAGS) $(INCLUDES)
LDFLAGS = $(LLVM_LDFLAGS) $(LLVM_LIBS) -lm

# Targets
PROTOTYPE = llvm_prototype

.PHONY: all clean run

all: check-llvm $(PROTOTYPE)

check-llvm:
	@echo "Checking LLVM installation..."
	@which $(LLVM_CONFIG) > /dev/null || \
		(echo "Error: llvm-config not found. Install LLVM development libraries."; \
		 echo "On Debian/Ubuntu: sudo apt-get install llvm-dev"; \
		 echo "On Fedora: sudo dnf install llvm-devel"; \
		 echo "On macOS: brew install llvm"; exit 1)
	@echo "Found LLVM version $(LLVM_VERSION)"

$(PROTOTYPE): llvm_prototype.c
	@echo "Building LLVM prototype..."
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)
	@echo "Build successful!"

run: $(PROTOTYPE)
	@echo "Running LLVM prototype..."
	@echo ""
	./$(PROTOTYPE)

clean:
	rm -f $(PROTOTYPE) *.o

help:
	@echo "LLVM Prototype Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Build the prototype (default)"
	@echo "  run        - Build and run the prototype"
	@echo "  clean      - Remove built files"
	@echo "  check-llvm - Verify LLVM installation"
	@echo "  help       - Show this help"
	@echo ""
	@echo "Requirements:"
	@echo "  - LLVM development libraries"
	@echo "  - clang compiler"
	@echo ""
	@echo "Example usage:"
	@echo "  make run"
