.PHONY: all clean deps profile

TARGET=cekf

# in Ubuntu 22.10
# `ulimit -c unlimited` to turn on core dumps
# written to /var/lib/apport/coredump/
PROFILING=-pg
OPTIMIZING=-O2
DEBUGGING=-g

CC=cc -Werror $(PROFILING)

CFILES=$(wildcard src/*.c)

OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) -o $@ $(OBJ)

-include $(DEP)

$(OBJ): obj/%.o: src/%.c | obj
	$(CC) -c $< -o $@

$(DEP): dep/%.d: src/%.c | dep
	$(CC) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

dep:
	mkdir $@

obj:
	mkdir $@

clean: deps
	rm -f $(TARGET) $(OBJ) callgrind.out.*

deps:
	rm -f $(DEP)

profile: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET)

# vim: noet,sw=8,tabstop=8
