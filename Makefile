.PHONY: all clean deps profile check-grammar list-cores test

TARGET=cekf

# in Ubuntu 22.10
# `ulimit -c unlimited` to turn on core dumps
# written to /var/lib/apport/coredump/
PROFILING=-pg
OPTIMIZING=-O2
DEBUGGING=-g

CC=cc -Werror $(DEBUGGING)
# CC=cc -Werror $(OPTIMIZING)
# CC=cc -Werror $(PROFILING)

CFILES=$(wildcard src/*.c)
EXTRA_CFILES=tmp/lexer.c tmp/parser.c

OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))

EXTRA_OBJ=$(patsubst tmp/%,obj/%,$(patsubst %.c,%.o,$(EXTRA_CFILES)))
EXTRA_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(EXTRA_OBJ)))

ALL_OBJ=$(OBJ) $(EXTRA_OBJ)
ALL_DEP=$(DEP) $(EXTRA_DEP)

all: $(TARGET)

$(TARGET): $(ALL_OBJ)
	$(CC) -o $@ $(ALL_OBJ)

-include $(ALL_DEP)
-include Makefile.extra

$(OBJ): obj/%.o: src/%.c | obj
	$(CC) -I tmp/ -c $< -o $@

$(EXTRA_OBJ): obj/%.o: tmp/%.c | obj
	$(CC) -I src/ -c $< -o $@

$(DEP): dep/%.d: src/%.c | dep
	$(CC) -I tmp/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(EXTRA_DEP): dep/%.d: tmp/%.c | dep
	$(CC) -I src/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

tmp/lexer.c: src/lexer.l tmp/parser.h | tmp
	flex -o $@ $<

tmp/parser.c tmp/parser.h: src/parser.y | tmp
	bison -v -Werror --header=tmp/parser.h -o tmp/parser.c $<

dep:
	mkdir $@

obj:
	mkdir $@

tmp:
	mkdir $@

clean: deps
	rm -f $(TARGET) obj/* callgrind.out.* tmp/*

deps:
	rm -f dep/*

test: all
	./$(TARGET)

profile: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET)

check-grammar:
	bison -Wcex --feature=syntax-only src/parser.y

list-cores:
	ls -rt1 /var/lib/apport/coredump/* | tail -1
# vim: noet,sw=8,tabstop=8
