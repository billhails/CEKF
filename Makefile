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

MAIN=src/main.c
CFILES=$(filter-out $(MAIN), $(wildcard src/*.c))
EXTRA_CFILES=tmp/lexer.c tmp/parser.c
TEST_CFILES=$(wildcard tests/*.c)

TEST_TARGETS=$(patsubst tests/%.c,%,$(TEST_CFILES))

MAIN_OBJ=obj/main.o
OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
TEST_OBJ=$(patsubst tests/%,obj/%,$(patsubst %.c,%.o,$(TEST_CFILES)))

MAIN_DEP=dep/main.d
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))
TEST_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(TEST_OBJ)))

EXTRA_OBJ=$(patsubst tmp/%,obj/%,$(patsubst %.c,%.o,$(EXTRA_CFILES)))
EXTRA_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(EXTRA_OBJ)))

ALL_OBJ=$(OBJ) $(EXTRA_OBJ)
ALL_DEP=$(DEP) $(EXTRA_DEP)

all: $(TARGET)

$(TARGET): $(MAIN_OBJ) $(ALL_OBJ)
	$(CC) -o $@ $(MAIN_OBJ) $(ALL_OBJ) -lm

-include $(ALL_DEP)
-include Makefile.extra

$(MAIN_OBJ) $(OBJ): obj/%.o: src/%.c | obj
	$(CC) -I tmp/ -c $< -o $@

$(EXTRA_OBJ): obj/%.o: tmp/%.c | obj
	$(CC) -I src/ -c $< -o $@

$(TEST_OBJ): obj/%.o: tests/%.c | obj
	$(CC) -I src/ -I tmp/ -c $< -o $@

$(MAIN_DEP) $(DEP): dep/%.d: src/%.c | dep
	$(CC) -I tmp/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(EXTRA_DEP): dep/%.d: tmp/%.c | dep
	$(CC) -I src/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

tmp/lexer.c tmp/lexer.h: src/lexer.l tmp/parser.h | tmp
	flex --header-file=tmp/lexer.h -o $@ $<

tmp/parser.c tmp/parser.h: src/parser.y | tmp
	bison -v -Werror --header=tmp/parser.h -o tmp/parser.c $<

test: $(TEST_TARGETS)

$(TEST_TARGETS): %: obj/%.o $(ALL_OBJ)
	$(CC) -o $@ $< $(ALL_OBJ) -lm

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

profile: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET)

check-grammar:
	bison -Wcex --feature=syntax-only src/parser.y

list-cores:
	ls -rt1 /var/lib/apport/coredump/* | tail -1
# vim: noet,sw=8,tabstop=8
