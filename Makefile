.PHONY: all clean realclean deps profile check-grammar list-cores test indent indent-src indent-generated docs

TARGET=cekf

MODE_P=-pg
MODE_O=-O2
MODE_D=-g

CCMODE = $(MODE_D)

CC:=cc -Wall -Wextra -Werror $(CCMODE)
LAXCC:=cc -Werror $(CCMODE)

ifdef TESTING
	CC := $(CC) -DNO_DEBUG_STRESS_GC
	LAXCC := $(LAXCC) -DNO_DEBUG_STRESS_GC
endif

PYTHON=python3
MAKE_AST=$(PYTHON) ./tools/makeAST.py

EXTRA_YAML=$(filter-out src/primitives.yaml, $(wildcard src/*.yaml))
EXTRA_C_TARGETS=$(patsubst src/%.yaml,generated/%.c,$(EXTRA_YAML))
EXTRA_H_TARGETS=$(patsubst src/%.yaml,generated/%.h,$(EXTRA_YAML))
EXTRA_OBJTYPES_H_TARGETS=$(patsubst src/%.yaml,generated/%_objtypes.h,$(EXTRA_YAML))
EXTRA_DEBUG_H_TARGETS=$(patsubst src/%.yaml,generated/%_debug.h,$(EXTRA_YAML))
EXTRA_DEBUG_C_TARGETS=$(patsubst src/%.yaml,generated/%_debug.c,$(EXTRA_YAML))

EXTRA_DOCS=$(patsubst src/%.yaml,docs/%.md,$(EXTRA_YAML))

EXTRA_TARGETS= \
    $(EXTRA_C_TARGETS) \
    $(EXTRA_H_TARGETS) \
    $(EXTRA_OBJTYPES_H_TARGETS) \
    $(EXTRA_DEBUG_H_TARGETS) \
    $(EXTRA_DEBUG_C_TARGETS)

MAIN=src/main.c
CFILES=$(filter-out $(MAIN), $(wildcard src/*.c))
EXTRA_CFILES=$(EXTRA_C_TARGETS) $(EXTRA_DEBUG_C_TARGETS)
PARSER_CFILES=generated/lexer.c generated/parser.c
TEST_CFILES=$(wildcard tests/src/*.c)

TEST_TARGETS=$(patsubst tests/src/%.c,tests/%,$(TEST_CFILES))

TEST_SUCCESSES=$(patsubst tests/%,tests/.%-success,$(TEST_TARGETS))

MAIN_OBJ=obj/main.o
OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
TEST_OBJ=$(patsubst tests/src/%,obj/%,$(patsubst %.c,%.o,$(TEST_CFILES)))

MAIN_DEP=dep/main.d
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))
TEST_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(TEST_OBJ)))

EXTRA_OBJ=$(patsubst generated/%,obj/%,$(patsubst %.c,%.o,$(EXTRA_CFILES)))
PARSER_OBJ=$(patsubst generated/%,obj/%,$(patsubst %.c,%.o,$(PARSER_CFILES)))
EXTRA_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(EXTRA_OBJ)))
PARSER_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(PARSER_OBJ)))

ALL_OBJ=$(OBJ) $(EXTRA_OBJ) $(PARSER_OBJ)
ALL_DEP=$(DEP) $(EXTRA_DEP) $(TEST_DEP) $(PARSER_DEP) $(MAIN_DEP)

INCLUDE_PATHS=-I generated/ -I src/

TMP_H=generated/parser.h generated/lexer.h
TMP_C=generated/parser.c generated/lexer.c

all: $(TARGET) docs

$(TARGET): $(MAIN_OBJ) $(ALL_OBJ)
	$(CC) -o $@ $(MAIN_OBJ) $(ALL_OBJ) -lm

docs: $(EXTRA_DOCS)

EXTRA_TYPES=bigint_word \
BigInt \
IntegerBinOp \
ProtectionStack \
HashSymbol \
hash_t \
Header \
PmModule \
HashTable \
byte \
word \
Value \
FILE \
Byte \
Character \
Word \
Integer \
Index \
Double \
Control
EXTRA_INDENT_ARGS=$(patsubst %,-T %,$(EXTRA_TYPES))

include $(ALL_DEP)

$(EXTRA_C_TARGETS): generated/%.c: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKE_AST) $< c > $@ || (rm -f $@ ; exit 1)

$(EXTRA_H_TARGETS): generated/%.h: src/%.yaml tools/makeAST.py | generated
	$(MAKE_AST) $< h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_OBJTYPES_H_TARGETS): generated/%_objtypes.h: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKE_AST) $< objtypes_h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DEBUG_H_TARGETS): generated/%_debug.h: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKE_AST) $< debug_h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DEBUG_C_TARGETS): generated/%_debug.c: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKE_AST) $< debug_c > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DOCS): docs/%.md: src/%.yaml tools/makeAST.py src/primitives.yaml
	$(MAKE_AST) $< md > $@ || (rm -f $@ ; exit 1)

.generated: $(EXTRA_TARGETS) $(TMP_H)
	touch $@

tags: src/* $(EXTRA_TARGETS) $(TMP_H) $(TMP_C)
	ctags src/* $(EXTRA_TARGETS) $(TMP_H) $(TMP_C)

$(MAIN_OBJ) $(OBJ): obj/%.o: src/%.c | obj
	$(CC) $(INCLUDE_PATHS) -c $< -o $@

$(PARSER_OBJ): obj/%.o: generated/%.c | obj
	$(LAXCC) $(INCLUDE_PATHS) -c $< -o $@

$(EXTRA_OBJ): obj/%.o: generated/%.c | obj
	$(CC) $(INCLUDE_PATHS) -c $< -o $@

$(TEST_OBJ): obj/%.o: tests/src/%.c | obj
	$(LAXCC) $(INCLUDE_PATHS) -c $< -o $@

$(MAIN_DEP) $(DEP): dep/%.d: src/%.c .generated | dep
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(PARSER_DEP) $(EXTRA_DEP): dep/%.d: generated/%.c .generated | dep
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(TEST_DEP): dep/%.d: tests/src/%.c .generated | dep
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

generated/lexer.c generated/lexer.h: src/lexer.l | generated
	flex --header-file=generated/lexer.h -o generated/lexer.c $<

generated/parser.c generated/parser.h: src/parser.y | generated
	bison -v -Werror -Wcounterexamples --header=generated/parser.h -o generated/parser.c $<

test: $(TEST_TARGETS) $(TARGET)
	for t in $(TEST_TARGETS) ; do echo '***' $$t '***' ; $$t || exit 1 ; done
	for t in tests/fn/*.fn ; do echo '***' $$t '***' ; ./$(TARGET) --include=fn --assertions-accumulate $$t || exit 1 ; done

$(TEST_TARGETS): tests/%: obj/%.o $(ALL_OBJ)
	$(CC) -o $@ $< $(ALL_OBJ) -lm

dep obj generated docs/generated:
	mkdir $@

realclean: clean
	rm -f tags

clean: deps
	rm -rf $(TARGET) obj callgrind.out.* generated $(TEST_TARGETS) .typedefs src/*~

deps:
	rm -rf dep

profile: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET)

indent: indent-src indent-generated

indent-src: .typedefs .indent.pro
	indent `cat .typedefs | sort -u | xargs` $(EXTRA_INDENT_ARGS) src/*.[ch]
	rm -f src/*~

indent-generated: .typedefs .indent.pro
	indent `cat .typedefs | sort -u | xargs` $(EXTRA_INDENT_ARGS) generated/*.[ch]
	rm -f generated/*~

.typedefs: .generated

check-grammar:
	bison -Wcex --feature=syntax-only src/parser.y

list-cores:
	ls -rt1 /var/lib/apport/coredump/* | tail -1

# vim: noet sw=8
