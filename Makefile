.PHONY: all clean realclean deps profile leak-check check-grammar list-cores test indent indent-src indent-generated docs install-sqlite3

# pass on the command line, i.e. `make test MODE=testing`
#
# debugging:  -g, and turns on DEBUG_STRESS_GC which forces a garbage collection on every malloc
# testing:    -g, but no DEBUG_STRESS_GC
# production: -O2, no DEBUG_STRESS_GC and disables all safety checks
ifndef MODE
MODE:=debugging
endif

TARGET=cekf
PRATT_TEST_TARGET=pratt_test

ifeq ($(MODE),debugging)
	CCMODE:= -g
	EXTRA_DEFINES:= -DBUILD_MODE=0
else
ifeq ($(MODE),testing)
	CCMODE:= -g
	EXTRA_DEFINES:= -DNO_DEBUG_STRESS_GC -DBUILD_MODE=1
else
ifeq ($(MODE),production)
	CCMODE:= -O2
	EXTRA_DEFINES:= -DPRODUCTION_BUILD -DBUILD_MODE=2
else
$(error invalid MODE $(MODE))
endif
endif
endif

CC:=cc -Wall -Wextra -Werror $(CCMODE) $(EXTRA_DEFINES)
LAXCC:=cc -Werror $(CCMODE) $(EXTRA_DEFINES)

PYTHON=python3
MAKE_AST=$(PYTHON) ./tools/makeAST.py

LIBS=-lm -lsqlite3

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
PRATT_TEST=src/pratt_test.c
CFILES=$(filter-out $(MAIN), $(filter-out $(PRATT_TEST), $(wildcard src/*.c)))
EXTRA_CFILES=$(EXTRA_C_TARGETS) $(EXTRA_DEBUG_C_TARGETS)
PARSER_CFILES=generated/lexer.c generated/parser.c
TEST_CFILES=$(wildcard tests/src/*.c)

TEST_TARGETS=$(patsubst tests/src/%.c,tests/%,$(TEST_CFILES))

TEST_SUCCESSES=$(patsubst tests/%,tests/.%-success,$(TEST_TARGETS))

MAIN_OBJ=obj/main.o
PRATT_TEST_OBJ=obj/pratt_test.o
OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
TEST_OBJ=$(patsubst tests/src/%,obj/%,$(patsubst %.c,%.o,$(TEST_CFILES)))

MAIN_DEP=dep/main.d
PRATT_DEP=dep/pratt_test.d
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))
TEST_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(TEST_OBJ)))

EXTRA_OBJ=$(patsubst generated/%,obj/%,$(patsubst %.c,%.o,$(EXTRA_CFILES)))
PARSER_OBJ=$(patsubst generated/%,obj/%,$(patsubst %.c,%.o,$(PARSER_CFILES)))
EXTRA_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(EXTRA_OBJ)))
PARSER_DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(PARSER_OBJ)))

ALL_OBJ=$(OBJ) $(EXTRA_OBJ) $(PARSER_OBJ)
ALL_DEP=$(DEP) $(EXTRA_DEP) $(TEST_DEP) $(PARSER_DEP) $(MAIN_DEP) $(PRATT_DEP)

INCLUDE_PATHS=-I generated/ -I src/

TMP_H=generated/parser.h generated/lexer.h
TMP_C=generated/parser.c generated/lexer.c

all: $(TARGET) $(PRATT_TEST_TARGET) docs

$(TARGET): $(MAIN_OBJ) $(ALL_OBJ)
	$(CC) -o $@ $(MAIN_OBJ) $(ALL_OBJ) $(LIBS)

$(PRATT_TEST_TARGET): $(PRATT_TEST_OBJ) $(ALL_OBJ)
	$(CC) -o $@ $(PRATT_TEST_OBJ) $(ALL_OBJ) $(LIBS)

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

$(MAIN_OBJ) $(PRATT_TEST_OBJ) $(OBJ): obj/%.o: src/%.c | obj
	$(CC) $(INCLUDE_PATHS) -c $< -o $@

$(PARSER_OBJ): obj/%.o: generated/%.c | obj
	$(LAXCC) $(INCLUDE_PATHS) -c $< -o $@

$(EXTRA_OBJ): obj/%.o: generated/%.c | obj
	$(CC) $(INCLUDE_PATHS) -c $< -o $@

$(TEST_OBJ): obj/%.o: tests/src/%.c | obj
	$(LAXCC) $(INCLUDE_PATHS) -c $< -o $@

$(MAIN_DEP) $(PRATT_DEP) $(DEP): dep/%.d: src/%.c .generated | dep
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(PARSER_DEP) $(EXTRA_DEP): dep/%.d: generated/%.c .generated | dep
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(TEST_DEP): dep/%.d: tests/src/%.c .generated | dep
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

generated/lexer.c generated/lexer.h: src/lexer.l | generated
	flex --header-file=generated/lexer.h -o generated/lexer.c $<

generated/parser.c generated/parser.h: src/parser.y | generated
	bison -v -Werror -Wcounterexamples --header=generated/parser.h -o generated/parser.c $<

test: $(TEST_TARGETS) $(TARGET) unicode/unicode.db
	for t in $(TEST_TARGETS) ; do echo '***' $$t '***' ; $$t || exit 1 ; done
	for t in tests/fn/*.fn ; do echo '***' $$t '***' ; ./$(TARGET) --include=fn --assertions-accumulate $$t || exit 1 ; done

$(TEST_TARGETS): tests/%: obj/%.o $(ALL_OBJ)
	$(CC) -o $@ $< $(ALL_OBJ) $(LIBS)

dep obj generated docs/generated unicode:
	mkdir $@

install-sqlite3:
	sudo apt-get --yes install sqlite3 libsqlite3-dev

unicode/unicode.db: unicode/UnicodeData.csv ./tools/create_table.sql
	rm -f $@
	sqlite3 $@ < ./tools/create_table.sql

unicode/UnicodeData.csv: unicode/UnicodeData.txt ./tools/convertCsv.py
	$(PYTHON) ./tools/convertCsv.py

unicode/UnicodeData.txt: | unicode
	wget https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	mv UnicodeData.txt $@

realclean: clean
	rm -rf tags unicode

clean: deps
	rm -rf $(TARGET) obj callgrind.out.* generated $(TEST_TARGETS) .typedefs src/*~ .generated gmon.out *.fnc core

deps:
	rm -rf dep

PROF_SRC=fib20

profile: all
	rm -f callgrind.out.*
	./$(TARGET) --binary-out=$(PROF_SRC).fnc fn/$(PROF_SRC).fn
	valgrind --tool=callgrind ./$(TARGET) --binary-in=$(PROF_SRC).fnc

leak-check: all
	valgrind --leak-check=full ./$(TARGET) fn/$(PROF_SRC).fn

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
	@ls -rt1 /var/lib/apport/coredump/* | tail -1

# vim: set noet sw=4:
