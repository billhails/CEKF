.PHONY: all clean deps profile check-grammar list-cores test indent

TARGET=cekf

# in Ubuntu 22.10
# `ulimit -c unlimited` to turn on core dumps
# written to /var/lib/apport/coredump/
MODE_P=-pg
MODE_O=-O2
MODE_D=-g

CCMODE = $(MODE_D)

CC=cc -Wall -Wextra -Werror $(CCMODE)
LAXCC=cc -Werror $(CCMODE)
PYTHON=python3
MAKEAST=$(PYTHON) ./tools/makeAST.py

EXTRA_YAML=$(filter-out src/primitives.yaml, $(wildcard src/*.yaml))
EXTRA_C_TARGETS=$(patsubst src/%.yaml,generated/%.c,$(EXTRA_YAML))
EXTRA_H_TARGETS=$(patsubst src/%.yaml,generated/%.h,$(EXTRA_YAML))
EXTRA_OBJTYPES_H_TARGETS=$(patsubst src/%.yaml,generated/%_objtypes.h,$(EXTRA_YAML))
EXTRA_DEBUG_H_TARGETS=$(patsubst src/%.yaml,generated/%_debug.h,$(EXTRA_YAML))
EXTRA_DEBUG_C_TARGETS=$(patsubst src/%.yaml,generated/%_debug.c,$(EXTRA_YAML))

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

TMP_H=generated/parser.h generated/lexer.h
TMP_C=generated/parser.c generated/lexer.c

all: $(TARGET)

$(TARGET): $(MAIN_OBJ) $(ALL_OBJ)
	$(CC) -o $@ $(MAIN_OBJ) $(ALL_OBJ) -lm

include $(ALL_DEP)

$(EXTRA_C_TARGETS): generated/%.c: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKEAST) $< c > $@ || (rm -f $@ ; exit 1)

$(EXTRA_H_TARGETS): generated/%.h: src/%.yaml tools/makeAST.py | generated
	$(MAKEAST) $< h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_OBJTYPES_H_TARGETS): generated/%_objtypes.h: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKEAST) $< objtypes_h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DEBUG_H_TARGETS): generated/%_debug.h: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKEAST) $< debug_h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DEBUG_C_TARGETS): generated/%_debug.c: src/%.yaml tools/makeAST.py src/primitives.yaml | generated
	$(MAKEAST) $< debug_c > $@ || (rm -f $@ ; exit 1)

.generated: $(EXTRA_TARGETS) $(TMP_H)
	touch $@

tags: src/* $(EXTRA_TARGETS) $(TMP_H) $(TMP_C)
	ctags src/* $(EXTRA_TARGETS) $(TMP_H) $(TMP_C)

$(MAIN_OBJ) $(OBJ): obj/%.o: src/%.c | obj
	$(CC) -I generated/ -I src/ -c $< -o $@

$(PARSER_OBJ): obj/%.o: generated/%.c | obj
	$(LAXCC) -I src/ -I generated/ -c $< -o $@

$(EXTRA_OBJ): obj/%.o: generated/%.c | obj
	$(CC) -I src/ -I generated/ -c $< -o $@

$(TEST_OBJ): obj/%.o: tests/src/%.c | obj
	$(LAXCC) -I src/ -I generated/ -c $< -o $@

$(MAIN_DEP) $(DEP): dep/%.d: src/%.c .generated | dep
	$(CC) -I generated/ -I src/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(EXTRA_DEP): dep/%.d: generated/%.c .generated | dep
	$(CC) -I src/ -I generated/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(PARSER_DEP): dep/%.d: generated/%.c .generated | dep
	$(CC) -I src/ -I generated/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(TEST_DEP): dep/%.d: tests/src/%.c .generated | dep
	$(CC) -I src/ -I generated/ -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

generated/lexer.c generated/lexer.h: src/lexer.l | generated
	flex --header-file=generated/lexer.h -o generated/lexer.c $<

generated/parser.c generated/parser.h: src/parser.y | generated
	bison -v -Werror --header=generated/parser.h -o generated/parser.c $<

test: $(TEST_TARGETS)
	for t in $(TEST_TARGETS) ; do echo '***' $$t '***' ; $$t || exit 1 ; done

$(TEST_TARGETS): tests/%: obj/%.o $(ALL_OBJ)
	$(CC) -o $@ $< $(ALL_OBJ) -lm

dep obj generated:
	mkdir $@

clean: deps
	rm -rf $(TARGET) obj callgrind.out.* generated $(TEST_TARGETS) tags .typedefs src/*~

deps:
	rm -rf dep

profile: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET)

indent: .typedefs .indent.pro
	indent `cat .typedefs | sort -u | xargs` -T bigint_word -T BigInt -T IntegerBinOp -T Stack -T Env -T Snapshot -T Kont -T ValueList -T Clo -T Fail -T Vec -T ProtectionStack -T HashSymbol -T hash_t -T Header -T PmModule -T HashTable -T byte -T word -T ByteCodes -T ByteCodeArray -T Value -T FILE -T Byte -T Character -T Word -T Integer -T Index -T Double -T Control src/*.[ch] generated/*.[ch]
	rm -f src/*~ generated/*~

.typedefs: .generated

check-grammar:
	bison -Wcex --feature=syntax-only src/parser.y

list-cores:
	ls -rt1 /var/lib/apport/coredump/* | tail -1

# vim: noet sw=8
