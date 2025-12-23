.PHONY: all clean realclean deps profile leak-check check-grammar \
list-cores test indent indent-src indent-generated docs \
install-sqlite3 coverage extracov view-coverage \
establish-baseline test-refactoring update-baseline clean-baseline

# pass on the command line, i.e. `make test MODE=testing`
#
# debugging:  -g, and turns on DEBUG_STRESS_GC which forces a garbage collection on every malloc
# testing:    -g, but no DEBUG_STRESS_GC
# unit:       like debugging but compiles with -DUNIT_TESTS
# production: -O2, no DEBUG_STRESS_GC and disables all safety checks
# coverage:   -g with --coverage flags for gcov/lcov analysis
ifndef MODE
MODE:=debugging
endif

BINDIR=bin
DEPDIR=dep
DOCDIR=docs/generated
GENDIR=generated
OBJDIR=obj
SRCDIR=src
TSTDIR=tests
UNIDIR=unicode

TARGET=$(BINDIR)/fn

ifeq ($(MODE),debugging)
	CCMODE:= -g
	EXTRA_DEFINES:= -DBUILD_MODE=0
else
ifeq ($(MODE),testing)
	CCMODE:= -g
	EXTRA_DEFINES:= -DNO_DEBUG_STRESS_GC -DBUILD_MODE=1
else
ifeq ($(MODE),unit)
	CCMODE:= -g
	EXTRA_DEFINES:= -DUNIT_TESTS -DBUILD_MODE=0
else
ifeq ($(MODE),coverage)
	CCMODE:= -g --coverage
	EXTRA_DEFINES:= -DNO_DEBUG_STRESS_GC -DBUILD_MODE=3
	LIBS+= -lgcov
else
ifeq ($(MODE),production)
	CCMODE:= -O2
	EXTRA_DEFINES:= -DPRODUCTION_BUILD -DBUILD_MODE=2
else
$(error invalid MODE=$(MODE), allowed values: debugging, testing, unit, coverage or production)
endif
endif
endif
endif
endif

ifndef CCC
CCC:=clang
endif

CC:=$(CCC) -Wall -Wextra -Werror $(CCMODE) $(EXTRA_DEFINES)
LAXCC:=$(CCC) -Werror $(CCMODE) $(EXTRA_DEFINES)

PYTHON=python3
GENERATE=./tools/generate.py
MAKE_AST=$(PYTHON) $(GENERATE)
GENDEPS=$(GENERATE) $(wildcard ./tools/generate/*.py)

LIBS=-lm -lsqlite3

PREAMBLE_SRC=$(SRCDIR)/preamble.fn
EXTRA_YAML=$(filter-out $(SRCDIR)/primitives.yaml, $(wildcard $(SRCDIR)/*.yaml))
EXTRA_C_TARGETS=$(patsubst $(SRCDIR)/%.yaml,$(GENDIR)/%.c,$(EXTRA_YAML))
EXTRA_H_TARGETS=$(patsubst $(SRCDIR)/%.yaml,$(GENDIR)/%.h,$(EXTRA_YAML))
EXTRA_OBJTYPES_H_TARGETS=$(patsubst $(SRCDIR)/%.yaml,$(GENDIR)/%_objtypes.h,$(EXTRA_YAML))
EXTRA_DEBUG_H_TARGETS=$(patsubst $(SRCDIR)/%.yaml,$(GENDIR)/%_debug.h,$(EXTRA_YAML))
EXTRA_DEBUG_C_TARGETS=$(patsubst $(SRCDIR)/%.yaml,$(GENDIR)/%_debug.c,$(EXTRA_YAML))

EXTRA_DOCS=$(patsubst $(SRCDIR)/%.yaml,$(DOCDIR)/%.md,$(EXTRA_YAML))

EXTRA_TARGETS= \
    $(EXTRA_C_TARGETS) \
    $(EXTRA_H_TARGETS) \
    $(EXTRA_OBJTYPES_H_TARGETS) \
    $(EXTRA_DEBUG_H_TARGETS) \
    $(EXTRA_DEBUG_C_TARGETS) \
	$(GENDIR)/UnicodeData.inc \
	$(GENDIR)/UnicodeDigits.inc \
	$(GENDIR)/anf_kont.h \
	$(GENDIR)/anf_kont.c \
	$(GENDIR)/anf_kont_objtypes.h \
	$(GENDIR)/anf_kont_debug.h \
	$(GENDIR)/anf_kont_debug.c \
	$(GENDIR)/anf_kont_impl.inc \
	$(GENDIR)/cps_kont.h \
	$(GENDIR)/cps_kont.c \
	$(GENDIR)/cps_kont_objtypes.h \
	$(GENDIR)/cps_kont_debug.h \
	$(GENDIR)/cps_kont_debug.c \
	$(GENDIR)/cps_kont_impl.h \
	$(GENDIR)/cps_kont_impl.c

MAIN=$(SRCDIR)/main.c
PREAMBLE=$(GENDIR)/preamble.c
CFILES=$(filter-out $(MAIN), $(wildcard $(SRCDIR)/*.c))
EXTRA_CFILES=$(EXTRA_C_TARGETS) $(EXTRA_DEBUG_C_TARGETS) $(GENDIR)/anf_kont.c $(GENDIR)/anf_kont_debug.c $(GENDIR)/cps_kont.c $(GENDIR)/cps_kont_debug.c $(GENDIR)/cps_kont_impl.c

TEST_CFILES=$(wildcard $(TSTDIR)/src/*.c)

TEST_TARGETS=$(patsubst $(TSTDIR)/src/%.c,$(TSTDIR)/%,$(TEST_CFILES))

TEST_SUCCESSES=$(patsubst $(TSTDIR)/%,$(TSTDIR)/.%-success,$(TEST_TARGETS))

MAIN_OBJ=$(OBJDIR)/main.o
PREAMBLE_OBJ=$(OBJDIR)/preamble.o
OBJ=$(patsubst $(SRCDIR)/%,$(OBJDIR)/%,$(patsubst %.c,%.o,$(CFILES)))
TEST_OBJ=$(patsubst $(TSTDIR)/src/%,$(OBJDIR)/%,$(patsubst %.c,%.o,$(TEST_CFILES)))

MAIN_DEP=$(DEPDIR)/main.d
PREAMBLE_DEP=$(DEPDIR)/preamble.d
DEP=$(patsubst $(OBJDIR)/%,$(DEPDIR)/%,$(patsubst %.o,%.d,$(OBJ)))
TEST_DEP=$(patsubst $(OBJDIR)/%,$(DEPDIR)/%,$(patsubst %.o,%.d,$(TEST_OBJ)))

EXTRA_OBJ=$(patsubst $(GENDIR)/%,$(OBJDIR)/%,$(patsubst %.c,%.o,$(EXTRA_CFILES)))
EXTRA_DEP=$(patsubst $(OBJDIR)/%,$(DEPDIR)/%,$(patsubst %.o,%.d,$(EXTRA_OBJ)))

ALL_OBJ=$(OBJ) $(EXTRA_OBJ) $(PREAMBLE_OBJ)
ALL_DEP=$(DEP) $(EXTRA_DEP) $(TEST_DEP) $(MAIN_DEP) $(PREAMBLE_DEP)

INCLUDE_PATHS=-I $(GENDIR)/ -I $(SRCDIR)/

all: $(TARGET) docs

$(TARGET): $(MAIN_OBJ) $(ALL_OBJ) | $(BINDIR)
	$(CC) -o $@ $(MAIN_OBJ) $(ALL_OBJ) $(LIBS)

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

$(PREAMBLE): $(PREAMBLE_SRC) | $(GENDIR)
	tools/make-preamble.sh

$(EXTRA_C_TARGETS): $(GENDIR)/%.c: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< c > $@ || (rm -f $@ ; exit 1)

$(EXTRA_H_TARGETS): $(GENDIR)/%.h: $(SRCDIR)/%.yaml $(GENDEPS) | $(GENDIR)
	$(MAKE_AST) $< h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_OBJTYPES_H_TARGETS): $(GENDIR)/%_objtypes.h: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< objtypes_h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DEBUG_H_TARGETS): $(GENDIR)/%_debug.h: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_h > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DEBUG_C_TARGETS): $(GENDIR)/%_debug.c: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_c > $@ || (rm -f $@ ; exit 1)

# ANF continuation scaffolding generation (from tools/anf_continuations.yaml)
$(GENDIR)/anf_kont.h: tools/anf_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< h > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/anf_kont.c: tools/anf_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< c > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/anf_kont_objtypes.h: tools/anf_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< objtypes_h > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/anf_kont_debug.h: tools/anf_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_h > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/anf_kont_debug.c: tools/anf_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_c > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/anf_kont_impl.inc: tools/anf_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< kont_impl_inc > $@ || (rm -f $@ ; exit 1)

# CPS continuation scaffolding generation (from tools/cps_continuations.yaml)
$(GENDIR)/cps_kont.h: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< h > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/cps_kont.c: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< c > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/cps_kont_objtypes.h: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< objtypes_h > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/cps_kont_debug.h: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_h > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/cps_kont_debug.c: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_c > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/cps_kont_impl.h: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< kont_impl_h > $@ || (rm -f $@ ; exit 1)

$(GENDIR)/cps_kont_impl.c: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< kont_impl_c > $@ || (rm -f $@ ; exit 1)

$(EXTRA_DOCS): $(DOCDIR)/%.md: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(DOCDIR)
	$(MAKE_AST) $< md > $@ || (rm -f $@ ; exit 1)

.generated: $(EXTRA_TARGETS)
	touch $@

tags: $(SRCDIR)/* $(EXTRA_TARGETS)
	ctags $(SRCDIR)/* $(EXTRA_TARGETS)

xref: $(SRCDIR)/* $(EXTRA_TARGETS)
	ctags -x $(SRCDIR)/* $(EXTRA_TARGETS) > $@

$(MAIN_OBJ) $(OBJ): $(OBJDIR)/%.o: $(SRCDIR)/%.c | $(OBJDIR)
	$(CC) $(INCLUDE_PATHS) -c $< -o $@

$(EXTRA_OBJ) $(PREAMBLE_OBJ): $(OBJDIR)/%.o: $(GENDIR)/%.c | $(OBJDIR)
	$(CC) $(INCLUDE_PATHS) -c $< -o $@

$(TEST_OBJ): $(OBJDIR)/%.o: $(TSTDIR)/src/%.c | $(OBJDIR)
	$(LAXCC) $(INCLUDE_PATHS) -c $< -o $@

$(MAIN_DEP) $(DEP): $(DEPDIR)/%.d: $(SRCDIR)/%.c .generated | $(DEPDIR)
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst $(DEPDIR)/%,$(OBJDIR)/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(EXTRA_DEP) $(PREAMBLE_DEP): $(DEPDIR)/%.d: $(GENDIR)/%.c .generated | $(DEPDIR)
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst $(DEPDIR)/%,$(OBJDIR)/%,$(patsubst %.d,%.o,$@)) -o $@ $<

$(TEST_DEP): $(DEPDIR)/%.d: $(TSTDIR)/src/%.c .generated | $(DEPDIR)
	$(CC) $(INCLUDE_PATHS) -MM -MT $(patsubst $(DEPDIR)/%,$(OBJDIR)/%,$(patsubst %.d,%.o,$@)) -o $@ $<

test: $(TEST_TARGETS) $(TARGET) $(UNIDIR)/unicode.db
	for t in $(TSTDIR)/fn/test_*.fn ; do echo '***' $$t '***' ; ./$(TARGET) --include=fn --assertions-accumulate $$t || exit 1 ; done
	for t in $(TSTDIR)/fn/fail_*.fn ; do echo '***' $$t '(expect to see an error) ***' ; ! ./$(TARGET) --include=fn --assertions-accumulate $$t || exit 1 ; done
	for t in $(TEST_TARGETS) ; do echo '***' $$t '***' ; $$t || exit 1 ; done
	@echo "All tests passed."

$(TEST_TARGETS): $(TSTDIR)/%: $(OBJDIR)/%.o $(ALL_OBJ)
	$(CC) -o $@ $< $(ALL_OBJ) $(LIBS)

$(DEPDIR) $(OBJDIR) $(BINDIR) $(GENDIR) $(DOCDIR) $(UNIDIR):
	mkdir -p $@

install-sqlite3:
	sudo apt-get --yes install sqlite3 libsqlite3-dev

$(UNIDIR)/unicode.db: $(UNIDIR)/UnicodeData.csv ./tools/create_table.sql
	rm -f $@
	sqlite3 $@ < ./tools/create_table.sql

$(UNIDIR)/UnicodeData.csv: $(UNIDIR)/UnicodeData.txt ./tools/convertCsv.py
	$(PYTHON) ./tools/convertCsv.py

$(UNIDIR)/UnicodeData.txt: | $(UNIDIR)
	wget -P $(UNIDIR) https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt

$(GENDIR)/UnicodeData.inc: $(UNIDIR)/UnicodeData.txt tools/analyzeCsv.py | $(GENDIR)
	$(PYTHON) ./tools/analyzeCsv.py > $@

$(GENDIR)/UnicodeDigits.inc: $(UNIDIR)/UnicodeData.txt tools/makeUnicodeDigits.py | $(GENDIR)
	$(PYTHON) ./tools/makeUnicodeDigits.py > $@

realclean: clean
	rm -rf tags xref $(UNIDIR)

clean: deps
	rm -rf $(BINDIR) $(OBJDIR) callgrind.out.* $(GENDIR) $(TEST_TARGETS) .typedefs $(SRCDIR)/*~ .generated gmon.out *.fnc core.* coverage_html coverage_report.txt gcov_output *.gcda *.gcno coverage.info coverage_filtered.info test_output.log

deps:
	rm -rf $(DEPDIR)

PROF_SRC=fib20

profile: all
	rm -f callgrind.out.*
	./$(TARGET) --binary-out=$(PROF_SRC).fnc fn/$(PROF_SRC).fn
	valgrind --tool=callgrind ./$(TARGET) --binary-in=$(PROF_SRC).fnc

# Profile the compiler pipeline (scanner/parser → typechecker → bytecode), no VM run
profile-compile: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET) --binary-out=/dev/null fn/$(PROF_SRC).fn

# Convenience: annotate the most recent callgrind output
profile-annotate:
	callgrind_annotate --auto=yes callgrind.out.* | head -n 120

# Profile parser-only (stop after AST parse)
profile-parse: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET) --parse-only fn/$(PROF_SRC).fn

leak-check: all
	valgrind --leak-check=full ./$(TARGET) fn/$(PROF_SRC).fn

indent: indent-src indent-generated

indent-src: .typedefs .indent.pro
	indent `cat .typedefs | sort -u | xargs` $(EXTRA_INDENT_ARGS) $(SRCDIR)/*.[ch]
	rm -f $(SRCDIR)/*~

indent-generated: .typedefs .indent.pro
	indent `cat .typedefs | sort -u | xargs` $(EXTRA_INDENT_ARGS) $(GENDIR)/*.[ch]
	rm -f $(GENDIR)/*~

.typedefs: .generated

list-cores:
	@ls -rt1 /var/lib/apport/coredump/* | tail -1

coverage:
	./tools/coverage.sh

extracov: $(TEST_TARGETS) $(TARGET) $(UNIDIR)/unicode.db
	./$(TARGET) --include=fn --dump-anf --dump-ast --dump-bytecode --dump-inline --dump-lambda --dump-tpmc=NOT tests/fn/test_macros.fn 2>&1 > /dev/null

view-coverage:
	firefox --new-tab coverage_html/index.html

# Code generation refactoring test targets
BASELINE_DIR=test_baseline

establish-baseline:
	@echo "Establishing baseline for code generation testing..."
	@echo "WARNING: This will regenerate all code and create a snapshot."
	@echo "Press Ctrl-C to cancel, or Enter to continue..."
	@read dummy
	$(MAKE) clean
	$(MAKE) MODE=testing
	mkdir -p $(BASELINE_DIR)/generated
	mkdir -p $(BASELINE_DIR)/src
	cp -r $(GENDIR)/* $(BASELINE_DIR)/generated/
	cp $(SRCDIR)/*.yaml $(BASELINE_DIR)/src/
	find $(BASELINE_DIR)/generated -type f \( -name "*.h" -o -name "*.c" \) | \
		sort | xargs md5sum > $(BASELINE_DIR)/checksums.txt
	git rev-parse HEAD > $(BASELINE_DIR)/commit.txt
	date -u > $(BASELINE_DIR)/timestamp.txt
	@echo ""
	@echo "Baseline established in $(BASELINE_DIR)/"
	@echo "Commit: $$(cat $(BASELINE_DIR)/commit.txt)"
	@echo "Files: $$(find $(BASELINE_DIR)/generated -type f | wc -l)"
	@echo ""
	@echo "Add to .gitignore if not tracking baselines in git"

test-refactoring:
	@./tools/test_refactoring.sh

update-baseline:
	@echo "Updating baseline with current generated code..."
	@echo "WARNING: This will replace the baseline. Only do this if you've"
	@echo "verified the new output is correct!"
	@echo "Press Ctrl-C to cancel, or Enter to continue..."
	@read dummy
	rm -rf $(BASELINE_DIR)/generated
	mkdir -p $(BASELINE_DIR)/generated
	cp -r $(GENDIR)/* $(BASELINE_DIR)/generated/
	find $(BASELINE_DIR)/generated -type f \( -name "*.h" -o -name "*.c" \) | \
		sort | xargs md5sum > $(BASELINE_DIR)/checksums.txt
	git rev-parse HEAD > $(BASELINE_DIR)/commit.txt
	date -u > $(BASELINE_DIR)/timestamp.txt
	@echo "Baseline updated."

clean-baseline:
	@echo "Removing baseline directory..."
	rm -rf $(BASELINE_DIR)
	@echo "Baseline removed."

# vim: set noet sw=4: