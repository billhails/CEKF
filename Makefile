.PHONY: all clean realclean deps profile leak-check check-grammar \
list-cores test indent indent-src indent-generated docs \
install-sqlite3 coverage extracov view-coverage \
coverage-target test-binary test-big-binary help \
establish-baseline test-refactoring update-baseline clean-baseline \
scratch

# pass on the command line, i.e. `make test MODE=prod`
#
# dev:        -g, -DSAFETY_CHECKS (indirectly via common.h)
# prod:       -O2
# coverage:   -g -DSAFETY_CHECKS -lgcov
ifndef MODE
MODE:=dev
endif

BINDIR=bin
DEPDIR=dep
DOCDIR=docs/generated
GENDIR=generated
OBJDIR=obj
SRCDIR=src
FNDIR=fn
TSTDIR=tests
UNIDIR=unicode

TARGET=$(BINDIR)/fn

ifeq ($(MODE),dev)
	CCMODE:= -g
	EXTRA_DEFINES:= -DBUILD_MODE=0
else ifeq ($(MODE),prod)
	CCMODE:= -O2
	EXTRA_DEFINES:= -DPRODUCTION_BUILD -DBUILD_MODE=1
else ifeq ($(MODE),coverage)
	CCMODE:= -g --coverage
	EXTRA_DEFINES:= -DBUILD_MODE=2
	LIBS+= -lgcov
else
$(error invalid MODE=$(MODE), allowed values: dev, coverage or prod)
endif

ifndef CCC
CCC:=clang
WEXTRA=-Werror=implicit-fallthrough
endif

CC:=$(CCC) -Wall -Wextra -Werror $(WEXTRA) $(CCMODE) $(EXTRA_DEFINES)
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

TEST_FN_DIR=tests/fn
FNDIR=fn
TMPDIR=tmp
TEST_FN_FILES=$(wildcard $(TEST_FN_DIR)/test_*.fn)
TEST_FN_CFILES=$(patsubst $(TEST_FN_DIR)/%,$(TMPDIR)/%,$(patsubst %.fn,%.c,$(TEST_FN_FILES)))
TEST_FN_SFILES=$(patsubst $(TEST_FN_DIR)/%,$(TMPDIR)/%,$(patsubst %.fn,%.scm,$(TEST_FN_FILES)))
TEST_FN_BFILES=$(patsubst $(TEST_FN_DIR)/%,$(TMPDIR)/%,$(patsubst %.fn,%.fnc,$(TEST_FN_FILES)))
TEST_FN_OFILES=$(patsubst %.c,%.o,$(TEST_FN_CFILES))
TEST_FN_BINARIES=$(patsubst %.o,%,$(TEST_FN_OFILES))

FN_FILES=$(wildcard $(FNDIR)/*.fn)
FN_CFILES=$(patsubst $(FNDIR)/%,$(TMPDIR)/%,$(patsubst %.fn,%.c,$(FN_FILES)))
FN_SFILES=$(patsubst $(FNDIR)/%,$(TMPDIR)/%,$(patsubst %.fn,%.scm,$(FN_FILES)))
FN_BFILES=$(patsubst $(FNDIR)/%,$(TMPDIR)/%,$(patsubst %.fn,%.fnc,$(FN_FILES)))
FN_OFILES=$(patsubst %.c,%.o,$(FN_CFILES))
FN_BINARIES=$(patsubst %.o,%,$(FN_OFILES))

TARGET_ARGS=--include=fn --flat-closure

$(TEST_FN_CFILES): $(TMPDIR)/%.c: $(TEST_FN_DIR)/%.fn $(TARGET) | $(TMPDIR)
	$(TARGET) $(TARGET_ARGS) --target-c=$@~ $<  && mv $@~ $@
	indent $@
	rm -f $@~

$(FN_CFILES): $(TMPDIR)/%.c: $(FNDIR)/%.fn $(TARGET) | $(TMPDIR)
	$(TARGET) $(TARGET_ARGS) --target-c=$@~ $< && mv $@~ $@
	indent $@
	rm -f $@~

$(TEST_FN_BFILES): $(TMPDIR)/%.fnc: $(TEST_FN_DIR)/%.fn $(TARGET) | $(TMPDIR)
	$(TARGET) --binary-out=$@~ $<  && mv $@~ $@

$(FN_BFILES): $(TMPDIR)/%.fnc: $(FNDIR)/%.fn $(TARGET) | $(TMPDIR)
	$(TARGET) --binary-out=$@~ $<  && mv $@~ $@

$(TEST_FN_SFILES): $(TMPDIR)/%.scm: $(TEST_FN_DIR)/%.fn $(TARGET) | $(TMPDIR)
	$(TARGET) $(TARGET_ARGS) --dump-ir $<  > $@~ && mv $@~ $@

$(FN_SFILES): $(TMPDIR)/%.scm: $(FNDIR)/%.fn $(TARGET) | $(TMPDIR)
	$(TARGET) $(TARGET_ARGS) --dump-ir $<  > $@~ && mv $@~ $@

$(FN_OFILES) $(TEST_FN_OFILES): %.o: %.c
	$(LAXCC) $(INCLUDE_PATHS) -c $< -o $@

$(FN_BINARIES) $(TEST_FN_BINARIES): %: %.o $(ALL_OBJ)
	$(LAXCC) -o $@ $< $(ALL_OBJ) $(LIBS)

test-binary: all $(TEST_FN_BINARIES)
	@for t in $(TEST_FN_BINARIES) ; do echo $$t ; $$t || exit 1 ; done
	@echo All binary tests pass

irs: $(TEST_FN_SFILES)

test-big-binary: all tmp/test_harness
	tmp/test_harness

tmp/test_harness: tmp/test_harness.o $(ALL_OBJ)
	$(LAXCC) -o $@ $< $(ALL_OBJ) $(LIBS)

tmp/test_harness.o: tmp/test_harness.c
	$(LAXCC) $(INCLUDE_PATHS) -c $< -o $@

tmp/test_harness.c: $(FNDIR)/rewrite/test_harness.fn $(TARGET)
	$(TARGET) --target-c=$@~ $< && mv $@~ $@
	indent $@
	rm -f $@~

tmp/test_harness.fnc: $(FNDIR)/rewrite/test_harness.fn $(TARGET)
	$(TARGET) --binary-out=$@~ $<  && mv $@~ $@

PERF_CASE=fib35
test-perf-binary: $(TMPDIR)/$(PERF_CASE)
	time $(TMPDIR)/$(PERF_CASE)

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
	$(MAKE_AST) $< c > $@~ && mv $@~ $@

$(EXTRA_H_TARGETS): $(GENDIR)/%.h: $(SRCDIR)/%.yaml $(GENDEPS) | $(GENDIR)
	$(MAKE_AST) $< h > $@~ && mv $@~ $@

$(EXTRA_OBJTYPES_H_TARGETS): $(GENDIR)/%_objtypes.h: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< objtypes_h > $@~ && mv $@~ $@

$(EXTRA_DEBUG_H_TARGETS): $(GENDIR)/%_debug.h: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_h > $@~ && mv $@~ $@

$(EXTRA_DEBUG_C_TARGETS): $(GENDIR)/%_debug.c: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_c > $@~ && mv $@~ $@

# generic continuation scaffolding generation (from tools/*_continuations.yaml)
$(GENDIR)/%_kont.h: tools/%_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< h > $@~ && mv $@~ $@

$(GENDIR)/%_kont.c: tools/%_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< c > $@~ && mv $@~ $@

$(GENDIR)/%_kont_objtypes.h: tools/%_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< objtypes_h > $@~ && mv $@~ $@

$(GENDIR)/%_kont_debug.h: tools/%_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_h > $@~ && mv $@~ $@

$(GENDIR)/%_kont_debug.c: tools/%_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< debug_c > $@~ && mv $@~ $@

# ANF-specific continuation scaffolding generation (from tools/anf_continuations.yaml)
$(GENDIR)/anf_kont_impl.inc: tools/anf_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< kont_impl_inc > $@~ && mv $@~ $@

# CPS-specific continuation scaffolding generation (from tools/cps_continuations.yaml)
$(GENDIR)/cps_kont_impl.h: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< kont_impl_h > $@~ && mv $@~ $@

$(GENDIR)/cps_kont_impl.c: tools/cps_continuations.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml $(SRCDIR)/minlam.yaml | $(GENDIR)
	$(MAKE_AST) $< kont_impl_c > $@~ && mv $@~ $@


$(EXTRA_DOCS): $(DOCDIR)/%.md: $(SRCDIR)/%.yaml $(GENDEPS) $(SRCDIR)/primitives.yaml | $(DOCDIR)
	$(MAKE_AST) $< md > $@~ && mv $@~ $@

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
	for t in $(TSTDIR)/fn/fail_*.fn ; do echo '***' $$t '***' ; ! ./$(TARGET) --include=fn --assertions-accumulate $$t >/dev/null 2>&1 || exit 1 ; done
	for t in $(TEST_TARGETS) ; do echo '***' $$t '***' ; $$t || exit 1 ; done
	@echo "All tests passed."

$(TEST_TARGETS): $(TSTDIR)/%: $(OBJDIR)/%.o $(ALL_OBJ)
	$(CC) -o $@ $< $(ALL_OBJ) $(LIBS)

$(DEPDIR) $(OBJDIR) $(BINDIR) $(GENDIR) $(DOCDIR) $(UNIDIR) $(TMPDIR) :
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

$(GENDIR)/UnicodeData.inc: $(UNIDIR)/UnicodeData.txt tools/makeUnicodeData.py | $(GENDIR)
	$(PYTHON) ./tools/makeUnicodeData.py > $@

$(GENDIR)/UnicodeDigits.inc: $(UNIDIR)/UnicodeData.txt tools/makeUnicodeDigits.py | $(GENDIR)
	$(PYTHON) ./tools/makeUnicodeDigits.py > $@

realclean: clean
	rm -rf tags xref $(UNIDIR)

clean: deps
	rm -rf $(BINDIR) $(OBJDIR) callgrind.out.* $(GENDIR) $(TEST_TARGETS) .typedefs $(SRCDIR)/*~ .generated gmon.out *.fnc core.* coverage_html coverage_report.txt gcov_output *.gcda *.gcno coverage.info coverage_filtered.info test_output.log $(TEST_FN_CFILES) $(TEST_FN_OFILES) $(TEST_FN_BINARIES) $(TEST_FN_SFILES) $(TMPDIR)
	$(MAKE) -C scratch clean

deps:
	rm -rf $(DEPDIR)

PROF_SRC=fib20

profile: all
	rm -f callgrind.out.*
	./$(TARGET) --binary-out=$(PROF_SRC).fnc $(FNDIR)/$(PROF_SRC).fn
	valgrind --tool=callgrind ./$(TARGET) --binary-in=$(PROF_SRC).fnc

# Profile the compiler pipeline (scanner/parser → typechecker → bytecode), no VM run
profile-compile: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET) --binary-out=/dev/null $(FNDIR)/$(PROF_SRC).fn

# Profile a compiled C target
profile-compiled: all tmp/test_harness
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./tmp/test_harness

# Convenience: annotate the most recent callgrind output
profile-annotate:
	callgrind_annotate --auto=yes callgrind.out.* | head -n 120

# Profile parser-only (stop after AST parse)
profile-parse: all
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./$(TARGET) --parse-only $(FNDIR)/$(PROF_SRC).fn

leak-check: all
	valgrind --leak-check=full ./$(TARGET) $(FNDIR)/$(PROF_SRC).fn

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

help:
	@echo "CEKF Make targets"
	@echo ""
	@echo "Coverage"
	@echo "  make coverage"
	@echo "      Run full coverage workflow (build + test suite + report generation)."
	@echo ""
	@echo "  make coverage-target COVERAGE_TARGET=tests/test_minlam_freeVars [COVERAGE_FILE=src/minlam_freeVars.c]"
	@echo "      Run focused coverage on one executable target."
	@echo "      Uses gcov when compiler is GCC, llvm-cov gcov when compiler is Clang."
	@echo ""
	@echo "  make view-coverage"
	@echo "      Open HTML coverage report when generated."
	@echo ""
	@echo "Coverage variables"
	@echo "  CCC=<compiler>        e.g. clang (default) or gcc"
	@echo "  COVERAGE_TARGET=<bin> e.g. tests/test_minlam_freeVars"
	@echo "  COVERAGE_FILE=<src>   e.g. src/minlam_freeVars.c"
	@echo "  LLVM_COV=<path>       Override llvm-cov binary"
	@echo "  GCOV=<path>           Override gcov binary"

coverage:
	./tools/coverage.sh

coverage-target:
	@if [ -z "$(COVERAGE_TARGET)" ]; then \
		echo "Usage: make coverage-target COVERAGE_TARGET=tests/test_minlam_freeVars [COVERAGE_FILE=src/minlam_freeVars.c]"; \
		exit 1; \
	fi
	find obj -name '*.gcda' -delete
	find obj -name '*.gcno' -delete
	$(MAKE) -B MODE=coverage $(COVERAGE_TARGET)
	./$(COVERAGE_TARGET)
	mkdir -p gcov_output
	@COV_GCOV_CMD=""; \
	COMPILER_BASENAME="$$(basename "$(CCC)")"; \
	case "$$COMPILER_BASENAME" in \
		gcc*|*-gcc|g++*|*-g++) \
		if [ -n "$$GCOV" ]; then \
			COV_GCOV_CMD="$$GCOV"; \
		elif command -v gcov >/dev/null 2>&1; then \
			COV_GCOV_CMD="gcov"; \
		else \
			echo "ERROR: gcov not found. Set GCOV=<path-to-gcov>."; \
			exit 1; \
		fi; \
		;; \
		*) \
		if [ -n "$$LLVM_COV" ]; then \
			COV_GCOV_CMD="$$LLVM_COV gcov"; \
		elif command -v llvm-cov-18 >/dev/null 2>&1; then \
			COV_GCOV_CMD="llvm-cov-18 gcov"; \
		elif command -v llvm-cov >/dev/null 2>&1; then \
			COV_GCOV_CMD="llvm-cov gcov"; \
		else \
			echo "ERROR: llvm-cov not found. Set LLVM_COV=<path-to-llvm-cov>."; \
			exit 1; \
		fi; \
		;; \
	esac; \
	if [ -n "$(COVERAGE_FILE)" ]; then \
		gcda="obj/$$(basename $(COVERAGE_FILE) .c).gcda"; \
		if [ ! -f "$$gcda" ]; then \
			echo "ERROR: $$gcda not found. Run target may not have touched $(COVERAGE_FILE)."; \
			exit 1; \
		fi; \
		echo "Generating focused coverage for $(COVERAGE_FILE)"; \
		eval "$$COV_GCOV_CMD -b \"$$gcda\""; \
	else \
		echo "Generating coverage for all touched gcda files"; \
		for f in obj/*.gcda; do eval "$$COV_GCOV_CMD -b \"$$f\" >/dev/null"; done; \
		echo "Done. Set COVERAGE_FILE=src/<file>.c for focused output."; \
	fi; \
	mv ./*.gcov gcov_output/ 2>/dev/null || true

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
	$(MAKE)
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

scratch:
	$(MAKE) -C scratch run

# vim: set noet sw=4: