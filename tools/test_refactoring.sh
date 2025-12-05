#!/bin/bash
#
# CEKF - VM supporting amb
# Copyright (C) 2022-2025  Bill Hails
#
# Test script for code generation refactoring
# Compares newly generated code against baseline to ensure no regressions
#

set -e  # Exit on any error

BASELINE_DIR="${BASELINE_DIR:-test_baseline}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=== Testing Code Generation Refactoring ==="
echo "Baseline: $BASELINE_DIR"
echo "Current:  generated/"
echo ""

# Check if baseline exists
if [ ! -d "$BASELINE_DIR/generated" ]; then
    echo -e "${RED}ERROR: Baseline directory not found: $BASELINE_DIR/generated${NC}"
    echo "Run: make establish-baseline"
    exit 1
fi

# Clean and regenerate
echo "Cleaning..."
make clean > /dev/null 2>&1

echo "Regenerating code..."
if ! make MODE=testing > /dev/null 2>&1; then
    echo -e "${RED}ERROR: Code generation failed${NC}"
    exit 1
fi

# Compare generated files
echo "Comparing generated code..."
DIFFERENCES=0
TOTAL_FILES=0

for file in "$BASELINE_DIR/generated"/*.{h,c}; do
    [ -e "$file" ] || continue  # Skip if glob doesn't match
    
    basename=$(basename "$file")
    TOTAL_FILES=$((TOTAL_FILES + 1))
    
    if [ ! -f "generated/$basename" ]; then
        echo -e "${RED}✗ Missing file: generated/$basename${NC}"
        DIFFERENCES=$((DIFFERENCES + 1))
        continue
    fi
    
    # Compare files (ignoring whitespace differences)
    # -w: ignore whitespace
    # -B: ignore blank lines
    if ! diff -w -B "$file" "generated/$basename" > /dev/null 2>&1; then
        echo -e "${RED}✗ Difference found in $basename${NC}"
        DIFFERENCES=$((DIFFERENCES + 1))
        
        # Show first few lines of diff
        echo "  First 20 lines of diff:"
        diff -u -w -B "$file" "generated/$basename" | head -20 | sed 's/^/  /'
        echo "  ..."
    else
        echo -e "${GREEN}✓${NC} $basename"
    fi
done

echo ""
echo "Checked $TOTAL_FILES files"

if [ $DIFFERENCES -gt 0 ]; then
    echo -e "${RED}=== FAILED: $DIFFERENCES file(s) differ from baseline ===${NC}"
    echo ""
    echo "To investigate differences:"
    echo "  diff -u $BASELINE_DIR/generated/<file> generated/<file>"
    echo ""
    echo "If differences are intentional:"
    echo "  make update-baseline"
    exit 1
fi

echo -e "${GREEN}✓ All generated files match baseline!${NC}"
echo ""

# Run actual tests to ensure functionality
echo "Running test suite..."
if make test > /dev/null 2>&1; then
    echo -e "${GREEN}✓ All tests pass!${NC}"
else
    echo -e "${RED}✗ Tests failed${NC}"
    exit 1
fi

echo ""
echo -e "${GREEN}=== Refactoring test PASSED ===${NC}"
