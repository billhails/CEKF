#!/bin/bash
# Coverage analysis script for CEKF tests
# Generates both HTML reports and AI-readable text summaries

set -e

echo "=== CEKF Test Coverage Analysis ==="
echo ""

# Clean previous coverage data
echo "Cleaning previous coverage data..."
find . -name "*.gcda" -delete
find . -name "*.gcno" -delete
rm -rf coverage_html coverage_report.txt gcov_output

# Rebuild with coverage flags
echo "Rebuilding with coverage instrumentation..."
make clean
make MODE=coverage

# Run all tests
echo "Running test suite..."
make test MODE=coverage 2>&1 | tee test_output.log || true
make extracov MODE=coverage 2>&1 | tee -a test_output.log || true

# Create output directories
mkdir -p gcov_output coverage_html

# Generate coverage data with llvm-cov (clang-compatible)
echo "Generating coverage data with llvm-cov..."
echo "=== LLVM-COV COVERAGE SUMMARY ===" > coverage_report.txt
echo "" >> coverage_report.txt

# Process each .gcda file and capture summary output
cd src
total_lines_exec=0
total_lines_total=0
total_branches_exec=0
total_branches_total=0

for objfile in ../obj/*.gcda; do
    if [ -f "$objfile" ]; then
        basename_obj=$(basename "$objfile")
        echo "Processing $basename_obj..." >&2
        
        # Run llvm-cov and capture output
        output=$(llvm-cov-18 gcov -b "$objfile" 2>&1)
        
        # Extract file summaries and append to report
        echo "$output" | grep -A4 "^File " | while IFS= read -r line; do
            echo "$line" >> ../coverage_report.txt
        done
    fi
done

cd ..

# Move gcov files to output directory  
mv src/*.gcov gcov_output/ 2>/dev/null || true

# Parse the coverage report to calculate overall statistics
echo "" >> coverage_report.txt
echo "=== OVERALL STATISTICS ===" >> coverage_report.txt
echo "" >> coverage_report.txt

# Extract lines and branches from coverage_report.txt (excluding generated files)
total_lines_covered=0
total_lines_total=0
total_branches_covered=0
total_branches_total=0

while IFS= read -r line; do
    # Skip generated files
    if [[ "$line" =~ ^File\ \'generated/ ]] || [[ "$line" =~ ^File\ \'../generated/ ]]; then
        skip_next=5
        continue
    fi
    
    if [ ${skip_next:-0} -gt 0 ]; then
        skip_next=$((skip_next - 1))
        continue
    fi
    
    # Parse "Lines executed:82.44% of 689"
    if [[ "$line" =~ Lines\ executed:([0-9.]+)%\ of\ ([0-9]+) ]]; then
        pct="${BASH_REMATCH[1]}"
        total="${BASH_REMATCH[2]}"
        covered=$(awk "BEGIN {printf \"%.0f\", ($pct * $total / 100)}")
        total_lines_covered=$((total_lines_covered + covered))
        total_lines_total=$((total_lines_total + total))
    fi
    
    # Parse "Branches executed:92.38% of 210"  
    if [[ "$line" =~ Branches\ executed:([0-9.]+)%\ of\ ([0-9]+) ]]; then
        pct="${BASH_REMATCH[1]}"
        total="${BASH_REMATCH[2]}"
        covered=$(awk "BEGIN {printf \"%.0f\", ($pct * $total / 100)}")
        total_branches_covered=$((total_branches_covered + covered))
        total_branches_total=$((total_branches_total + total))
    fi
done < coverage_report.txt

if [ $total_lines_total -gt 0 ]; then
    line_coverage_pct=$(awk "BEGIN {printf \"%.2f\", ($total_lines_covered/$total_lines_total)*100}")
    echo "Total executable lines: $total_lines_total" >> coverage_report.txt
    echo "Covered lines: $total_lines_covered" >> coverage_report.txt
    echo "Line coverage: $line_coverage_pct%" >> coverage_report.txt
    echo "" >> coverage_report.txt
fi

if [ $total_branches_total -gt 0 ]; then
    branch_coverage_pct=$(awk "BEGIN {printf \"%.2f\", ($total_branches_covered/$total_branches_total)*100}")
    echo "Total branches: $total_branches_total" >> coverage_report.txt
    echo "Covered branches: $total_branches_covered" >> coverage_report.txt
    echo "Branch coverage: $branch_coverage_pct%" >> coverage_report.txt
else
    echo "No coverage data collected" >> coverage_report.txt
fi

# Generate HTML report with gcovr
echo ""
echo "Generating HTML report with gcovr..."

# Create wrapper script for llvm-cov
cat > /tmp/llvm-gcov.sh << 'EOF'
#!/bin/bash
exec llvm-cov-18 gcov "$@"
EOF
chmod +x /tmp/llvm-gcov.sh

if gcovr --gcov-executable /tmp/llvm-gcov.sh \
    --exclude 'generated/.*' \
    --exclude 'tests/.*' \
    --exclude 'tools/.*' \
    --html-details coverage_html/index.html \
    --print-summary 2>&1 | tee -a coverage_report.txt; then
    echo ""
    echo "  HTML report generated successfully"
else
    echo ""
    echo "  gcovr failed, HTML generation skipped"
fi

echo ""
echo "=== REPORTS GENERATED ==="
echo "  Text Report: coverage_report.txt"
echo "  Gcov Files: gcov_output/*.gcov"
if [ -f "coverage_html/index.html" ]; then
    echo "  HTML Report: coverage_html/index.html"
fi
echo ""
echo "Coverage Summary:"
cat coverage_report.txt | tail -5
echo ""
