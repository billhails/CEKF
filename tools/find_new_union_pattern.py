#!/usr/bin/env python3
"""
Find pattern: newType() followed by PROTECT then newUnion_Type(name)

This script searches for code patterns where a type is allocated, protected,
then wrapped in a union. These can potentially be simplified using make*()
functions.

Usage:
    python3 tools/find_new_union_pattern.py src/*.c
    python3 tools/find_new_union_pattern.py src/pratt_parser.c
"""

import re
import sys
from pathlib import Path


def find_pattern_in_file(filepath):
    """
    Find instances of the pattern:
        Type *name = newType(...);
        ...PROTECT...
        Union *name2 = newUnion_Type(name);
    
    Returns list of (line_number, context_lines) tuples.
    """
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    
    matches = []
    
    # Pattern for: Type *name = newType(...);
    # Flexible about whitespace, captures Type and name
    # Less restrictive - just looks for the assignment pattern
    new_pattern = re.compile(
        r'^\s*([A-Z][a-zA-Z0-9]*)\s*\*\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*new\1\s*\(',
        re.IGNORECASE
    )
    
    # Pattern for: Union *name2 = newUnion_Type(...);
    # Captures Union, Type, and looks for the original variable name anywhere on the line
    # Very permissive to handle complex argument lists like CPI(node), other, var)
    union_pattern = re.compile(
        r'^\s*([A-Z][a-zA-Z0-9]*)\s*\*\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*'
        r'new\1_([A-Z][a-zA-Z0-9]*)\s*\(',
        re.IGNORECASE
    )
    
    i = 0
    while i < len(lines):
        match = new_pattern.match(lines[i])
        if match:
            type_name = match.group(1)
            var_name = match.group(2)
            start_line = i
            
            # Look ahead for PROTECT and then newUnion_Type
            # Search within the next 4 lines (reasonable window)
            found_protect = False
            for j in range(i + 1, min(i + 5, len(lines))):
                if 'PROTECT' in lines[j]:
                    found_protect = True
                
                union_match = union_pattern.match(lines[j])
                if union_match and found_protect:
                    union_type = union_match.group(1)
                    union_var = union_match.group(2)
                    variant_type = union_match.group(3)
                    
                    # Check if the original variable name appears in this line
                    # (handles cases like newUnion_Type(CPI(x), otherargs, var))
                    # Don't check type matching - just look for the variable name
                    if re.search(r'\b' + re.escape(var_name) + r'\b', lines[j]):
                        # Found a match!
                        context = {
                            'start_line': start_line + 1,  # 1-indexed
                            'end_line': j + 1,
                            'type_name': type_name,
                            'var_name': var_name,
                            'union_type': union_type,
                            'union_var': union_var,
                            'context_lines': lines[start_line:j+1]
                        }
                        matches.append(context)
                        break
        i += 1
    
    return matches


def format_match(filepath, match):
    """Format a match for display."""
    output = []
    output.append(f"{filepath}:")
    for i, line in enumerate(match['context_lines'], start=match['start_line']):
        output.append(f"{i}: {line.rstrip()}")
    return '\n'.join(output)


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    
    for pattern in sys.argv[1:]:
        for filepath in Path('.').glob(pattern):
            if filepath.is_file() and filepath.suffix in ['.c', '.h']:
                matches = find_pattern_in_file(filepath)
                
                if matches:
                    for match in matches:
                        print(format_match(filepath, match))


if __name__ == '__main__':
    main()
