#!/usr/bin/env bash

set -eu

cd "$(dirname "$0")/../.."

output=$(./bin/fn --include=fn --dump-ast tests/fn/test_syntax_def_shadowing.fn)
body=$(printf '%s\n' "$output" | sed -n '/^body: {$/,$p')

outer_name=$(printf '%s\n' "$body" | sed -n 's/^[[:space:]]*\(name\$[0-9][0-9]*\) = 100;$/\1/p')

test -n "$outer_name"

printf '%s\n' "$body" | grep -F "local = +" >/dev/null
printf '%s\n' "$body" | grep -F "(${outer_name}, 1);" >/dev/null

if printf '%s\n' "$body" | grep -E '^[[:space:]]*local\$[0-9]+' >/dev/null; then
    printf 'definition syntax lowered to a freshened local binder instead of the captured use-site name\n' >&2
    exit 1
fi

if printf '%s\n' "$body" | grep -E 'syntax-decl|syntax-use-def|template\[' >/dev/null; then
    printf 'definition syntax carriers survived lowering in dump-ast output\n' >&2
    exit 1
fi