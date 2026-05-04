#!/usr/bin/env bash

set -eu

cd "$(dirname "$0")/../.."

output=$(./bin/fn --include=fn --dump-ast tests/fn/test_syntax_hygiene_unquote_shadowing.fn)
body=$(printf '%s\n' "$output" | sed -n '/^body: {$/,$p')

outer_x=$(printf '%s\n' "$body" | sed -n 's/^[[:space:]]*\(x\$[0-9][0-9]*\) = 10;$/\1/p' | sed -n '1p')
inner_x=$(printf '%s\n' "$body" | sed -n 's/^[[:space:]]*\(x\$[0-9][0-9]*\) = 20;$/\1/p' | sed -n '1p')

test -n "$outer_x"
test -n "$inner_x"
test "$outer_x" != "$inner_x"

printf '%s\n' "$body" | grep -F 'syntax-decl[1 captureUseSite ' >/dev/null
printf '%s\n' "$body" | grep -F 'syntax-use-expr[decl=1 alt=0] (' >/dev/null
printf '%s\n' "$body" | grep -F "y := $inner_x" >/dev/null

if printf '%s\n' "$body" | grep -F "y := $outer_x" >/dev/null; then
    printf 'unquote captured declaration-site x instead of use-site x\n' >&2
    exit 1
fi