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

printf '%s\n' "$body" | grep -F "                $inner_x;" >/dev/null

if printf '%s\n' "$body" | grep -F "                $outer_x;" >/dev/null; then
    printf 'syntax lowering captured declaration-site x instead of use-site x\n' >&2
    exit 1
fi

if printf '%s\n' "$body" | grep -E 'syntax-decl|syntax-use-expr|template\[' >/dev/null; then
    printf 'syntax carriers survived lowering in dump-ast output\n' >&2
    exit 1
fi