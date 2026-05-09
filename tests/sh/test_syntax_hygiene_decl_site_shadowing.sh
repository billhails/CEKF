#!/usr/bin/env bash

set -eu

cd "$(dirname "$0")/../.."

output=$(./bin/fn --include=fn --dump-ast tests/fn/test_syntax_hygiene_decl_site_shadowing.fn)
body=$(printf '%s\n' "$output" | sed -n '/^body: {$/,$p')

helper_names=$(printf '%s\n' "$body" | sed -n 's/^[[:space:]]*\(helper\$[0-9][0-9]*\) = fn .*$/\1/p')
outer_helper=$(printf '%s\n' "$helper_names" | sed -n '1p')
inner_helper=$(printf '%s\n' "$helper_names" | sed -n '2p')

test -n "$outer_helper"
test -n "$inner_helper"
test "$outer_helper" != "$inner_helper"

printf '%s\n' "$body" | grep -F "$outer_helper(41);" >/dev/null

if printf '%s\n' "$body" | grep -F "$inner_helper(41);" >/dev/null; then
    printf 'syntax lowering captured the use-site helper instead of the declaration-site helper\n' >&2
    exit 1
fi

if printf '%s\n' "$body" | grep -E 'syntax-decl|syntax-use-expr|template\[' >/dev/null; then
    printf 'syntax carriers survived lowering in dump-ast output\n' >&2
    exit 1
fi