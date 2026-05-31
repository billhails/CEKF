# Rewrite Status Update

Current status for the rewrite prototype is narrower and healthier than it looked earlier.

- `fn/rewrite/infer_expr.fn` now supports flat `typedefs(list(ctorScheme), expr)` constructor schemes for type inference.
- `construct` and `deconstruct` are both typed through those constructor schemes.
- The focused rewrite regression suite in `fn/rewrite/tests/test_infer_typedefs.fn` now covers user-defined type families including `maybe`, `either`, and recursive `list`.
- Those tests confirm concrete result typing and deconstruction of payload fields and recursive tails without changing the current flat `ctorScheme` representation.

What this means in practice:

- richer user-defined type heads such as `maybe('a)`, `either('a, 'b)`, and `list('a)` are already recognised by the rewrite type checker
- the immediate pressure is no longer on redesigning rewrite typedef syntax
- the useful next work should come from new failing type-checking scenarios, not from front-end typedef representation changes

Recommended validation path remains:

1. `make test-rewrite`
2. `./bin/fn fn/rewrite/test_harness.fn` for optional pipeline smoke coverage
3. `make test` after the focused rewrite slice is stable