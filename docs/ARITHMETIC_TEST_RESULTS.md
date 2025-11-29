# Mixed-Type Arithmetic Test Results

## Summary

Created comprehensive tests for mixed-type arithmetic operations including:
- BigInt + Float operations
- Complex number operations (com_mag, com_theta)
- Cross-type comparisons
- Subtraction of different numeric types
- Power operations with all numeric type combinations

## Bug Discovered

**Critical Bug: Comparison operators don't handle mixed numeric types**

- **Error**: `different types in _cmp VALUE_TYPE_STDINT vs VALUE_TYPE_IRRATIONAL`
- **Location**: `src/step.c:324`
- **Impact**: Cannot compare integers with floats, rationals with floats, or any cross-type numeric comparisons
- **Test Case**: `tests/fn/bug_compare_int_float.fn`

### Examples that crash:
```fn
3 < 3.5          // int < float - CRASHES
2 < (5/2)        // int < rational - CRASHES  
(1/2) < 0.75     // rational < float - CRASHES
1000 > 123.456   // bigint > float - CRASHES
```

## What Works

✅ **Mixed-type addition**:
- `bigint + float` → `float` (with precision limits)
- `float + bigint` → `float`
- `rational + float` → `float`
- `float + rational` → `float`

✅ **Complex number operations**:
- `com_mag(complex)` - Returns magnitude
- `com_theta(complex)` - Returns angle/argument
- Complex number creation: `r + (i * 1i)`

✅ **Integer operations**:
- `int ** int` - Integer exponentiation
- `0 ** 0` - Returns 1 (by convention)

✅ **Complex exponentiation** (doesn't crash):
- `complex ** int`
- `complex ** float`
- `int ** complex`

## What's Blocked by the Bug

❌ All comparison operations between different numeric types
❌ Subtraction tests (can't verify results due to comparison bug)
❌ Most power operation tests (can't verify results due to comparison bug)

## Test File Status

**Active**: `tests/fn/test_arithmetic_mixed_types.fn`
- Tests what works (addition, complex ops, basic power)
- Many tests commented out due to comparison bug

**Bug Documentation**: `tests/fn/bug_compare_int_float.fn`
- Minimal reproduction case for the comparison bug

## Coverage Impact

The comparison bug prevents testing of:
- Cross-type numeric comparisons (6 test functions)
- Subtraction verification (7 test functions) 
- Power operation verification (13 test functions)

**Total**: 26 test functions blocked by this bug

## Observations

1. **Float precision loss**: Adding small floats to very large bigints loses precision
   - `1000000000000000000000 + 3.14` → `1000000000000000000000.0` (exact)
   - Need to use smaller bigints to preserve float precision

2. **Type promotion works**: Mixed operations correctly promote to appropriate types
   - Operations involving floats promote to float
   - Operations preserving precision stay as rationals

3. **Complex numbers functional**: Basic complex operations work without crashing

## Recommendation

The comparison bug should be prioritized - it blocks a significant amount of arithmetic functionality testing and likely affects real-world usage where comparing numbers of different types is common.
