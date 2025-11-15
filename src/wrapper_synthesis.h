#ifndef cekf_wrapper_synthesis_h
#define cekf_wrapper_synthesis_h

#include "ast.h"
#include "builtins_helper.h"

/**
 * Generate wrapper function definitions for each builtin so external names
 * become ordinary (curriable) user functions. Each wrapper is:
 *   fn <externalName>(a1, ..., an) { builtin$<externalName>(a1, ..., an) }
 * inserted into the program preamble definitions list (before existing ones).
 * Safe to call exactly once after parsing and builtin registration, before
 * lambda conversion.
 */
void generateBuiltinWrappers(BuiltIns *builtIns);
void markGeneratedBuiltins(void);
extern AstDefinitions *generatedBuiltins;

#endif
