/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * Minimal AST after desugaring
 * Generated from src/minlam.yaml by tools/generate.py
 */

#include "memory.h"
#include "minlam.h"
#include "minlam_pp.h"
#include "symbol.h"
#include "utils_helper.h"

#include "minlam_uncurry.h"

#ifdef DEBUG_MINLAM_UNCURRY
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

static MinAmb *uncurryMinAmb(MinAmb *node, IntMap *context);
static MinExp *uncurryMinApply(MinApply *node, IntMap *context);
static MinCharCondCases *uncurryMinCharCondCases(MinCharCondCases *node,
                                                 IntMap *context);
static MinCondCases *uncurryMinCondCases(MinCondCases *node, IntMap *context);
static MinCond *uncurryMinCond(MinCond *node, IntMap *context);
static MinExprList *uncurryMinExprList(MinExprList *node, IntMap *context);
static MinExp *uncurryMinExp(MinExp *node, IntMap *context);
static MinIff *uncurryMinIff(MinIff *node, IntMap *context);
static MinIntCondCases *uncurryMinIntCondCases(MinIntCondCases *node,
                                               IntMap *context);
static MinLam *uncurryMinLam(MinLam *node, IntMap *context);
static MinLetRec *uncurryMinLetRec(MinLetRec *node, IntMap *context);
static MinMatchList *uncurryMinMatchList(MinMatchList *node, IntMap *context);
static MinMatch *uncurryMinMatch(MinMatch *node, IntMap *context);
static MinPrimApp *uncurryMinPrimApp(MinPrimApp *node, IntMap *context);

////////////////////
// Helper functions
////////////////////

static IntMap *deleteListFromIntMap(SymbolList *list, IntMap *map) {
    // Do NOT optimize for an empty list, always return a copy.
    IntMap *result = newIntMap();
    int save = PROTECT(result);
    HashSymbol *key = NULL;
    Index i = 0;
    int arity = 0;
    while ((key = iterateIntMap(map, &i, &arity)) != NULL) {
        if (!symbolInList(key, list)) {
            setIntMap(result, key, arity);
        }
    }
    UNPROTECT(save);
    return result;
}

static IntMap *copyIntMap(IntMap *map) {
    return deleteListFromIntMap(NULL, map);
}

//  fn collect_arities {
//      (#(name, #(expr, arity)) @ bindings, env) {
//          D.insert(name, arity, collect_arities(bindings, env))
//      }
//      ([], env) { env }
//  }

static void collectArities(MinBindings *bindings, IntMap *map) {
    MinBindings *current = bindings;
    while (current != NULL) {
        setIntMap(map, current->var, current->arity);
        current = current->next;
    }
}

//  fn flatten_lambda {
//      (M.lambda([arg], body)) {
//          switch (flatten_lambda(body)) {
//              (M.lambda(args, body)) {
//                  M.lambda(arg @ args, body)
//              }
//              (body) {
//                  M.lambda([arg], body)
//              }
//          }
//      }
//      (body) { body }
//  }

static MinExp *flattenLambda(MinExp *exp) {
    if (isMinExp_Lam(exp)) {
        MinLam *lam = getMinExp_Lam(exp);
        if (countSymbolList(lam->args) == 1) {
            MinExp *flat = flattenLambda(lam->exp);
            int save = PROTECT(flat);
            MinExp *res = NULL;
            if (isMinExp_Lam(flat)) {
                SymbolList *args =
                    newSymbolList(CPI(lam->args), lam->args->symbol,
                                  getMinExp_Lam(flat)->args);
                PROTECT(args);
                res = makeMinExp_Lam(CPI(args), args, getMinExp_Lam(flat)->exp);
            } else {
                res = makeMinExp_Lam(CPI(lam), lam->args, flat);
            }
            UNPROTECT(save);
            return res;
        } else {
            return exp;
        }
    } else {
        return exp;
    }
}

//  fn make_arity_name(name, arity) {
//      name @@ "$arity_" @@ $arity;
//  }

static HashSymbol *makeArityName(HashSymbol *name, Integer arity) {
    SCharVec *buf = newSCharVec(128 + strlen(name->name));
    int save = PROTECT(buf);
    sprintf(buf->entries, "%s$arity_%d", name->name, arity);
    HashSymbol *result = newSymbol(buf->entries);
    UNPROTECT(save);
    return result;
}

//  fn extend_bindings {
//      (#(name, #(expr, arity)) @ bindings, env) {
//          let
//              rest_bindings = extend_bindings(bindings, env);
//              newExpr = unc(expr, env);
//          in
//              if (arity > 1) {
//                  #(make_arity_name(name, arity),
//                      #(flatten_lambda(newExpr), arity)) @
//                  #(name, #(newExpr, arity)) @ rest_bindings
//              } else {
//                  #(name, #(newExpr, arity)) @ rest_bindings
//              }
//      }
//      ([], _) { [] }
//  }

static MinBindings *extendBindings(MinBindings *bindings, IntMap *context) {
    if (bindings == NULL) {
        return NULL;
    }
    MinBindings *next = extendBindings(bindings->next, context);
    int save = PROTECT(next);
    MinExp *newExp = uncurryMinExp(bindings->val, context);
    PROTECT(newExp);
    MinBindings *this =
        newMinBindings(CPI(bindings), bindings->var, newExp, next);
    PROTECT(this);
    this->arity = bindings->arity; // probably unnecessary
    if (bindings->arity > 1) {
        HashSymbol *arityName = makeArityName(bindings->var, bindings->arity);
        MinExp *flat = flattenLambda(newExp);
        PROTECT(flat);
        MinBindings *extra =
            newMinBindings(CPI(bindings), arityName, flat, this);
        PROTECT(extra);
        extra->arity = bindings->arity; // probably unnecessary
        UNPROTECT(save);
        return extra;
    } else {
        UNPROTECT(save);
        return this;
    }
}

//  fn flatten_apply {
//      (M.apply(op, [arg]), args_rev) {
//          flatten_apply(op, u(arg) @ args_rev)
//      }
//      (head, args) {
//          #(head, args)
//      }
//  }

static MinApply *flattenApply(MinApply *apply, IntMap *context) {
    if (isMinExp_Apply(apply->function)) {
        MinApply *inner =
            getMinExp_Apply(apply->function); // M.apply(op, [arg])
        if (countMinExprList(inner->args) == 1) {
            MinExp *arg = uncurryMinExp(inner->args->exp, context);
            int save = PROTECT(arg);
            MinExprList *args = newMinExprList(CPI(arg), arg, apply->args);
            PROTECT(args);
            MinApply *new =
                newMinApply(CPI(inner->function), inner->function, args);
            PROTECT(new);
            MinApply *result = flattenApply(new, context);
            UNPROTECT(save);
            return result;
        }
    }
    return apply;
}

//  fn make_curried_application {
//      (expr, []) {
//          expr
//      }
//      (expr, [arg]) {
//          M.apply(expr, [arg])
//      }
//      (expr, arg @ args) {
//          make_curried_application(M.apply(expr, [arg]), args)
//      }
//  }

static MinExp *makeCurriedApplicationHelper(MinExp *exp, MinExprList *args) {
    switch (countMinExprList(args)) {
    case 0:
        return exp;
    case 1:
        return makeMinExp_Apply(CPI(exp), exp, args);
    default: {
        MinExprList *arg = newMinExprList(CPI(args), args->exp, NULL);
        int save = PROTECT(arg);
        MinExp *apply = makeMinExp_Apply(CPI(exp), exp, arg);
        MinExp *result = makeCurriedApplicationHelper(apply, args->next);
        UNPROTECT(save);
        return result;
    }
    }
}

static MinExp *makeCurriedApplication(MinExp *exp, MinExprList *args) {
    if (countMinExprList(args) == 0) {
        return makeMinExp_Apply(CPI(exp), exp, args);
    } else {
        return makeCurriedApplicationHelper(exp, args);
    }
}

//  fn make_saturated_application(name, args) {
//      M.apply(M.var(make_arity_name(name, len args)), args)
//  }

static MinExp *makeSaturatedApplication(HashSymbol *name, MinExprList *args) {
    HashSymbol *satName = makeArityName(name, countMinExprList(args));
    MinExp *var = newMinExp_Var(CPI(args), satName);
    int save = PROTECT(var);
    MinExp *result = makeMinExp_Apply(CPI(args), var, args);
    UNPROTECT(save);
    return result;
}

// TODO consider generating this
static MinExprList *takeMinExprList(MinExprList *list, int count) {
    if (count <= 0)
        return NULL;
    if (list == NULL)
        return NULL;
    MinExprList *rest = takeMinExprList(list->next, count - 1);
    int save = PROTECT(rest);
    MinExprList *this = newMinExprList(CPI(list), list->exp, rest);
    UNPROTECT(save);
    return this;
}

// TODO consider generating this
static MinExprList *dropMinExprList(MinExprList *list, int count) {
    if (count <= 0)
        return list;
    if (list == NULL)
        return NULL;
    return dropMinExprList(list->next, count - 1);
}

//  fn process_apply(head, args) {
//      switch (head) {
//          (M.var(s)) {
//              switch (D.lookup(s, env)) {
//                  (just(arity)) {
//                      if (arity > 1) {
//                          switch (arity <=> len args) {
//                              (gt) {
//                                  make_curried_application(M.var(s), args)
//                              }
//                              (eq) {
//                                  make_saturated_application(s, args)
//                              }
//                              (lt) {
//                                  let
//                                      mainargs = args take arity;
//                                      restargs = args drop arity;
//                                  in
//                                      make_curried_application(
//                                          make_saturated_application(s,
//                                          mainargs), restargs
//                                      )
//                              }
//                          }
//                      } else {
//                          make_curried_application(M.var(s), args)
//                      }
//                  }
//                  (nothing) {
//                      make_curried_application(M.var(s), args)
//                  }
//              }
//          }
//          (x) {
//              make_curried_application(u(x), args)
//          }
//      }
//  }

static MinExp *processApply(MinApply *apply, IntMap *context) {
    MinExp *head = apply->function;
    MinExprList *args = apply->args;
    if (isMinExp_Var(head)) {
        int arity = 0;
        HashSymbol *var = getMinExp_Var(head);
        if (getIntMap(context, var, &arity)) {
            if (arity > 1) {
                int len = countMinExprList(args);
                if (len == arity) {
                    return makeSaturatedApplication(var, args);
                }
                if (len > arity) {
                    MinExprList *mainArgs = takeMinExprList(args, arity);
                    int save = PROTECT(mainArgs);
                    MinExprList *restArgs = dropMinExprList(args, arity);
                    PROTECT(restArgs);
                    MinExp *mainExp = makeSaturatedApplication(var, mainArgs);
                    PROTECT(mainExp);
                    MinExp *restExp = makeCurriedApplication(mainExp, restArgs);
                    UNPROTECT(save);
                    return restExp;
                }
            }
        }
        return makeCurriedApplication(head, args);
    } else {
        MinExp *u = uncurryMinExp(head, context);
        int save = PROTECT(u);
        MinExp *result = makeCurriedApplication(u, args);
        UNPROTECT(save);
        return result;
    }
}

///////////////////////////
// Visitor implementations
///////////////////////////

//  (M.lambda(args, body)) {
//      M.lambda(args, unc(body, D.delete_list(args, env)))
//  }

static MinLam *uncurryMinLam(MinLam *node, IntMap *context) {
    ENTER(uncurryMinLam);
    if (node == NULL) {
        LEAVE(uncurryMinLam);
        return NULL;
    }
    IntMap *new_context = deleteListFromIntMap(node->args, context);
    int save = PROTECT(new_context);
    bool changed = false;
    MinExp *new_exp = uncurryMinExp(node->exp, new_context);
    PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinLam *result = node;
    if (changed) {
        result = newMinLam(CPI(node), node->args, new_exp);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinLam);
    return result;
}

//  args |> u

static MinExprList *uncurryMinExprList(MinExprList *node, IntMap *context) {
    ENTER(uncurryMinExprList);
    if (node == NULL) {
        LEAVE(uncurryMinExprList);
        return NULL;
    }
    bool changed = false;
    MinExp *new_exp = uncurryMinExp(node->exp, context);
    int save = PROTECT(new_exp);
    changed = changed || (new_exp != node->exp);
    MinExprList *new_next = uncurryMinExprList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinExprList *result = node;
    if (changed) {
        result = newMinExprList(CPI(node), new_exp, new_next);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinExprList);
    return result;
}

//  (M.apply(x=M.primop(_), args)) {
//      M.apply(x, args |> u)
//  }

static MinPrimApp *uncurryMinPrimApp(MinPrimApp *node, IntMap *context) {
    ENTER(uncurryMinPrimApp);
    if (node == NULL) {
        LEAVE(uncurryMinPrimApp);
        return NULL;
    }
    bool changed = false;
    MinExp *new_exp1 = uncurryMinExp(node->exp1, context);
    int save = PROTECT(new_exp1);
    changed = changed || (new_exp1 != node->exp1);
    MinExp *new_exp2 = uncurryMinExp(node->exp2, context);
    PROTECT(new_exp2);
    changed = changed || (new_exp2 != node->exp2);
    MinPrimApp *result = node;
    if (changed) {
        result = newMinPrimApp(CPI(node), node->type, new_exp1, new_exp2);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinPrimApp);
    return result;
}

//  switch (apply) {
//      (M.apply(expr, [arg])) {
//          switch (flatten_apply(M.apply(expr, [arg]), [])) {
//              (#(head, args)) {
//                  process_apply(head, args)
//              }
//          }
//      }
//      (M.apply(expr, args)) {
//          make_curried_application(u(expr), args |> u)
//      }
//  }

static MinExp *uncurryMinApply(MinApply *node, IntMap *context) {
    ENTER(uncurryMinApply);
    if (node == NULL) {
        LEAVE(uncurryMinApply);
        return NULL;
    }
    if (node->isBuiltin) {
        MinExprList *args = uncurryMinExprList(node->args, context);
        int save = PROTECT(args);
        MinExp *apply = makeMinExp_Apply(CPI(node), node->function, args);
        getMinExp_Apply(apply)->isBuiltin = true;
        UNPROTECT(save);
        return apply;
    }
    if (countMinExprList(node->args) == 1) {
        MinExp *function = newMinExp_Apply(CPI(node), node);
        int save = PROTECT(function);
        MinApply *wrapper = newMinApply(CPI(function), function, NULL);
        PROTECT(wrapper);
        MinApply *apply = flattenApply(wrapper, context);
        PROTECT(apply);
        MinExp *exp = processApply(apply, context);
        UNPROTECT(save);
        return exp;
    } else {
        MinExp *u = uncurryMinExp(node->function, context);
        int save = PROTECT(u);
        MinExprList *args = uncurryMinExprList(node->args, context);
        PROTECT(args);
        MinExp *result = makeCurriedApplication(u, args);
        UNPROTECT(save);
        return result;
    }
}

//  (M.if_expr(e1, e2, e3)) {
//      M.if_expr(u(e1), u(e2), u(e3))
//  }

static MinIff *uncurryMinIff(MinIff *node, IntMap *context) {
    ENTER(uncurryMinIff);
    if (node == NULL) {
        LEAVE(uncurryMinIff);
        return NULL;
    }
    bool changed = false;
    MinExp *new_condition = uncurryMinExp(node->condition, context);
    int save = PROTECT(new_condition);
    changed = changed || (new_condition != node->condition);
    MinExp *new_consequent = uncurryMinExp(node->consequent, context);
    PROTECT(new_consequent);
    changed = changed || (new_consequent != node->consequent);
    MinExp *new_alternative = uncurryMinExp(node->alternative, context);
    PROTECT(new_alternative);
    changed = changed || (new_alternative != node->alternative);
    MinIff *result = node;
    if (changed) {
        result = newMinIff(CPI(node), new_condition, new_consequent,
                           new_alternative);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinIff);
    return result;
}

//  (M.cond_expr(e, cases)) {
//      M.cond_expr(u(e), cases |> identity && u)
//  }

static MinCond *uncurryMinCond(MinCond *node, IntMap *context) {
    ENTER(uncurryMinCond);
    if (node == NULL) {
        LEAVE(uncurryMinCond);
        return NULL;
    }
    bool changed = false;
    MinExp *new_value = uncurryMinExp(node->value, context);
    int save = PROTECT(new_value);
    changed = changed || (new_value != node->value);
    MinCondCases *new_cases = uncurryMinCondCases(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinCond *result = node;
    if (changed) {
        result = newMinCond(CPI(node), new_value, new_cases);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinCond);
    return result;
}

static MinCondCases *uncurryMinCondCases(MinCondCases *node, IntMap *context) {
    ENTER(uncurryMinCondCases);
    if (node == NULL) {
        LEAVE(uncurryMinCondCases);
        return NULL;
    }
    int save = PROTECT(NULL);
    MinCondCases *result = node;
    switch (node->type) {
    case MINCONDCASES_TYPE_INTEGERS: {
        // MinIntCondCases
        MinIntCondCases *variant = getMinCondCases_Integers(node);
        MinIntCondCases *new_variant = uncurryMinIntCondCases(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Integers(CPI(node), new_variant);
        }
        break;
    }
    case MINCONDCASES_TYPE_CHARACTERS: {
        // MinCharCondCases
        MinCharCondCases *variant = getMinCondCases_Characters(node);
        MinCharCondCases *new_variant =
            uncurryMinCharCondCases(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinCondCases_Characters(CPI(node), new_variant);
        }
        break;
    }
    default:
        cant_happen("unrecognized MinCondCases type %d", node->type);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinCondCases);
    return result;
}

static MinIntCondCases *uncurryMinIntCondCases(MinIntCondCases *node,
                                               IntMap *context) {
    ENTER(uncurryMinIntCondCases);
    if (node == NULL) {
        LEAVE(uncurryMinIntCondCases);
        return NULL;
    }
    bool changed = false;
    MinExp *new_body = uncurryMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinIntCondCases *new_next = uncurryMinIntCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinIntCondCases *result = node;
    if (changed) {
        result =
            newMinIntCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinIntCondCases);
    return result;
}

static MinCharCondCases *uncurryMinCharCondCases(MinCharCondCases *node,
                                                 IntMap *context) {
    ENTER(uncurryMinCharCondCases);
    if (node == NULL) {
        LEAVE(uncurryMinCharCondCases);
        return NULL;
    }
    bool changed = false;
    MinExp *new_body = uncurryMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinCharCondCases *new_next = uncurryMinCharCondCases(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinCharCondCases *result = node;
    if (changed) {
        result =
            newMinCharCondCases(CPI(node), node->constant, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinCharCondCases);
    return result;
}

//  (M.match_cases(e, cases)) {
//      M.match_cases(u(e), cases |> identity && u)
//  }

static MinMatch *uncurryMinMatch(MinMatch *node, IntMap *context) {
    ENTER(uncurryMinMatch);
    if (node == NULL) {
        LEAVE(uncurryMinMatch);
        return NULL;
    }
    bool changed = false;
    MinExp *new_index = uncurryMinExp(node->index, context);
    int save = PROTECT(new_index);
    changed = changed || (new_index != node->index);
    MinMatchList *new_cases = uncurryMinMatchList(node->cases, context);
    PROTECT(new_cases);
    changed = changed || (new_cases != node->cases);
    MinMatch *result = node;
    if (changed) {
        result = newMinMatch(CPI(node), new_index, new_cases);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinMatch);
    return result;
}

// list |> identity && u

static MinMatchList *uncurryMinMatchList(MinMatchList *node, IntMap *context) {
    ENTER(uncurryMinMatchList);
    if (node == NULL) {
        LEAVE(uncurryMinMatchList);
        return NULL;
    }
    bool changed = false;
    MinExp *new_body = uncurryMinExp(node->body, context);
    int save = PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinMatchList *new_next = uncurryMinMatchList(node->next, context);
    PROTECT(new_next);
    changed = changed || (new_next != node->next);
    MinMatchList *result = node;
    if (changed) {
        result = newMinMatchList(CPI(node), node->matches, new_body, new_next);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinMatchList);
    return result;
}

//  (M.letrec_expr(bindings, body)) {
//      let
//          newEnv = collect_arities(bindings, env);
//          newBindings = extend_bindings(bindings, newEnv)
//      in
//          M.letrec_expr(newBindings, unc(body, newEnv))
//  }

static MinLetRec *uncurryMinLetRec(MinLetRec *node, IntMap *context) {
    ENTER(uncurryMinLetRec);
    if (node == NULL) {
        LEAVE(uncurryMinLetRec);
        return NULL;
    }
    IntMap *new_context = copyIntMap(context);
    int save = PROTECT(new_context);
    bool changed = false;
    collectArities(node->bindings, new_context);
    MinBindings *new_bindings = extendBindings(node->bindings, new_context);
    PROTECT(new_bindings);
    changed = changed || (new_bindings != node->bindings);
    MinExp *new_body = uncurryMinExp(node->body, new_context);
    PROTECT(new_body);
    changed = changed || (new_body != node->body);
    MinLetRec *result = node;
    if (changed) {
        result = newMinLetRec(CPI(node), new_bindings, new_body);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinLetRec);
    return result;
}

//  (M.amb_expr(e1, e2)) {
//      M.amb_expr(u(e1), u(e2))
//  }

static MinAmb *uncurryMinAmb(MinAmb *node, IntMap *context) {
    ENTER(uncurryMinAmb);
    if (node == NULL) {
        LEAVE(uncurryMinAmb);
        return NULL;
    }
    bool changed = false;
    MinExp *new_left = uncurryMinExp(node->left, context);
    int save = PROTECT(new_left);
    changed = changed || (new_left != node->left);
    MinExp *new_right = uncurryMinExp(node->right, context);
    PROTECT(new_right);
    changed = changed || (new_right != node->right);
    MinAmb *result = node;
    if (changed) {
        result = newMinAmb(CPI(node), new_left, new_right);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinAmb);
    return result;
}

static MinExp *uncurryMinExp(MinExp *node, IntMap *context) {
    ENTER(uncurryMinExp);
    if (node == NULL) {
        LEAVE(uncurryMinExp);
        return NULL;
    }
    int save = PROTECT(NULL);
    MinExp *result = node;
    switch (node->type) {
    case MINEXP_TYPE_AMB: {
        // MinAmb
        MinAmb *variant = getMinExp_Amb(node);
        MinAmb *new_variant = uncurryMinAmb(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Amb(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_APPLY: {
        // MinApply
        MinApply *variant = getMinExp_Apply(node);
        result = uncurryMinApply(variant, context);
        PROTECT(result);
        // ppMinExp(result);
        // eprintf("\n");
        break;
    }
    case MINEXP_TYPE_BACK: {
        // void_ptr
        break;
    }
    case MINEXP_TYPE_BIGINTEGER: {
        // MaybeBigInt
        break;
    }
    case MINEXP_TYPE_CALLCC: {
        // MinExp
        MinExp *variant = getMinExp_CallCC(node);
        MinExp *new_variant = uncurryMinExp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_CallCC(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_CHARACTER: {
        // character
        break;
    }
    case MINEXP_TYPE_COND: {
        // MinCond
        MinCond *variant = getMinExp_Cond(node);
        MinCond *new_variant = uncurryMinCond(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Cond(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_IFF: {
        // MinIff
        MinIff *variant = getMinExp_Iff(node);
        MinIff *new_variant = uncurryMinIff(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Iff(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LAM: {
        // MinLam
        MinLam *variant = getMinExp_Lam(node);
        MinLam *new_variant = uncurryMinLam(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Lam(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_LETREC: {
        // MinLetRec
        MinLetRec *variant = getMinExp_LetRec(node);
        MinLetRec *new_variant = uncurryMinLetRec(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_LetRec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MAKEVEC: {
        // MinExprList
        MinExprList *variant = getMinExp_MakeVec(node);
        MinExprList *new_variant = uncurryMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_MakeVec(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_MATCH: {
        // MinMatch
        MinMatch *variant = getMinExp_Match(node);
        MinMatch *new_variant = uncurryMinMatch(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Match(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_PRIM: {
        // MinPrimApp
        MinPrimApp *variant = getMinExp_Prim(node);
        MinPrimApp *new_variant = uncurryMinPrimApp(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Prim(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_SEQUENCE: {
        // MinExprList
        MinExprList *variant = getMinExp_Sequence(node);
        MinExprList *new_variant = uncurryMinExprList(variant, context);
        if (new_variant != variant) {
            PROTECT(new_variant);
            result = newMinExp_Sequence(CPI(node), new_variant);
        }
        break;
    }
    case MINEXP_TYPE_STDINT: {
        // int
        break;
    }
    case MINEXP_TYPE_VAR: {
        // HashSymbol
        break;
    }
    default:
        cant_happen("unrecognized MinExp type %d", node->type);
    }
    UNPROTECT(save);
    LEAVE(uncurryMinExp);
    return result;
}

//////////////////////
// Public Interface
//////////////////////

MinExp *uncurry(MinExp *exp) {
    IntMap *context = newIntMap();
    int save = PROTECT(context);
    MinExp *result = uncurryMinExp(exp, context);
    UNPROTECT(save);
    return result;
}