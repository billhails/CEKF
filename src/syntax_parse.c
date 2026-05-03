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
 */

#include "syntax_parse.h"
#include "memory.h"
#include "pratt.h"
#include "pratt_scanner.h"

PrattMacroSpec *prattFindLocalSyntaxSpecForHead(PrattParser *parser,
                                                HashSymbol *head) {
    Index i = 0;
    PrattMacroSpec *spec = NULL;
    while (iteratePrattMacroTable(parser->macros, &i, &spec) != NULL) {
        if (spec != NULL && spec->isExprEntry && spec->headSymbol == head) {
            return spec;
        }
    }
    return NULL;
}

PrattMacroSpec *prattFindSyntaxSpecByName(PrattParser *parser,
                                          HashSymbol *name) {
    PrattMacroSpec *spec = NULL;
    getPrattMacroTable(parser->macros, name, &spec);
    if (spec != NULL) {
        return spec;
    } else if (parser->next != NULL) {
        return prattFindSyntaxSpecByName(parser->next, name);
    } else {
        return NULL;
    }
}

PrattMacroSpec *prattFindSyntaxSpecForHead(PrattParser *parser,
                                           HashSymbol *head) {
    PrattMacroSpec *spec = prattFindLocalSyntaxSpecForHead(parser, head);
    if (spec != NULL) {
        return spec;
    } else if (parser->next != NULL) {
        return prattFindSyntaxSpecForHead(parser->next, head);
    } else {
        return NULL;
    }
}

static bool syntaxPatternNameEquivalent(HashSymbol *left, HashSymbol *right,
                                        SymbolArray *leftNames,
                                        SymbolArray *rightNames,
                                        Index mappedCount) {
    bool rightMapped = false;

    for (Index i = 0; i < mappedCount; ++i) {
        if (getSymbolArray(leftNames, i) == left) {
            return getSymbolArray(rightNames, i) == right;
        }
        if (getSymbolArray(rightNames, i) == right) {
            rightMapped = true;
        }
    }

    if (rightMapped) {
        return false;
    }

    return left == right;
}

static bool recordSyntaxPatternNameMap(HashSymbol *left, HashSymbol *right,
                                       SymbolArray *leftNames,
                                       SymbolArray *rightNames,
                                       Index *mappedCount) {
    for (Index i = 0; i < *mappedCount; ++i) {
        if (getSymbolArray(leftNames, i) == left) {
            return getSymbolArray(rightNames, i) == right;
        }
        if (getSymbolArray(rightNames, i) == right) {
            return false;
        }
    }

    pushSymbolArray(leftNames, left);
    pushSymbolArray(rightNames, right);
    (*mappedCount)++;
    return true;
}

static bool syntaxCallArgumentsEquivalent(SymbolArray *left, SymbolArray *right,
                                          SymbolArray *leftNames,
                                          SymbolArray *rightNames,
                                          Index mappedCount) {
    if (left == NULL || right == NULL) {
        return left == right;
    }

    if (sizeSymbolArray(left) != sizeSymbolArray(right)) {
        return false;
    }

    for (Index i = 0; i < sizeSymbolArray(left); ++i) {
        if (!syntaxPatternNameEquivalent(getSymbolArray(left, i),
                                         getSymbolArray(right, i), leftNames,
                                         rightNames, mappedCount)) {
            return false;
        }
    }

    return true;
}

static bool syntaxPatternItemsMatch(PrattMacroPatternItems *leftItems,
                                    PrattMacroPatternItems *rightItems,
                                    bool requireRightLonger) {
    Index leftCount = countPrattMacroPatternItems(leftItems);
    Index rightCount = countPrattMacroPatternItems(rightItems);

    if (requireRightLonger) {
        if (leftCount >= rightCount) {
            return false;
        }
    } else if (leftCount != rightCount) {
        return false;
    }

    SymbolArray *leftNames = newSymbolArray();
    int save = PROTECT(leftNames);
    SymbolArray *rightNames = newSymbolArray();
    PROTECT(rightNames);
    bool matched = true;
    Index mappedCount = 0;

    for (Index i = 0; i < leftCount; ++i) {
        PrattMacroPatternItem *leftItem =
            getPrattMacroPatternItems(leftItems, i);
        PrattMacroPatternItem *rightItem =
            getPrattMacroPatternItems(rightItems, i);
        if (leftItem->type != rightItem->type) {
            matched = false;
            break;
        }

        if (leftItem->type == PRATTMACROPATTERNITEM_TYPE_QUOTEDTERMINAL) {
            if (getPrattMacroPatternItem_QuotedTerminal(leftItem) !=
                getPrattMacroPatternItem_QuotedTerminal(rightItem)) {
                matched = false;
                break;
            }
            continue;
        }

        PrattMacroHole *leftHole = getPrattMacroPatternItem_TypedHole(leftItem);
        PrattMacroHole *rightHole =
            getPrattMacroPatternItem_TypedHole(rightItem);

        if (leftHole->syntaxClass != rightHole->syntaxClass ||
            leftHole->callTarget != rightHole->callTarget ||
            !syntaxCallArgumentsEquivalent(leftHole->callArguments,
                                           rightHole->callArguments, leftNames,
                                           rightNames, mappedCount) ||
            !recordSyntaxPatternNameMap(leftHole->name, rightHole->name,
                                        leftNames, rightNames, &mappedCount)) {
            matched = false;
            break;
        }
    }

    UNPROTECT(save);
    return matched;
}

static bool syntaxAlternativesEquivalent(PrattMacroAlternative *left,
                                         PrattMacroAlternative *right) {
    return syntaxPatternItemsMatch(left->patternItems, right->patternItems,
                                   false);
}

static bool syntaxAlternativeShadows(PrattMacroAlternative *earlier,
                                     PrattMacroAlternative *later) {
    if (countPrattMacroPatternItems(earlier->patternItems) == 0) {
        return false;
    }

    return syntaxPatternItemsMatch(earlier->patternItems, later->patternItems,
                                   true);
}

void prattValidateSyntaxAlternatives(PrattParser *parser, HashSymbol *ruleName,
                                     PrattMacroAlternatives *alternatives) {
    bool sawEmpty = false;

    for (Index i = 0; i < sizePrattMacroAlternatives(alternatives); ++i) {
        PrattMacroAlternative *alternative =
            getPrattMacroAlternatives(alternatives, i);
        if (countPrattMacroPatternItems(alternative->patternItems) == 0) {
            if (sawEmpty) {
                parserError(parser,
                            "syntax %s has more than one empty alternative",
                            ruleName->name);
                return;
            }
            sawEmpty = true;
        }

        for (Index j = 0; j < i; ++j) {
            PrattMacroAlternative *earlier =
                getPrattMacroAlternatives(alternatives, j);
            if (syntaxAlternativesEquivalent(earlier, alternative)) {
                parserError(
                    parser,
                    "syntax %s alternative %ld duplicates alternative %ld",
                    ruleName->name, (long)(i + 1), (long)(j + 1));
                return;
            }
            if (syntaxAlternativeShadows(earlier, alternative)) {
                parserError(
                    parser,
                    "syntax %s alternative %ld is shadowed by alternative %ld",
                    ruleName->name, (long)(i + 1), (long)(j + 1));
                return;
            }
        }
    }
}
