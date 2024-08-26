/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2024  Bill Hails
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

#include <ctype.h>

#include"symbol.h"
#include "pratt_scanner.h"

static void reverseLexer(PrattLexer *lexer) {
    int i = 0;
    int j = lexer->size - 1;
    while (i < j) {
        PrattToken *tmp = lexer->entries[i];
        lexer->entries[i] = lexer->entries[j];
        lexer->entries[j] = tmp;
        i++;
        j--;
    }
}

static PrattToken *makePrattAtom(char **input) {
    char *start = *input;
    while (isalnum(**input)) {
        ++*input;
    }
    int size = *input - start;
    char *buf = NEW_ARRAY(char, size + 1);
    memcpy(buf, start, size);
    buf[size] = '\0';
    HashSymbol *symbol = newSymbol(buf);
    FREE_ARRAY(char, buf, size + 1);
    PrattToken *atom = newPrattToken_Atom(symbol);
    return atom;
}

static HashSymbol *lookupTrieRecursive(PrattTrie *trie,
                                       unsigned char **input,
                                       unsigned char *last,
                                       HashSymbol *found) {
    if (trie == NULL || **input > trie->character) {
        *input = last;
        return found;
    }
    if (**input < trie->character) {
        return lookupTrieRecursive(trie->siblings, input, last, found);
    }
    // trie matches
    ++*input;
    if (trie->terminal != NULL) {
        // avoid i.e. "orbit" false matching "or"
        if (!isalpha(trie->character) || !isalpha(**input)) {
            found = trie->terminal;
            last = *input;
        }
    }
    return lookupTrieRecursive(trie->children, input, last, found);
}

static PrattToken *lookupTrieSymbol(PrattTrie *trie, char **input) {
    HashSymbol *symbol = lookupTrieRecursive(trie, (unsigned char **)input, (unsigned char *)*input, NULL);
    if (symbol == NULL) {
        return NULL;
    }
    return newPrattToken_Op(symbol);
}

PrattLexer *makePrattLexer(PrattTrie *trie, char *input) {
    PrattLexer *lexer = newPrattLexer();
    int save = PROTECT(lexer);
    while(*input) {
        if(isspace(*input)) {
            ++input;
        } else if (isalpha(*input)) {
            PrattToken *atom = lookupTrieSymbol(trie, &input);
            if (atom == NULL) {
                atom = makePrattAtom(&input);
            }
            int save2 = PROTECT(atom);
            pushPrattLexer(lexer, atom);
            UNPROTECT(save2);
        } else if (isdigit(*input)) {
            PrattToken *number = newPrattToken_Number(*input - '0');
            int save2 = PROTECT(number);
            pushPrattLexer(lexer, number);
            UNPROTECT(save2);
            ++input;
        } else if (ispunct(*input)) {
            PrattToken *op = lookupTrieSymbol(trie, &input);
            if (op == NULL) {
                cant_happen("unrecognised operator %s", input);
            }
            int save2 = PROTECT(op);
            pushPrattLexer(lexer, op);
            UNPROTECT(save2);
        } else {
            cant_happen("unexpected %c", *input);
        }
    }
    PrattToken *eof = newPrattToken_Eof();
    PROTECT(eof);
    pushPrattLexer(lexer, eof);
    reverseLexer(lexer);
    UNPROTECT(save);
    return lexer;
}

static PrattTrie *makeTrie(HashSymbol *symbol, PrattTrie *siblings, unsigned char *bytes) {
    if (*bytes == 0) {
        return NULL;
    }
    PrattTrie *next = makeTrie(symbol, NULL, bytes + 1);
    if (next == NULL) {
        return newPrattTrie(*bytes, symbol, siblings, NULL);
    }
    int save = PROTECT(next);
    PrattTrie *this = newPrattTrie(*bytes, NULL, siblings, next);
    UNPROTECT(save);
    return this;
}

static PrattTrie *insertTrie(PrattTrie *current, HashSymbol *symbol, unsigned char *bytes) {
    if (*bytes == 0) {
        return NULL;
    }
    if (current == NULL) {
        return makeTrie(symbol, NULL, bytes);
    }
    if (*bytes < current->character) {
        PrattTrie *sibling = insertTrie(current->siblings, symbol, bytes);
        int save = PROTECT(sibling);
        PrattTrie *this = newPrattTrie(current->character, current->terminal, sibling, current->children);
        UNPROTECT(save);
        return this;
    }
    if (*bytes == current->character) {
        PrattTrie *child = insertTrie(current->children, symbol, bytes + 1);
        if (child == NULL) {
            return newPrattTrie(current->character, symbol, current->siblings, current->children);
        }
        int save = PROTECT(child);
        PrattTrie *this = newPrattTrie(current->character, current->terminal, current->siblings, child);
        UNPROTECT(save);
        return this;
    }
    return makeTrie(symbol, current, bytes);
}

PrattTrie *insertPrattTrie(PrattTrie *current, HashSymbol *symbol) {
    PrattTrie *this = insertTrie(current, symbol, (unsigned char *) symbol->name);
    if (this == NULL) {
        return current;
    }
    return this;
}

