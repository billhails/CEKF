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
#include "utf8.h"

#ifdef DEBUG_PRATT_SCANNER
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static void errorAtLexer(char *message, PrattLexer *lexer) {
    can_happen("%s at %s line %d", message, lexer->bufList->filename->name, lexer->bufList->lineno);
}

static char *readFile(char *path) {
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
        perror(path);
        exit(1);
    }
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);
    char *buffer = (char *) safeMalloc(fileSize + 1);
    size_t bytes_read = fread(buffer, sizeof(char), fileSize, file);
    buffer[bytes_read] = '\0';
    fclose(file);
    return buffer;
}

static HashSymbol *lookupTrieRecursive(PrattTrie *trie,
                                       PrattBuffer *buffer,
                                       int last,
                                       HashSymbol *found) {
    DEBUG("lookupTrieRecursive %p %c", trie, buffer->start[buffer->length]);
    if (trie == NULL || buffer->start[buffer->length] > trie->character) {
        buffer->length = last;
        return found;
    } else if (buffer->start[buffer->length] < trie->character) {
        return lookupTrieRecursive(trie->siblings, buffer, last, found);
    }
    ++buffer->length;
    if (trie->terminal != NULL) {
        // avoid i.e. "orbit" false matching "or"
        if (!isalpha(trie->character) || !isalpha(buffer->start[buffer->length])) {
            found = trie->terminal;
            last = buffer->length;
        }
    }
    return lookupTrieRecursive(trie->children, buffer, last, found);
}

static bool isLeadingUtf8(char c) {
    return isTwoByteUtf8((Byte) c) || isThreeByteUtf8((Byte) c) || isFourByteUtf8((Byte) c);
}

static PrattToken *tokenFromBufList(PrattBufList *bufList, HashSymbol *tokenType) {
    PrattBuffer *copy = copyPrattBuffer(bufList->buffer);
    int save = PROTECT(copy);
    PrattToken *token = newPrattToken(tokenType, bufList->filename, bufList->lineno, copy, NULL);
    UNPROTECT(save);
    return token;
}

static void advance(PrattBuffer *buffer) {
    buffer->start += buffer->length;
    buffer->length = 0;
}

static PrattToken *lookupTrieSymbol(PrattLexer *lexer) {
    DEBUG("lookupTrieSymbol \"%s\" (%d)", lexer->bufList->buffer->start, lexer->bufList->buffer->length);
    HashSymbol *symbol = lookupTrieRecursive(lexer->trie, lexer->bufList->buffer, 0, NULL);
    if (symbol == NULL) {
        return NULL;
    }
    PrattToken *res = tokenFromBufList(lexer->bufList, symbol);
    advance(lexer->bufList->buffer);
    return res;
}

static void walkUtf8(PrattBuffer *buffer, int size, HashSymbol *file, int line) {
    ++buffer->length;
    while (size > 0) {
        --size;
        if (!isTrailingByteUtf8(buffer->start[buffer->length])) {
            can_happen("malformed UTF8 at %s line %d", file->name, line);
        }
        ++buffer->length;
    }
}

static PrattToken *parseIdentifier(PrattLexer *lexer) {
    PrattBuffer *buffer = lexer->bufList->buffer;
    while (true) {
        if (isalnum(buffer->start[buffer->length])) {
            ++buffer->length;
        } else if (isTwoByteUtf8(buffer->start[buffer->length])) {
            walkUtf8(buffer, 1, lexer->bufList->filename, lexer->bufList->lineno);
        } else if (isThreeByteUtf8(buffer->start[buffer->length])) {
            walkUtf8(buffer, 2, lexer->bufList->filename, lexer->bufList->lineno);
        } else if (isFourByteUtf8(buffer->start[buffer->length])) {
            walkUtf8(buffer, 3, lexer->bufList->filename, lexer->bufList->lineno);
        } else {
#ifdef SAFETY_CHECKS
            if (buffer->length == 0) {
                cant_happen("parseIdentifier passed bad identifier");
            }
#endif
            PrattToken *token = tokenFromBufList(lexer->bufList, newSymbol("ATOM"));
            advance(buffer);
            return token;
        }
    }
}

static PrattToken *parseNumeric(PrattLexer *lexer) {
    PrattBuffer *buffer = lexer->bufList->buffer;
    HashSymbol *type = S("INT");
    PrattNumberState state = PRATTNUMBERSTATE_TYPE_START;
    while (state != PRATTNUMBERSTATE_TYPE_END) {
        switch (state) {
            case PRATTNUMBERSTATE_TYPE_START:
                switch (buffer->start[buffer->length]) {
                    case '0':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_ZERO;
                        break;
                    case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_DEC;
                        break;
                    default:
                        cant_happen("parseNumeric passed bad char %d", buffer->start[buffer->length]);
                }
                break;
            case PRATTNUMBERSTATE_TYPE_ZERO:
                switch (buffer->start[buffer->length]) {
                    case '0': case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9': case '_':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_DEC;
                        break;
                    case 'x': case 'X':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_HEX;
                        break;
                    case 'i':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                    case '.':
                        ++buffer->length;
                        type = S("FLOAT");
                        state = PRATTNUMBERSTATE_TYPE_FLOAT;
                        break;
                    default:
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                }
                break;
            case PRATTNUMBERSTATE_TYPE_HEX:
                switch (buffer->start[buffer->length]) {
                    case '0': case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9': case 'a': case 'b':
                    case 'c': case 'd': case 'e': case 'f': case 'A': case 'B':
                    case 'C': case 'D': case 'E': case 'F': case '_':
                        ++buffer->length;
                        break;
                    case 'i':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                    default:
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                }
                break;
            case PRATTNUMBERSTATE_TYPE_DEC:
                switch (buffer->start[buffer->length]) {
                    case '0': case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9': case '_':
                        ++buffer->length;
                        break;
                    case 'i':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                    case '.':
                        ++buffer->length;
                        type = S("FLOAT");
                        state = PRATTNUMBERSTATE_TYPE_FLOAT;
                        break;
                    default:
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                }
                break;
            case PRATTNUMBERSTATE_TYPE_FLOAT:
                switch (buffer->start[buffer->length]) {
                    case '0': case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9': case '_':
                        ++buffer->length;
                        break;
                    case 'i':
                        ++buffer->length;
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                    default:
                        state = PRATTNUMBERSTATE_TYPE_END;
                        break;
                }
                break;
            case PRATTNUMBERSTATE_TYPE_END:
                cant_happen("end state in loop");
        }
    }
    PrattToken *token = tokenFromBufList(lexer->bufList, type);
    advance(buffer);
    return token;
}

static PrattToken *dequeueToken(PrattLexer *lexer) {
    if (lexer->tokenHead) {
        PrattToken *next = lexer->tokenHead;
        lexer->tokenHead = lexer->tokenHead->next;
        if (lexer->tokenHead == NULL) {
            lexer->tokenTail = NULL;
        }
        next->next = NULL;
        return next;
    } else {
        return NULL;
    }
}

static void enqueueToken(PrattLexer *lexer, PrattToken *token) {
    token->next = NULL;
    if (lexer->tokenTail == NULL) { // Queue is empty
        lexer->tokenHead = lexer->tokenTail = token;
    } else {
        lexer->tokenTail->next = token;
        lexer->tokenTail = token;
    }
}

static PrattToken *tokenEOF() {
    return newPrattToken(newSymbol("EOF"), NULL, 0, NULL, NULL);
}

static PrattToken *parseString(PrattLexer *lexer) {
    PrattBuffer *buffer = lexer->bufList->buffer;
    PrattStringState state = PRATTSTRINGSTATE_TYPE_START;
    while (state != PRATTSTRINGSTATE_TYPE_END) {
        switch (state) {
            case PRATTSTRINGSTATE_TYPE_START:
#ifdef SAFETCY_CHECKS
                if (buffer->start[buffer->length] != '"') {
                    cant_happen("expected '\"' got '%c'", buffer->start[buffer->length]);
                }
#endif
                ++buffer->length;
                state = PRATTSTRINGSTATE_TYPE_STR;
                break;
            case PRATTSTRINGSTATE_TYPE_STR:
                switch (buffer->start[buffer->length]) {
                    case '\\':
                        ++buffer->length;
                        state = PRATTSTRINGSTATE_TYPE_ESC;
                        break;
                    case '"':
                        ++buffer->length;
                        state = PRATTSTRINGSTATE_TYPE_END;
                        break;
                    case '\n':
                        errorAtLexer("EOL while parsing string", lexer);
                        ++buffer->length;
                        break;
                    case '\0':
                        errorAtLexer("EOF while parsing string", lexer);
                        state = PRATTSTRINGSTATE_TYPE_END;
                        break;
                    default:
                        ++buffer->length;
                        break;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_ESC:
                switch (buffer->start[buffer->length]) {
                    case 'u': case 'U':
                        ++buffer->length;
                        state = PRATTSTRINGSTATE_TYPE_UNI;
                        break;
                    case '\n':
                        errorAtLexer("EOL while parsing string", lexer);
                        ++buffer->length;
                        break;
                    case '\0':
                        errorAtLexer("EOF while parsing string", lexer);
                        state = PRATTSTRINGSTATE_TYPE_END;
                        break;
                    default:
                        ++buffer->length;
                        state = PRATTSTRINGSTATE_TYPE_STR;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_UNI:
                switch (buffer->start[buffer->length]) {
                    case '0': case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9': case 'a': case 'b':
                    case 'c': case 'd': case 'e': case 'f': case 'A': case 'B':
                    case 'C': case 'D': case 'E': case 'F':
                        ++buffer->length;
                        break;
                    case ';':
                        ++buffer->length;
                        state = PRATTSTRINGSTATE_TYPE_STR;
                        break;
                    case '\0':
                        errorAtLexer("EOF while parsing unicode escape", lexer);
                        state = PRATTSTRINGSTATE_TYPE_END;
                        break;
                    default:
                        errorAtLexer("expected hex digit or ';'", lexer);
                        state = PRATTSTRINGSTATE_TYPE_END;
                        ++buffer->length;
                        break;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_END:
                cant_happen("end state in loop");
        }
    }
    PrattToken *token = tokenFromBufList(lexer->bufList, newSymbol("STRING"));
    advance(buffer);
    return token;
}

static PrattToken *parseChar(PrattLexer *lexer) {
    PrattBuffer *buffer = lexer->bufList->buffer;
    PrattCharState state = PRATTCHARSTATE_TYPE_START;
    while (state != PRATTCHARSTATE_TYPE_END) {
        switch (state) {
            case PRATTCHARSTATE_TYPE_START:
#ifdef SAFETY_CHECKS
                if (buffer->start[buffer->length] != '\'') {
                    cant_happen("parseChar given non-char %c", buffer->start[buffer->length]);
                }
#endif
                ++buffer->length;
                state = PRATTCHARSTATE_TYPE_CHR;
                break;
            case PRATTCHARSTATE_TYPE_CHR:
                if (isTwoByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR2;
                } else if (isThreeByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR3;
                } else if (isFourByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR4;
                } else {
                    switch (buffer->start[buffer->length]) {
                        case '\'':
                            errorAtLexer("empty char ''", lexer);
                            ++buffer->length;
                            state = PRATTCHARSTATE_TYPE_END;
                            break;
                        case '\n':
                            errorAtLexer("EOL while parsing char", lexer);
                            ++buffer->length;
                            break;
                        case '\0':
                            errorAtLexer("EOF while parsing char", lexer);
                            state = PRATTCHARSTATE_TYPE_END;
                            break;
                        case '\\':
                            ++buffer->length;
                            state = PRATTCHARSTATE_TYPE_ESC;
                            break;
                        default:
                            ++buffer->length;
                            state = PRATTCHARSTATE_TYPE_CHR1;
                            break;
                    }
                }
                break;
            case PRATTCHARSTATE_TYPE_ESC:
                if (isTwoByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR2;
                } else if (isThreeByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR3;
                } else if (isFourByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR4;
                } else {
                    switch (buffer->start[buffer->length]) {
                        case 'u': case 'U':
                            ++buffer->length;
                            state = PRATTCHARSTATE_TYPE_UNI;
                            break;
                        case '\n':
                            cant_happen("EOL while parsing char");
                        case '\0':
                            cant_happen("EOF while parsing char");
                        default:
                            ++buffer->length;
                            state = PRATTCHARSTATE_TYPE_CHR1;
                            break;
                    }
                }
                break;
            case PRATTCHARSTATE_TYPE_UNI:
                switch (buffer->start[buffer->length]) {
                    case '0': case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9': case 'a': case 'b':
                    case 'c': case 'd': case 'e': case 'f': case 'A': case 'B':
                    case 'C': case 'D': case 'E': case 'F':
                        ++buffer->length;
                        break;
                    case ';':
                        ++buffer->length;
                        state = PRATTCHARSTATE_TYPE_CHR1;
                        break;
                    default:
                        cant_happen("malformed unicode value");
                }
                break;
            case PRATTCHARSTATE_TYPE_CHR1:
                if (buffer->start[buffer->length] == '\'') {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_END;
                } else {
                    cant_happen("expected '");
                }
                break;
            case PRATTCHARSTATE_TYPE_CHR2:
                if (isTrailingByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR1;
                } else {
                    cant_happen("malformed utf8");
                }
                break;
            case PRATTCHARSTATE_TYPE_CHR3:
                if (isTrailingByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR2;
                } else {
                    cant_happen("malformed utf8");
                }
                break;
            case PRATTCHARSTATE_TYPE_CHR4:
                if (isTrailingByteUtf8(buffer->start[buffer->length])) {
                    ++buffer->length;
                    state = PRATTCHARSTATE_TYPE_CHR3;
                } else {
                    cant_happen("malformed utf8");
                }
                break;
            case PRATTCHARSTATE_TYPE_END:
                cant_happen("reached end state in loop");
        }
    }
    PrattToken *token = tokenFromBufList(lexer->bufList, newSymbol("CHAR"));
    advance(buffer);
    return token;
}

PrattToken *next(PrattLexer *lexer) {
    PrattToken *lookahead = dequeueToken(lexer);
    if (lookahead != NULL) {
        DEBUG("next found lookahead %s", lookahead->type->name);
        return lookahead;
    } else {
        DEBUG("next no lookahead");
        while (lexer->bufList != NULL) {
            PrattBuffer *buffer = lexer->bufList->buffer;
            if (buffer->start == NULL) {
                buffer->start = buffer->data;
            }
            while (buffer->start[0]) {
                if (isspace(buffer->start[0])) {
                    if (buffer->start[0] == '\n') {
                        ++lexer->bufList->lineno;
                    }
                    ++(buffer->start);
                } else if (buffer->start[0] == '/' && buffer->start[1] == '/') {
                    while (buffer->start[0] && buffer->start[0] != '\n') {
                        ++buffer->start;
                    }
                    if (buffer->start[0] == '\n') {
                        ++buffer->start;
                        ++lexer->bufList->lineno;
                    }
                } else if (isalpha(buffer->start[0]) || isLeadingUtf8(buffer->start[0])) {
                    PrattToken *token = lookupTrieSymbol(lexer);
                    if (token != NULL) {
                        return token;
                    } else {
                        return parseIdentifier(lexer);
                    }
                } else if (isdigit(buffer->start[0])) {
                    return parseNumeric(lexer);
                } else if (buffer->start[0] == '"') {
                    return parseString(lexer);
                } else if (buffer->start[0] == '\'') {
                    return parseChar(lexer);
                } else if (ispunct(buffer->start[0])) {
                    PrattToken *token = lookupTrieSymbol(lexer);
                    if (token != NULL) {
                        return token;
                    }
                    cant_happen("unrecognised operator %c", buffer->start[0]);
                } else if (isTrailingByteUtf8((Byte) (buffer->start[0]))) {
                    cant_happen("malformed utf8");
                } else {
                    cant_happen("unexpected %c", buffer->start[0]);
                }
            }
            lexer->bufList = lexer->bufList->next;
        }
        return tokenEOF();
    }
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

static PrattBuffer *prattBufferFromString(char *string) {
    return newPrattBuffer(string);
}

static PrattBuffer *prattBufferFromFileName(char *path) {
    char *content = readFile(path);
    return newPrattBuffer(content);
}

void errorAt(PrattToken *token, char *message) {
    can_happen("%s at \"%s\" in %s line %d", message, token->type->name, token->filename->name, token->lineno);
}

PrattToken *peek(PrattLexer *lexer) {
    DEBUG("peek");
    PrattToken *token = next(lexer);
    int save = PROTECT(token);
    enqueueToken(lexer, token);
    UNPROTECT(save);
    return token;
}

bool check(PrattLexer *lexer, HashSymbol *type) {
    PrattToken *token = peek(lexer);
    return token->type == type;
}

bool match(PrattLexer *lexer, HashSymbol *type) {
    PrattToken *token = next(lexer);
    if (token->type == type) {
        return true;
    }
    int save = PROTECT(token);
    enqueueToken(lexer, token);
    UNPROTECT(save);
    return false;
}

void consume(PrattLexer *lexer, HashSymbol *type, char *message) {
    PrattToken *token = next(lexer);
    if (token->type == type) {
        return;
    }
    errorAt(token, message);
}

PrattBufList *prattBufListFromFileName(char *fileName, PrattBufList *next) {
    PrattBuffer *buffer = prattBufferFromFileName(fileName);
    int save = PROTECT(buffer);
    PrattBufList *res = newPrattBufList(1, newSymbol(fileName), buffer, next);
    UNPROTECT(save);
    return res;
}

PrattBufList *prattBufListFromString(char *origin, char *string, PrattBufList *next) {
    PrattBuffer *buffer = prattBufferFromString(string);
    int save = PROTECT(buffer);
    PrattBufList *res = newPrattBufList(1, newSymbol(origin), buffer, next);
    UNPROTECT(save);
    return res;
}

PrattLexer *makePrattLexer(PrattTrie *trie, char *input) {
    PrattBufList *bl = prattBufListFromString("test", input, NULL);
    int save = PROTECT(bl);
    PrattLexer *res = newPrattLexer(bl, trie);
    UNPROTECT(save);
    return res;
}
