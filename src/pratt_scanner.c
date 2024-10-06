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
#include <stdarg.h>

#include"symbol.h"
#include "pratt_scanner.h"
#include "utf8.h"
#include "bigint.h"

#ifdef DEBUG_PRATT_SCANNER
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

HashSymbol *TOK_PREFIX(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("prefix");
    return s;
}

HashSymbol *TOK_INFIX(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("infix");
    return s;
}

HashSymbol *TOK_POSTFIX(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("postfix");
    return s;
}

HashSymbol *TOK_KW_NUMBER(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("number");
    return s;
}

HashSymbol *TOK_BACK(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("back");
    return s;
}

HashSymbol *TOK_SWITCH(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("switch");
    return s;
}

HashSymbol *TOK_ASSERT(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("assert");
    return s;
}

HashSymbol *TOK_KW_CHAR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("char");
    return s;
}

HashSymbol *TOK_IF(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("if");
    return s;
}

HashSymbol *TOK_ELSE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("else");
    return s;
}

HashSymbol *TOK_PIPE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("|");
    return s;
}

HashSymbol *TOK_WILDCARD(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("_");
    return s;
}

HashSymbol *TOK_LCURLY(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("{");
    return s;
}

HashSymbol *TOK_RCURLY(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("}");
    return s;
}

HashSymbol *TOK_LSQUARE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("[");
    return s;
}

HashSymbol *TOK_RSQUARE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("]");
    return s;
}

HashSymbol *TOK_ATOM(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(" ATOM"); // tokens with leading spaces are internal to the parser
    return s;
}

HashSymbol *TOK_NUMBER(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(" NUMBER");
    return s;
}

HashSymbol *TOK_EOF(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(" EOF");
    return s;
}

HashSymbol *TOK_STRING(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(" STRING");
    return s;
}

HashSymbol *TOK_ERROR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(" ERROR");
    return s;
}

HashSymbol *TOK_CHAR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(" CHAR");
    return s;
}

HashSymbol *TOK_TUPLE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("#(");
    return s;
}

HashSymbol *TOK_OPEN(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("(");
    return s;
}

HashSymbol *TOK_CLOSE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(")");
    return s;
}

HashSymbol *TOK_COMMA(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(",");
    return s;
}

HashSymbol *TOK_ARROW(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("->");
    return s;
}

HashSymbol *TOK_THEN(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("then");
    return s;
}

HashSymbol *TOK_AND(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("and");
    return s;
}

HashSymbol *TOK_OR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("or");
    return s;
}

HashSymbol *TOK_XOR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("xor");
    return s;
}

HashSymbol *TOK_NAND(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("nand");
    return s;
}

HashSymbol *TOK_NOR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("nor");
    return s;
}

HashSymbol *TOK_XNOR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("xnor");
    return s;
}

HashSymbol *TOK_NOT(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("not");
    return s;
}

HashSymbol *TOK_EQ(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("==");
    return s;
}

HashSymbol *TOK_NE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("!=");
    return s;
}

HashSymbol *TOK_GT(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(">");
    return s;
}

HashSymbol *TOK_LT(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("<");
    return s;
}

HashSymbol *TOK_GE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(">=");
    return s;
}

HashSymbol *TOK_LE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("<=");
    return s;
}

HashSymbol *TOK_CMP(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("<=>");
    return s;
}

HashSymbol *TOK_ASSIGN(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("=");
    return s;
}

HashSymbol *TOK_COLON(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(":");
    return s;
}

HashSymbol *TOK_APPEND(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("@@");
    return s;
}

HashSymbol *TOK_CONS(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("@");
    return s;
}

HashSymbol *TOK_PLUS(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("+");
    return s;
}

HashSymbol *TOK_MINUS(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("-");
    return s;
}

HashSymbol *TOK_TIMES(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("*");
    return s;
}

HashSymbol *TOK_DIVIDE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("/");
    return s;
}

HashSymbol *TOK_MOD(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("%");
    return s;
}

HashSymbol *TOK_EXP(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("**");
    return s;
}

HashSymbol *TOK_HERE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("here");
    return s;
}

HashSymbol *TOK_HASH(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("#");
    return s;
}

HashSymbol *TOK_BANG(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("!");
    return s;
}

HashSymbol *TOK_PERIOD(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(".");
    return s;
}

HashSymbol *TOK_LET(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("let");
    return s;
}

HashSymbol *TOK_IN(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("in");
    return s;
}

HashSymbol *TOK_NS(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("__NAMESPACE__");
    return s;
}

HashSymbol *TOK_NAMESPACE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("namespace");
    return s;
}

HashSymbol *TOK_KW_ERROR(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("error");
    return s;
}

HashSymbol *TOK_TYPEDEF(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("typedef");
    return s;
}

HashSymbol *TOK_UNSAFE(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("unsafe");
    return s;
}

HashSymbol *TOK_FN(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("fn");
    return s;
}

HashSymbol *TOK_LINK(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("link");
    return s;
}

HashSymbol *TOK_AS(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("as");
    return s;
}

HashSymbol *TOK_ALIAS(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("alias");
    return s;
}

HashSymbol *TOK_SEMI(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol(";");
    return s;
}

HashSymbol *TOK_PRINT(void) {
    static HashSymbol *s = NULL;
    if (s == NULL) s = newSymbol("print");
    return s;
}

static bool isLeadingUtf8(char c) {
    return isTwoByteUtf8((Byte) c) || isThreeByteUtf8((Byte) c) || isFourByteUtf8((Byte) c);
}

static bool isALPHA(char c) {
    return isalpha(c) || c == '_';
}

static bool isALNUM(char c) {
    return isdigit(c) || isALPHA(c);
}

ParserInfo LEXPI(PrattLexer *lexer) {
    ParserInfo res;
    res.lineno = 0;
    res.filename = "undefined";
    if (lexer) {
        if (lexer->tokenHead) {
            res.lineno = lexer->tokenHead->lineno;
            res.filename = lexer->tokenHead->filename->name;
        } else if (lexer->bufList) {
            res.lineno = lexer->bufList->lineno;
            res.filename = lexer->bufList->filename->name;
        }
    }
    return res;
}

void parserError(PrattParser *parser, const char *message, ...) {
    va_list args;
    if (parser->panicMode) return;
    parser->panicMode = true;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    PrattBufList *bufList = parser->lexer->bufList;
    if (bufList) {
        can_happen(" at %s line %d", bufList->filename->name, bufList->lineno);
    } else {
        can_happen(" at EOF");
    }
}

void parserErrorAt(ParserInfo PI, PrattParser *parser, const char *message, ...) {
    va_list args;
    if (parser->panicMode) return;
    parser->panicMode = true;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    can_happen(" at %s line %d", PI.filename, PI.lineno);
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
    // DEBUG("lookupTrieRecursive %p %c", trie, buffer->start[buffer->length]);
    if (trie == NULL || buffer->start[buffer->length] > trie->character) {
        buffer->length = last;
        return found;
    } else if (buffer->start[buffer->length] < trie->character) {
        return lookupTrieRecursive(trie->siblings, buffer, last, found);
    }
    ++buffer->length;
    if (trie->terminal != NULL) {
        // avoid i.e. "orbit" false matching "or"
        if (!isALPHA(trie->character) || !isALPHA(buffer->start[buffer->length])) {
            found = trie->terminal;
            last = buffer->length;
        }
    }
    return lookupTrieRecursive(trie->children, buffer, last, found);
}

static PrattToken *tokenFromSymbol(PrattBufList *bufList, HashSymbol *symbol, HashSymbol *tokenType) {
    PrattValue *value = newPrattValue_Atom(symbol);
    int save = PROTECT(value);
    PrattToken *token = newPrattToken(tokenType, bufList->filename, bufList->lineno, value, NULL);
    UNPROTECT(save);
    return token;
}

static PrattToken *tokenFromBigInt(PrattBufList *bufList, MaybeBigInt *bi, HashSymbol *tokenType) {
    PrattValue *value = newPrattValue_Number(bi);
    int save = PROTECT(value);
    PrattToken *token = newPrattToken(tokenType, bufList->filename, bufList->lineno, value, NULL);
    UNPROTECT(save);
    return token;
}

static HashSymbol *symbolFromBuffer(PrattBuffer *buffer) {
    return newSymbolLength(buffer->start, buffer->length);
}

static PrattToken *tokenFromString(PrattBufList *bufList, PrattUTF8 *string, HashSymbol *tokenType) {
    PrattValue *value = newPrattValue_String(string);
    int save = PROTECT(value);
    PrattToken *token = newPrattToken(tokenType, bufList->filename, bufList->lineno, value, NULL);
    UNPROTECT(save);
    return token;
}

static void advance(PrattBuffer *buffer) {
    buffer->start += buffer->length;
    buffer->length = 0;
}

static PrattToken *lookupTrieSymbol(PrattLexer *lexer) {
    // DEBUG("lookupTrieSymbol \"%s\" (%d)", lexer->bufList->buffer->start, lexer->bufList->buffer->length);
    HashSymbol *symbol = lookupTrieRecursive(lexer->trie, lexer->bufList->buffer, 0, NULL);
    if (symbol == NULL) {
        return NULL;
    }
    PrattToken *res = tokenFromSymbol(lexer->bufList, symbol, symbol);
    advance(lexer->bufList->buffer);
    return res;
}

static void walkUtf8(PrattParser *parser, int size) {
    PrattBuffer *buffer = parser->lexer->bufList->buffer;
    ++buffer->length;
    while (size > 0) {
        --size;
        if (!isTrailingByteUtf8(buffer->start[buffer->length])) {
            parserError(parser, "malformed UTF8");
        }
        ++buffer->length;
    }
}

static PrattToken *parseIdentifier(PrattParser *parser) {
    PrattBuffer *buffer = parser->lexer->bufList->buffer;
    while (true) {
        if (isALNUM(buffer->start[buffer->length])) {
            ++buffer->length;
        } else if (isTwoByteUtf8(buffer->start[buffer->length])) {
            walkUtf8(parser, 1);
        } else if (isThreeByteUtf8(buffer->start[buffer->length])) {
            walkUtf8(parser, 2);
        } else if (isFourByteUtf8(buffer->start[buffer->length])) {
            walkUtf8(parser, 3);
        } else {
#ifdef SAFETY_CHECKS
            if (buffer->length == 0) {
                cant_happen("parseIdentifier passed bad identifier");
            }
#endif
            HashSymbol *symbol = symbolFromBuffer(buffer);
            PrattToken *token = tokenFromSymbol(parser->lexer->bufList, symbol, TOK_ATOM());
            advance(buffer);
            return token;
        }
    }
}

static void bigint_mul_by_n(bigint *b, int n) {
    bigint old;
    bigint_init(&old);
    bigint_cpy(&old, b);
    bigint_free(b);
    bigint_init(b);
    bigint mul;
    bigint_init(&mul);
    bigint_from_int(&mul, n);
    bigint_mul(b, &mul, &old);
    bigint_free(&mul);
    bigint_free(&old);
}

static void bigint_add_n(bigint *b, int n) {
    bigint old;
    bigint_init(&old);
    bigint_cpy(&old, b);
    bigint_free(b);
    bigint_init(b);
    bigint_add_word(b, &old, n);
    bigint_free(&old);
}

static int convert_char(char c) {
    switch (c) {
        case '0': case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
            return c - '0';
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
            return 10 + (c - 'a');
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
            return 10 + (c - 'A');
        default:
            cant_happen("unrecognised numeric digit '%c'", c);
    }
}

static MaybeBigInt *makeMaybeBigInt(char *digits, int length) {
    bool overflowed = false;
    bool imag = false;
    int a = 0;
    bigint bi;
    int multiplier = 10;
    if (digits[0] == '0' && (digits[1] == 'x' || digits[1] == 'X')) {
        digits += 2;
        length -= 2;
        multiplier = 16;
    }
    for (char *p = digits; length > 0; --length, ++p) {
        if (*p == '_') continue;
        if (*p == 'i') {
            imag = true;
            continue;
        }
        int n = convert_char(*p);
        if(overflowed) {
            bigint_mul_by_n(&bi, multiplier);
            bigint_add_n(&bi, n);
        } else {
            int c;
            if (__builtin_mul_overflow(a, multiplier, &c)) {
                overflowed = true;
                bigint_init(&bi);
                bigint_from_int(&bi, a);
                bigint_mul_by_n(&bi, multiplier);
                bigint_add_n(&bi, n);
            } else {
                a = c;
                if (__builtin_add_overflow(a, n, &c)) {
                    overflowed = true;
                    bigint_init(&bi);
                    bigint_from_int(&bi, a);
                    bigint_add_n(&bi, n);
                } else {
                    a = c;
                }
            }
        }
    }
    if (overflowed) {
        MaybeBigInt *bbi = newMaybeBigInt(bi, imag);
        return bbi;
    } else {
        return fakeBigInt(a, imag);
    }
}

static MaybeBigInt *makeIrrational(char *str, int length) {
    bool imag = false;
    bool frac = false;
    Double f = 0.0;
    Double div = 1.0;
    for (char *p = str; length > 0; --length, ++p) {
        switch (*p) {
            case '_':
                continue;
            case 'i':
                imag = true;
                continue;
            case '.':
                frac = true;
                continue;
            default:
                int n = convert_char(*p);
                f *= 10.0;
                f += n;
                if (frac) div *= 10.0;
        }
    }
    return irrationalBigInt(f/div, imag);
}

static PrattToken *parseNumeric(PrattLexer *lexer) {
    PrattBuffer *buffer = lexer->bufList->buffer;
    HashSymbol *type = TOK_NUMBER();
    PrattNumberState state = PRATTNUMBERSTATE_TYPE_START;
    bool floating = false;
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
                        state = PRATTNUMBERSTATE_TYPE_FLOAT;
                        floating = true;
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
                        state = PRATTNUMBERSTATE_TYPE_FLOAT;
                        floating = true;
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
    MaybeBigInt *bi = NULL;
    if (floating) {
        bi = makeIrrational(buffer->start, buffer->length);
    } else {
        bi = makeMaybeBigInt(buffer->start, buffer->length);
    }
    int save = PROTECT(bi);
    PrattToken *token = tokenFromBigInt(lexer->bufList, bi, type);
    advance(buffer);
    UNPROTECT(save);
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

void enqueueToken(PrattLexer *lexer, PrattToken *token) {
    token->next = NULL;
    if (lexer->tokenTail == NULL) { // Queue is empty
        lexer->tokenHead = lexer->tokenTail = token;
    } else {
        lexer->tokenTail->next = token;
        lexer->tokenTail = token;
    }
}

static PrattToken *tokenEOF() {
    return newPrattToken(TOK_EOF(), newSymbol("eof"), 0, NULL, NULL);
}

static PrattToken *tokenERROR(PrattLexer *lexer) {
    ParserInfo PI = LEXPI(lexer);
    return newPrattToken(TOK_ERROR(), newSymbol(PI.filename), PI.lineno, NULL, NULL);
}

static PrattToken *parseString(PrattParser *parser, bool single, char sep) {
    PrattLexer *lexer = parser->lexer;
    PrattBuffer *buffer = lexer->bufList->buffer;
    PrattUTF8 *string = newPrattUTF8();
    int save = PROTECT(string);
    PrattStringState state = PRATTSTRINGSTATE_TYPE_START;
    Character uni = 0;
    while (state != PRATTSTRINGSTATE_TYPE_END) {
        switch (state) {
            case PRATTSTRINGSTATE_TYPE_START:
                DEBUG("parseString %s %d (sep %c) START: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
#ifdef SAFETCY_CHECKS
                if (buffer->start[buffer->length] != sep) {
                    cant_happen("expected '%c' got '%c'", sep, buffer->start[buffer->length]);
                }
#endif
                ++buffer->length;
                state = PRATTSTRINGSTATE_TYPE_STR;
                break;
            case PRATTSTRINGSTATE_TYPE_STR:
                DEBUG("parseString %s %d (sep %c) STR: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
                if (isTwoByteUtf8(buffer->start[buffer->length])) {
                    pushPrattUTF8(string, buffer->start[buffer->length]);
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_CHR2;
                } else if (isThreeByteUtf8(buffer->start[buffer->length])) {
                    pushPrattUTF8(string, buffer->start[buffer->length]);
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_CHR3;
                } else if (isFourByteUtf8(buffer->start[buffer->length])) {
                    pushPrattUTF8(string, buffer->start[buffer->length]);
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_CHR4;
                } else if (isTrailingByteUtf8(buffer->start[buffer->length])) {
                    parserError(parser, "Malformed UTF8");
                    ++buffer->length;
                } else if (buffer->start[buffer->length] == sep) {
                    if (single) {
                        parserError(parser, "empty char");
                    }
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_END;
                } else {
                    switch (buffer->start[buffer->length]) {
                        case '\\':
                            ++buffer->length;
                            state = PRATTSTRINGSTATE_TYPE_ESC;
                            break;
                        case '\n':
                            parserError(parser, "unexpected EOL");
                            ++buffer->length;
                            ++lexer->bufList->lineno;
                            break;
                        case '\0':
                            parserError(parser, "unexpected EOF");
                            state = PRATTSTRINGSTATE_TYPE_END;
                            break;
                        default:
                            pushPrattUTF8(string, buffer->start[buffer->length]);
                            ++buffer->length;
                            state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                            break;
                    }
                }
                break;
            case PRATTSTRINGSTATE_TYPE_CHR4:
                DEBUG("parseString %s %d (sep %c) CHR4: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
                if (isTrailingByteUtf8(buffer->start[buffer->length])) {
                    pushPrattUTF8(string, buffer->start[buffer->length]);
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_CHR3;
                } else {
                    parserError(parser, "Malformed UTF8");
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_STR;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_CHR3:
                DEBUG("parseString %s %d (sep %c) CHR3: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
                if (isTrailingByteUtf8(buffer->start[buffer->length])) {
                    pushPrattUTF8(string, buffer->start[buffer->length]);
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_CHR2;
                } else {
                    parserError(parser, "Malformed UTF8");
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_STR;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_CHR2:
                DEBUG("parseString %s %d (sep %c) CHR2: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
                if (isTrailingByteUtf8(buffer->start[buffer->length])) {
                    pushPrattUTF8(string, buffer->start[buffer->length]);
                    ++buffer->length;
                    state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                } else {
                    parserError(parser, "Malformed UTF8");
                    ++buffer->length;
                    state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_ESC:
                DEBUG("parseString %s %d (sep %c) ESC: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
                switch (buffer->start[buffer->length]) {
                    case 'u': case 'U':
                        ++buffer->length;
                        uni = 0; // reset
                        state = PRATTSTRINGSTATE_TYPE_UNI;
                        break;
                    case 'n':
                        pushPrattUTF8(string, '\n');
                        ++buffer->length;
                        state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                        break;
                    case 't':
                        pushPrattUTF8(string, '\t');
                        ++buffer->length;
                        state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                        break;
                    case '\n':
                        parserError(parser, "unexpected EOL");
                        ++buffer->length;
                        break;
                    case '\0':
                        parserError(parser, "unexpected EOF");
                        state = PRATTSTRINGSTATE_TYPE_END;
                        break;
                    default:
                        ++buffer->length;
                        state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_UNI:
                DEBUG("parseString %s %d (sep %c) UNI: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
                switch (buffer->start[buffer->length]) {
                    case '0': case '1': case '2': case '3': case '4': case '5':
                    case '6': case '7': case '8': case '9': {
                        Character c = buffer->start[buffer->length] - '0';
                        uni <<= 4;
                        uni |= c;
                        buffer->length++;
                    }
                    break;
                    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':{
                        Character c = 10 + buffer->start[buffer->length] - 'a';
                        uni <<= 4;
                        uni |= c;
                        buffer->length++;
                    }
                    break;
                    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': {
                        Character c = 10 + buffer->start[buffer->length] - 'A';
                        uni <<= 4;
                        uni |= c;
                        buffer->length++;
                    }
                    break;
                    case ';':
                        ++buffer->length;
                        if (uni == 0) {
                            parserError(parser, "Empty Unicode escape while parsing string");
                        } else {
                            int size = byteSize(uni);
                            unsigned char *writePoint = &string->entries[string->size];
                            while (size-- > 0) {
                                pushPrattUTF8(string, '\0'); // ensure capacity
                            }
                            writeChar(writePoint, uni);
                        }
                        state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                        break;
                    case '\0':
                        parserError(parser, "EOF while parsing unicode escape");
                        state = PRATTSTRINGSTATE_TYPE_END;
                        break;
                    default:
                        parserError(parser, "expected hex digit or ';'");
                        state = single ? PRATTSTRINGSTATE_TYPE_CHR1 : PRATTSTRINGSTATE_TYPE_STR;
                        ++buffer->length;
                        break;
                }
                break;
            case PRATTSTRINGSTATE_TYPE_CHR1: // only get here if single == true
                DEBUG("parseString %s %d (sep %c) CHR1: %c", lexer->bufList->filename->name, lexer->bufList->lineno, sep, buffer->start[buffer->length]);
                if (buffer->start[buffer->length] == sep) {
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_END;
                } else {
                    parserError(parser, "expected terminator");
                }
                break;
            case PRATTSTRINGSTATE_TYPE_END:
                cant_happen("end state in loop");
        }
    }
    pushPrattUTF8(string, '\0');
    PrattToken *token = tokenFromString(lexer->bufList, string, single ? TOK_CHAR() : TOK_STRING());
    advance(buffer);
    UNPROTECT(save);
    return token;
}

PrattToken *next(PrattParser *parser) {
    PrattLexer *lexer = parser->lexer;
    PrattToken *lookahead = dequeueToken(lexer);
    if (lookahead != NULL) {
        return lookahead;
    } else {
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
                } else if (isALPHA(buffer->start[0]) || isLeadingUtf8(buffer->start[0])) {
                    PrattToken *token = lookupTrieSymbol(lexer);
                    if (token != NULL) {
                        return token;
                    } else {
                        return parseIdentifier(parser);
                    }
                } else if (isdigit(buffer->start[0])) {
                    return parseNumeric(lexer);
                } else if (buffer->start[0] == '"') {
                    return parseString(parser, false, '"');
                } else if (buffer->start[0] == '\'') {
                    return parseString(parser, true, '\'');
                } else if (ispunct(buffer->start[0])) {
                    PrattToken *token = lookupTrieSymbol(lexer);
                    if (token != NULL) {
                        return token;
                    }
                    parserError(parser, "unrecognised operator %c", buffer->start[0]);
                    ++buffer->start;
                    return tokenERROR(lexer);
                } else if (isTrailingByteUtf8((Byte) (buffer->start[0]))) {
                    parserError(parser, "malformed utf8");
                    ++buffer->start;
                    return tokenERROR(lexer);
                } else {
                    parserError(parser, "unexpected character '%c'", buffer->start[0]);
                    ++buffer->start;
                    return tokenERROR(lexer);
                }
            }
            lexer->bufList = lexer->bufList->next;
            if (lexer->bufList) {
                DEBUG("next buffer %s", lexer->bufList->filename->name);
            }
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
    if (symbol->name[0] == ' ') {
        return current; // skip internal tokens
    }
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

PrattToken *peek(PrattParser *parser) {
    // DEBUG("peek");
    PrattToken *token = next(parser);
    int save = PROTECT(token);
    enqueueToken(parser->lexer, token);
    UNPROTECT(save);
    return token;
}

bool check(PrattParser *parser, HashSymbol *type) {
    PrattToken *token = peek(parser);
    return token->type == type;
}

bool match(PrattParser *parser, HashSymbol *type) {
    PrattToken *token = next(parser);
    if (token->type == type) {
        validateLastAlloc();
        return true;
    }
    int save = PROTECT(token);
    enqueueToken(parser->lexer, token);
    UNPROTECT(save);
    return false;
}

bool consume(PrattParser *parser, HashSymbol *type) {
    PrattToken *token = next(parser);
    validateLastAlloc();
    if (token->type == type) {
        return true;
    }
    parserError(parser, "expected \"%s\" got \"%s\"", type->name, token->type->name);
    return false;
}

PrattBufList *prattBufListFromFileName(char *fileName, PrattBufList *next) {
    PrattBuffer *buffer = prattBufferFromFileName(fileName);
    int save = PROTECT(buffer);
    PrattBufList *res = newPrattBufList(1, newSymbol(fileName), buffer, next);
    UNPROTECT(save);
    return res;
}

PrattBufList *prattBufListFromString(char *string, char *origin, PrattBufList *next) {
    PrattBuffer *buffer = prattBufferFromString(string);
    int save = PROTECT(buffer);
    PrattBufList *res = newPrattBufList(1, newSymbol(origin), buffer, next);
    UNPROTECT(save);
    return res;
}

PrattLexer *makePrattLexer(PrattTrie *trie, char *input, char *origin) {
    PrattBufList *bl = prattBufListFromString(input, origin, NULL);
    int save = PROTECT(bl);
    PrattLexer *res = newPrattLexer(bl, trie);
    UNPROTECT(save);
    return res;
}

PrattLexer *makePrattLexerFromFilename(PrattTrie *trie, char *filename) {
    PrattBufList *bl = prattBufListFromFileName(filename, NULL);
    int save = PROTECT(bl);
    PrattLexer *res = newPrattLexer(bl, trie);
    UNPROTECT(save);
    return res;
}
