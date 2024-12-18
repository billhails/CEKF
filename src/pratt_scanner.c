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
#include "pratt_debug.h"

#ifdef DEBUG_PRATT_SCANNER
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

#define TOKFN(name, string)               \
HashSymbol *TOK_ ## name(void) {          \
    static HashSymbol *s = NULL;          \
    if (s == NULL) s = newSymbol(string); \
    return s;                             \
}

TOKFN(MACRO,"macro")
TOKFN(LEFT,"left")
TOKFN(RIGHT,"right")
TOKFN(PREFIX,"prefix")
TOKFN(INFIX,"infix")
TOKFN(POSTFIX,"postfix")
TOKFN(KW_NUMBER,"number")
TOKFN(BACK,"back")
TOKFN(SWITCH,"switch")
TOKFN(ASSERT,"assert")
TOKFN(KW_CHAR,"char")
TOKFN(IF,"if")
TOKFN(ELSE,"else")
TOKFN(PIPE,"|")
TOKFN(WILDCARD,"_")
TOKFN(LCURLY,"{")
TOKFN(RCURLY,"}")
TOKFN(LSQUARE,"[")
TOKFN(RSQUARE,"]")
TOKFN(ATOM," ATOM") // tokens with leading spaces are internal to the parser
TOKFN(NUMBER," NUMBER")
TOKFN(EOF," EOF")
TOKFN(STRING," STRING")
TOKFN(ERROR," ERROR")
TOKFN(CHAR," CHAR")
TOKFN(TUPLE,"#(")
TOKFN(OPEN,"(")
TOKFN(CLOSE, ")")
TOKFN(COMMA, ",")
TOKFN(ARROW, "->")
TOKFN(ASSIGN, "=")
TOKFN(COLON, ":")
TOKFN(HASH, "#")
TOKFN(BANG, "!")
TOKFN(PERIOD, ".")
TOKFN(LET, "let")
TOKFN(IN, "in")
TOKFN(NAMESPACE, "namespace")
TOKFN(KW_ERROR, "error")
TOKFN(TYPEDEF, "typedef")
TOKFN(UNSAFE, "unsafe")
TOKFN(FN, "fn")
TOKFN(LINK, "link")
TOKFN(AS, "as")
TOKFN(ALIAS, "alias")
TOKFN(SEMI, ";");
TOKFN(PRINT, "print")

#undef TOKFN

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
        can_happen(" at +%d %s", bufList->lineno, bufList->filename->name);
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
    can_happen(" at +%d %s", PI.lineno, PI.filename);
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
    return newSymbolLength((char *)buffer->start, buffer->length);
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

static PrattToken *_lookupTrieSymbol(PrattParser *parser, PrattLexer *lexer) {
    HashSymbol *symbol = lookupTrieRecursive(parser->trie, lexer->bufList->buffer, 0, NULL);
    if (symbol != NULL) {
        PrattToken *res = tokenFromSymbol(lexer->bufList, symbol, symbol);
        advance(lexer->bufList->buffer);
        return res;
    }
    if (parser->next) {
        return _lookupTrieSymbol(parser->next, lexer);
    }
    return NULL;
}

static PrattToken *lookupTrieSymbol(PrattParser *parser) {
    return _lookupTrieSymbol(parser, parser->lexer);
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
            default: {
                int n = convert_char(*p);
                f *= 10.0;
                f += n;
                if (frac) div *= 10.0;
            }
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
        bi = makeIrrational((char *)buffer->start, buffer->length);
    } else {
        bi = makeMaybeBigInt((char *)buffer->start, buffer->length);
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
            case PRATTSTRINGSTATE_TYPE_ESCS:
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
                } else {
                    if (state == PRATTSTRINGSTATE_TYPE_STR) {
                        if (buffer->start[buffer->length] == sep) {
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
                    } else { // PRATTSTRINGSTATE_TYPE_ESCS
                        switch (buffer->start[buffer->length]) {
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
                        state = PRATTSTRINGSTATE_TYPE_ESCS;
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
                    ++buffer->length;
                    state = PRATTSTRINGSTATE_TYPE_END;
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

static Character nextCharacter(PrattBuffer *buffer) {
    Character dest;
    buffer->start = utf8Sgetc(buffer->start, &dest);
    return dest;
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
                // whitespace
                if (utf8_isspace(buffer->start)) {
                    if (buffer->start[0] == '\n') {
                        ++lexer->bufList->lineno;
                    }
                    nextCharacter(buffer);
                // comment
                } else if (buffer->start[0] == '/' && buffer->start[1] == '/') {
                    while (buffer->start[0] && buffer->start[0] != '\n') {
                        ++buffer->start;
                    }
                    if (buffer->start[0] == '\n') {
                        ++buffer->start;
                        ++lexer->bufList->lineno;
                    }
                // alpha
                } else if (utf8_isalpha(buffer->start)) {
                    PrattToken *token = lookupTrieSymbol(parser);
                    if (token != NULL) {
                        return token;
                    } else {
                        return parseIdentifier(parser);
                    }
                // digit (no unicode support yet)
                } else if (isdigit(buffer->start[0])) {
                    return parseNumeric(lexer);
                // string
                } else if (buffer->start[0] == '"') {
                    return parseString(parser, false, '"');
                // char
                } else if (buffer->start[0] == '\'') {
                    return parseString(parser, true, '\'');
                // punctuation and symbols
                } else if (utf8_ispunct(buffer->start) || utf8_issymbol(buffer->start)) {
                    PrattToken *token = lookupTrieSymbol(parser);
                    if (token != NULL) {
                        return token;
                    }
                    parserError(parser, "unrecognised operator %c", buffer->start[0]);
                    cant_happen("abort");
                    nextCharacter(buffer);
                    return tokenERROR(lexer);
                // bad UTF8
                } else if (isTrailingByteUtf8((Byte) (buffer->start[0]))) {
                    parserError(parser, "malformed utf8");
                    ++buffer->start;
                    return tokenERROR(lexer);
                // unrecognised
                } else {
                    parserError(parser, "unexpected character 0x%02x", buffer->start[0]);
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
    return newPrattBuffer((unsigned char *)string);
}

static PrattBuffer *prattBufferFromFileName(char *path) {
    char *content = readFile(path);
    return newPrattBuffer((unsigned char *)content);
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

PrattLexer *makePrattLexerFromString(char *input, char *origin) {
    PrattBufList *bl = prattBufListFromString(input, origin, NULL);
    int save = PROTECT(bl);
    PrattLexer *res = newPrattLexer(bl);
    UNPROTECT(save);
    return res;
}

PrattLexer *makePrattLexerFromFilename(char *filename) {
    PrattBufList *bl = prattBufListFromFileName(filename, NULL);
    int save = PROTECT(bl);
    PrattLexer *res = newPrattLexer(bl);
    UNPROTECT(save);
    return res;
}
