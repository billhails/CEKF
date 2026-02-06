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

/**
 * @file pratt_scanner.c
 * @brief The scanner for the Pratt parser.
 */

#include <ctype.h>
#include <stdarg.h>
#include <wctype.h>

#include "bigint.h"
#include "pratt_debug.h"
#include "pratt_scanner.h"
#include "symbol.h"
#include "unicode.h"

#ifdef DEBUG_PRATT_SCANNER
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

/**
 * @brief Defines a token function that returns a static symbol.
 */
#define TOKFN(name, string)                                                    \
    HashSymbol *TOK_##name(void) {                                             \
        static HashSymbol *s = NULL;                                           \
        if (s == NULL)                                                         \
            s = newSymbol(string);                                             \
        return s;                                                              \
    }

TOKFN(ALIAS, "alias")
TOKFN(ARROW, "->")
TOKFN(AS, "as")
TOKFN(ASSERT, "assert")
TOKFN(ASSIGN, "=")
TOKFN(ATOM, " ATOM") // tokens with leading spaces are internal to the parser
TOKFN(BACK, "back")
TOKFN(BANG, "!")
TOKFN(BUILTINS, "__builtins__")
TOKFN(CHAR, " CHAR")
TOKFN(CLOSE, ")")
TOKFN(COLON, ":")
TOKFN(COMMA, ",")
TOKFN(ELSE, "else")
TOKFN(EOF, " EOF")
TOKFN(EQ, "EQ")
TOKFN(ERROR, " ERROR")
TOKFN(EXPORT, "export")
TOKFN(FN, "fn")
TOKFN(HASH, "#")
TOKFN(IF, "if")
TOKFN(IMPORT, "import")
TOKFN(IN, "in")
TOKFN(KW_CHAR, "char")
TOKFN(KW_ERROR, "error")
TOKFN(KW_NUMBER, "number")
TOKFN(LCURLY, "{")
TOKFN(LEFT, "left")
TOKFN(LET, "let")
TOKFN(LINK, "link")
TOKFN(LAZY, "lazy")
TOKFN(LSQUARE, "[")
TOKFN(NAMESPACE, "namespace")
TOKFN(NONE, "none")
TOKFN(NUMBER, " NUMBER")
TOKFN(OPEN, "(")
TOKFN(OPERATOR, "operator")
TOKFN(OPERATORS, "operators")
TOKFN(PERIOD, ".")
TOKFN(PIPE, "|")
TOKFN(PRINT, "print")
TOKFN(RCURLY, "}")
TOKFN(RIGHT, "right")
TOKFN(RSQUARE, "]")
TOKFN(SEMI, ";")
TOKFN(STRING, " STRING")
TOKFN(SWITCH, "switch")
TOKFN(TUPLE, "#(")
TOKFN(TYPEDEF, "typedef")
TOKFN(TYPEOF, "typeof")
TOKFN(UNSAFE, "unsafe")
TOKFN(WILDCARD, "_")

#undef TOKFN

/**
 * @brief Checks if a symbol is an internal symbol.
 */
static inline bool isInternal(HashSymbol *symbol) {
    return symbol->name[0] == ' ';
}

/**
 * @brief Constructs a ParserInfo value from the argument PrattLexer.
 */
ParserInfo LEXPI(PrattLexer *lexer) {
    ParserInfo res;
    res.lineNo = 0;
    res.fileName = "undefined";
    if (lexer) {
        if (lexer->tokenHead) {
            res.lineNo = lexer->tokenHead->lineNo;
            res.fileName = lexer->tokenHead->fileName->name;
        } else if (lexer->bufList) {
            res.lineNo = lexer->bufList->lineNo;
            res.fileName = lexer->bufList->fileName->name;
        }
    }
    return res;
}

/**
 * @brief Prints an error message using the parser's context and sets the parser
 * to panic mode.
 */
void parserError(PrattParser *parser, const char *message, ...) {
    va_list args;
    if (parser->panicMode)
        return;
    parser->panicMode = true;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    PrattBufList *bufList = parser->lexer->bufList;
    if (bufList) {
        ParserInfo pi = {.lineNo = bufList->lineNo,
                         .fileName = bufList->fileName->name};
        can_happen(pi, "");
    } else {
        can_happen(NULLPI, " at EOF");
    }
}

/**
 * @brief If the parser is not already in panic mode, then
 * prints an error message using the argument ParserInfo context and sets the
 * parser to panic mode.
 */
void parserErrorAt(ParserInfo PI, PrattParser *parser, const char *message,
                   ...) {
    va_list args;
    if (parser->panicMode)
        return;
    parser->panicMode = true;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    can_happen(PI, "");
}

static SCharVec *readFileBytes(char *path) {
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
        perror(path); // FIXME shouldn't need to exit here
        exit(1);
    }
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);
    SCharVec *bytes = newSCharVec(fileSize + 1);
    int save = PROTECT(bytes);
    size_t bytes_read = fread(bytes->entries, sizeof(char), fileSize, file);
    bytes->entries[bytes_read] = '\0';
    fclose(file);
    UNPROTECT(save);
    return bytes;
}

/**
 * @brief Reads the contents of a file into a dynamically allocated string.
 */
static WCharVec *readFile(char *path) {
    SCharVec *bytes = readFileBytes(path);
    int save = PROTECT(bytes);
    size_t wideSize = mbstowcs(NULL, bytes->entries, 0);
    if (wideSize == (size_t)-1) {
        can_happen(NULLPI, "invalid encoding in file %s", path);
        WCharVec *data = newWCharVec(1);
        data->entries[0] = L'\0';
        UNPROTECT(save);
        return data;
    }
    WCharVec *data = newWCharVec(wideSize + 1);
    PROTECT(data);
    mbstowcs(data->entries, bytes->entries, wideSize + 1);
    UNPROTECT(save);
    return data;
}

/**
 * @brief Recursively searches a PrattTrie for a matching symbol.
 *
 * This routine will handle overlapping cases, such as when a longer symbol
 * is a prefix of a shorter symbol. It will return the longest match found.
 *
 * @param trie The current node in the trie.
 * @param buffer The buffer containing the input data.
 * @param last The last position in the buffer where a match was found.
 * @param found The last found symbol, if any.
 * @return The found HashSymbol, or NULL if no match is found.
 */
static HashSymbol *lookUpTrieRecursive(PrattTrie *trie, PrattBuffer *buffer,
                                       int last, HashSymbol *found) {
    if (trie == NULL || buffer->start[buffer->offset] > trie->character) {
        buffer->offset = last;
        return found;
    } else if (buffer->start[buffer->offset] < trie->character) {
        return lookUpTrieRecursive(trie->siblings, buffer, last, found);
    }
    ++buffer->offset;
    if (trie->terminal != NULL) {
        // avoid i.e. "orbit" false matching "or"
        if (!unicode_isalnum(trie->character) ||
            !unicode_isalnum(buffer->start[buffer->offset])) {
            found = trie->terminal;
            last = buffer->offset;
        }
    }
    return lookUpTrieRecursive(trie->children, buffer, last, found);
}

/**
 * @brief Creates a new PrattToken from a HashSymbol and a token type.
 */
static PrattToken *tokenFromSymbol(PrattBufList *bufList, HashSymbol *symbol,
                                   HashSymbol *tokenType) {
    PrattValue *value = newPrattValue_Atom(symbol);
    int save = PROTECT(value);
    PrattToken *token = newPrattToken(tokenType, bufList->fileName,
                                      bufList->lineNo, value, NULL);
    UNPROTECT(save);
    return token;
}

/**
 * @brief Creates a new PrattToken from a MaybeBigInt and a token type.
 */
static PrattToken *tokenFromBigInt(PrattBufList *bufList, MaybeBigInt *bi,
                                   HashSymbol *tokenType) {
    PrattValue *value = newPrattValue_Number(bi);
    int save = PROTECT(value);
    PrattToken *token = newPrattToken(tokenType, bufList->fileName,
                                      bufList->lineNo, value, NULL);
    UNPROTECT(save);
    return token;
}

static SCharVec *wVecToCVec(WCharVec *wvec) {
    size_t needed = wcstombs(NULL, wvec->entries, 0);
    SCharVec *cvec = newSCharVec(needed + 1);
    wcstombs(cvec->entries, wvec->entries, needed + 1);
    return cvec;
}

/**
 * @brief Converts a PrattBuffer to a HashSymbol.
 */
static HashSymbol *symbolFromBuffer(PrattBuffer *buffer) {
    WCharVec *data = newWCharVec(buffer->offset + 1);
    int save = PROTECT(data);
    memcpy(data->entries, buffer->start, buffer->offset * sizeof(Character));
    data->entries[buffer->offset] = L'\0';
    SCharVec *bytes = wVecToCVec(data);
    PROTECT(bytes);
    HashSymbol *symbol = newSymbol(bytes->entries);
    UNPROTECT(save);
    return symbol;
}

/**
 * @brief Creates a new PrattToken from a string.
 * Uses the PrattBufList to provide ParserInfo context for the token.
 */
static PrattToken *tokenFromString(PrattBufList *bufList, WCharArray *string,
                                   HashSymbol *tokenType) {
    PrattValue *value = newPrattValue_String(string);
    int save = PROTECT(value);
    PrattToken *token = newPrattToken(tokenType, bufList->fileName,
                                      bufList->lineNo, value, NULL);
    UNPROTECT(save);
    return token;
}

/**
 * @brief Advances the buffer past the current token.
 */
static void advance(PrattBuffer *buffer) {
    buffer->start += buffer->offset;
    buffer->offset = 0;
}

/**
 * @brief Looks up a symbol in the trie and returns a PrattToken if found.
 * Helper for lookUpTrieSymbol.
 */
static inline PrattToken *_lookUpTrieSymbol(PrattParser *parser,
                                            PrattLexer *lexer) {
    PrattParser *p = parser;
    while (p) {
        HashSymbol *symbol =
            lookUpTrieRecursive(p->trie, lexer->bufList->buffer, 0, NULL);
        if (symbol != NULL) {
            PrattToken *res = tokenFromSymbol(lexer->bufList, symbol, symbol);
            advance(lexer->bufList->buffer);
            return res;
        }
        p = p->next;
    }
    return NULL;
}

/**
 * @brief Parses and returns the next token.
 */
static PrattToken *lookUpTrieSymbol(PrattParser *parser) {
    return _lookUpTrieSymbol(parser, parser->lexer);
}

/**
 * @brief Parses an identifier from the current position in the buffer.
 */
static PrattToken *parseIdentifier(PrattParser *parser) {
    PrattBuffer *buffer = parser->lexer->bufList->buffer;
    while (true) {
        if (unicode_isalnum(buffer->start[buffer->offset])) {
            ++buffer->offset;
        } else {
#ifdef SAFETY_CHECKS
            if (buffer->offset == 0) {
                cant_happen("parseIdentifier passed bad identifier");
            }
#endif
            HashSymbol *symbol = symbolFromBuffer(buffer);
            PrattToken *token =
                tokenFromSymbol(parser->lexer->bufList, symbol, TOK_ATOM());
            advance(buffer);
            return token;
        }
    }
}

/**
 * @brief Multiplies a bigint by an integer n, replacing the bigint with the
 * result.
 */
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

/**
 * @brief Adds an integer n to a bigint, replacing the bigint with the result.
 */
static void bigint_add_n(bigint *b, int n) {
    bigint old;
    bigint_init(&old);
    bigint_cpy(&old, b);
    bigint_free(b);
    bigint_init(b);
    bigint_add_word(b, &old, n);
    bigint_free(&old);
}

/**
 * @brief Converts a character to its numeric value, handling hexadecimal and
 * non-latin digits.
 *
 * @param c The character to convert.
 * @return The numeric value of the character.
 */
static int digitToInt(Character c) {
    switch (c) {
    case L'a':
    case L'b':
    case L'c':
    case L'd':
    case L'e':
    case L'f':
        return 10 + (c - L'a');
    case L'A':
    case L'B':
    case L'C':
    case L'D':
    case L'E':
    case L'F':
        return 10 + (c - L'A');
    default:
        return unicode_getdec(c);
    }
}

/**
 * @brief Creates a MaybeBigInt from a string of digits.
 *
 * @param digits The string of digits to convert, has already been validated.
 * @param length The length of the string.
 * @return A MaybeBigInt representing the number.
 */
static MaybeBigInt *makeMaybeBigInt(Character *digits, int length) {
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
    for (Character *p = digits; length > 0; --length, ++p) {
        if (*p == L'_')
            continue;
        if (*p == L'i') {
            imag = true; // parseNumeric guarantees this is last
            continue;
        }
        int n = digitToInt(*p);
        if (overflowed) {
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

/**
 * @brief Creates a MaybeBigInt representing an irrational number.
 *
 * MaybeBigInt is a misleading term as it can contain other numeric types than
 * just integers.
 */
static MaybeBigInt *makeIrrational(Character *str, int length) {
    bool imag = false;
    bool frac = false;
    Double f = 0.0;
    Double div = 1.0;
    for (Character *p = str; length > 0; --length, ++p) {
        switch (*p) {
        case L'_':
            continue;
        case L'i':
            imag = true;
            continue;
        case L'.':
            frac = true;
            continue;
        default: {
            int n = digitToInt(*p);
            f *= 10.0;
            f += n;
            if (frac)
                div *= 10.0;
        }
        }
    }
    return irrationalBigInt(f / div, imag);
}

/**
 * @brief Parses a numeric token from the current position in the buffer.
 */
static PrattToken *parseNumeric(PrattLexer *lexer) {
    PrattBuffer *buffer = lexer->bufList->buffer;
    HashSymbol *type = TOK_NUMBER();
    PrattNumberState state = PRATTNUMBERSTATE_TYPE_START;
    bool floating = false;
    while (state != PRATTNUMBERSTATE_TYPE_END) {
        if (buffer->start[buffer->offset] == L'_') {
            ++buffer->offset;
            continue;
        }
        switch (state) {
        case PRATTNUMBERSTATE_TYPE_START:
            switch (buffer->start[buffer->offset]) {
            case L'0':
                ++buffer->offset;
                state = PRATTNUMBERSTATE_TYPE_ZERO;
                break;
            default:
                ++buffer->offset;
                state = PRATTNUMBERSTATE_TYPE_DEC;
                break;
            }
            break;
        case PRATTNUMBERSTATE_TYPE_ZERO:
            if (unicode_isdigit(buffer->start[buffer->offset])) {
                ++buffer->offset;
                state = PRATTNUMBERSTATE_TYPE_DEC;
                break;
            } else {
                switch (buffer->start[buffer->offset]) {
                case L'x':
                case L'X':
                    ++buffer->offset;
                    state = PRATTNUMBERSTATE_TYPE_HEX;
                    break;
                case L'.':
                    ++buffer->offset;
                    state = PRATTNUMBERSTATE_TYPE_FLOAT;
                    floating = true;
                    break;
                case L'i':
                    ++buffer->offset;
                    state = PRATTNUMBERSTATE_TYPE_END;
                    break;
                default:
                    state = PRATTNUMBERSTATE_TYPE_END;
                    break;
                }
            }
            break;
        case PRATTNUMBERSTATE_TYPE_HEX:
            switch (buffer->start[buffer->offset]) {
            case L'0':
            case L'1':
            case L'2':
            case L'3':
            case L'4':
            case L'5':
            case L'6':
            case L'7':
            case L'8':
            case L'9':
            case L'a':
            case L'b':
            case L'c':
            case L'd':
            case L'e':
            case L'f':
            case L'A':
            case L'B':
            case L'C':
            case L'D':
            case L'E':
            case L'F':
            case L'_':
                ++buffer->offset;
                break;
            case L'i':
                ++buffer->offset;
                state = PRATTNUMBERSTATE_TYPE_END;
                break;
            default:
                state = PRATTNUMBERSTATE_TYPE_END;
                break;
            }
            break;
        case PRATTNUMBERSTATE_TYPE_DEC:
            if (unicode_isdigit(buffer->start[buffer->offset])) {
                ++buffer->offset;
                break;
            } else {
                switch (buffer->start[buffer->offset]) {
                case L'.':
                    ++buffer->offset;
                    state = PRATTNUMBERSTATE_TYPE_FLOAT;
                    floating = true;
                    break;
                case L'i':
                    ++buffer->offset;
                    state = PRATTNUMBERSTATE_TYPE_END;
                    break;
                default:
                    state = PRATTNUMBERSTATE_TYPE_END;
                    break;
                }
            }
            break;
        case PRATTNUMBERSTATE_TYPE_FLOAT:
            if (unicode_isdigit(buffer->start[buffer->offset])) {
                ++buffer->offset;
                break;
            } else {
                switch (buffer->start[buffer->offset]) {
                case L'i':
                    ++buffer->offset;
                    state = PRATTNUMBERSTATE_TYPE_END;
                    break;
                default:
                    state = PRATTNUMBERSTATE_TYPE_END;
                    break;
                }
            }
            break;
        case PRATTNUMBERSTATE_TYPE_END:
            cant_happen("end state in loop");
        }
    }
    MaybeBigInt *bi = NULL;
    if (floating) {
        bi = makeIrrational(buffer->start, buffer->offset);
    } else {
        bi = makeMaybeBigInt(buffer->start, buffer->offset);
    }
    int save = PROTECT(bi);
    PrattToken *token = tokenFromBigInt(lexer->bufList, bi, type);
    advance(buffer);
    UNPROTECT(save);
    return token;
}

/**
 * @brief Dequeues the next token from the lexer.
 *
 * If the token queue is empty, returns NULL.
 */
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

/**
 * @brief Enqueues a token to the lexer.
 *
 * If the queue is empty, sets both head and tail to the new token.
 * Otherwise, appends the new token to the end of the queue.
 */
void enqueueToken(PrattLexer *lexer, PrattToken *token) {
    token->next = NULL;
    if (lexer->tokenTail == NULL) { // Queue is empty
        lexer->tokenHead = lexer->tokenTail = token;
    } else {
        lexer->tokenTail->next = token;
        lexer->tokenTail = token;
    }
}

/**
 * @brief Creates a new EOF token.
 *
 * This token is used to indicate the end of the input stream.
 */
static PrattToken *tokenEOF() {
    return newPrattToken(TOK_EOF(), newSymbol("eof"), 0, NULL, NULL);
}

/**
 * @brief Creates a new error token.
 *
 * This token is used to indicate an error in the input stream and contains
 * the fileName and line number from the lexer where the error occurred.
 */
static PrattToken *tokenERROR(PrattLexer *lexer) {
    ParserInfo PI = LEXPI(lexer);
    return newPrattToken(TOK_ERROR(), newSymbol(PI.fileName), PI.lineNo, NULL,
                         NULL);
}

/**
 * @brief Parses a string or character from the current position in the buffer.
 *
 * This function handles both single quoted characters and double-quoted
 * strings, including escape sequences.
 * It returns a PrattToken containing the parsed character or string.
 */
static PrattToken *parseString(PrattParser *parser, bool parsingSingleChar,
                               Character sep) {
    PrattLexer *lexer = parser->lexer;
    PrattBuffer *buffer = lexer->bufList->buffer;
    WCharArray *string = newWCharArray();
    int save = PROTECT(string);
    PrattStringState state = PRATTSTRINGSTATE_TYPE_START;
    Character uni = 0;
    while (state != PRATTSTRINGSTATE_TYPE_END) {
        switch (state) {
        case PRATTSTRINGSTATE_TYPE_START:
            DEBUG("parseString %s %d (sep %lc) START: %lc",
                  lexer->bufList->fileName->name, lexer->bufList->lineNo, sep,
                  buffer->start[buffer->length]);
#ifdef SAFETY_CHECKS
            if (buffer->start[buffer->offset] != sep) {
                cant_happen("expected '%lc' got '%lc'", sep,
                            buffer->start[buffer->offset]);
            }
#endif
            ++buffer->offset;
            state = PRATTSTRINGSTATE_TYPE_STR;
            break;
        case PRATTSTRINGSTATE_TYPE_STR:
        case PRATTSTRINGSTATE_TYPE_ESCS:
            DEBUG("parseString %s %d (sep %lc) STR: %lc",
                  lexer->bufList->fileName->name, lexer->bufList->lineNo, sep,
                  buffer->start[buffer->offset]);
            if (state == PRATTSTRINGSTATE_TYPE_STR) {
                if (buffer->start[buffer->offset] == sep) {
                    if (parsingSingleChar) {
                        parserError(parser, "empty char");
                    }
                    ++buffer->offset;
                    state = PRATTSTRINGSTATE_TYPE_END;
                } else {
                    switch (buffer->start[buffer->offset]) {
                    case L'\\':
                        ++buffer->offset;
                        state = PRATTSTRINGSTATE_TYPE_ESC;
                        break;
                    case '\n':
                        parserError(parser, "unexpected EOL");
                        ++buffer->offset;
                        ++lexer->bufList->lineNo;
                        break;
                    case L'\0':
                        parserError(parser, "unexpected EOF");
                        state = PRATTSTRINGSTATE_TYPE_END;
                        break;
                    default:
                        pushWCharArray(string, buffer->start[buffer->offset]);
                        ++buffer->offset;
                        state = parsingSingleChar ? PRATTSTRINGSTATE_TYPE_CHR
                                                  : PRATTSTRINGSTATE_TYPE_STR;
                        break;
                    }
                }
            } else { // PRATTSTRINGSTATE_TYPE_ESCS
                switch (buffer->start[buffer->offset]) {
                case L'\n':
                    parserError(parser, "unexpected EOL");
                    ++buffer->offset;
                    ++lexer->bufList->lineNo;
                    break;
                case '\0':
                    parserError(parser, "unexpected EOF");
                    state = PRATTSTRINGSTATE_TYPE_END;
                    break;
                default:
                    pushWCharArray(string, buffer->start[buffer->offset]);
                    ++buffer->offset;
                    state = parsingSingleChar ? PRATTSTRINGSTATE_TYPE_CHR
                                              : PRATTSTRINGSTATE_TYPE_STR;
                    break;
                }
            }

            break;
        case PRATTSTRINGSTATE_TYPE_ESC:
            DEBUG("parseString %s %d (sep %lc) ESC: %lc",
                  lexer->bufList->fileName->name, lexer->bufList->lineNo, sep,
                  buffer->start[buffer->offset]);
            switch (buffer->start[buffer->offset]) {
            case L'u':
            case L'U':
                ++buffer->offset;
                uni = 0; // reset
                state = PRATTSTRINGSTATE_TYPE_UNI;
                break;
            case L'n':
                pushWCharArray(string, L'\n');
                ++buffer->offset;
                state = parsingSingleChar ? PRATTSTRINGSTATE_TYPE_CHR
                                          : PRATTSTRINGSTATE_TYPE_STR;
                break;
            case L't':
                pushWCharArray(string, L'\t');
                ++buffer->offset;
                state = parsingSingleChar ? PRATTSTRINGSTATE_TYPE_CHR
                                          : PRATTSTRINGSTATE_TYPE_STR;
                break;
            case L'\n':
                parserError(parser, "unexpected EOL");
                ++buffer->offset;
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
            DEBUG("parseString %s %d (sep %lc) UNI: %lc",
                  lexer->bufList->fileName->name, lexer->bufList->lineNo, sep,
                  buffer->start[buffer->offset]);
            switch (buffer->start[buffer->offset]) {
            case L'0':
            case L'1':
            case L'2':
            case L'3':
            case L'4':
            case L'5':
            case L'6':
            case L'7':
            case L'8':
            case L'9': {
                Character c = buffer->start[buffer->offset] - L'0';
                uni <<= 4;
                uni |= c;
                buffer->offset++;
            } break;
            case L'a':
            case L'b':
            case L'c':
            case L'd':
            case L'e':
            case L'f': {
                Character c = 10 + buffer->start[buffer->offset] - L'a';
                uni <<= 4;
                uni |= c;
                buffer->offset++;
            } break;
            case L'A':
            case L'B':
            case L'C':
            case L'D':
            case L'E':
            case L'F': {
                Character c = 10 + buffer->start[buffer->offset] - L'A';
                uni <<= 4;
                uni |= c;
                buffer->offset++;
            } break;
            case L';':
                ++buffer->offset;
                if (uni == 0) {
                    parserError(parser,
                                "Empty Unicode escape while parsing string");
                } else {
                    pushWCharArray(string, uni);
                }
                state = parsingSingleChar ? PRATTSTRINGSTATE_TYPE_CHR
                                          : PRATTSTRINGSTATE_TYPE_STR;
                break;
            case L'\0':
                parserError(parser, "EOF while parsing unicode escape");
                state = PRATTSTRINGSTATE_TYPE_END;
                break;
            default:
                parserError(parser, "expected hex digit or ';'");
                state = parsingSingleChar ? PRATTSTRINGSTATE_TYPE_CHR
                                          : PRATTSTRINGSTATE_TYPE_STR;
                ++buffer->offset;
                break;
            }
            break;
        case PRATTSTRINGSTATE_TYPE_CHR: // only get here if single == true
            DEBUG("parseString %s %d (sep %lc) CHR1: %lc",
                  lexer->bufList->fileName->name, lexer->bufList->lineNo, sep,
                  buffer->start[buffer->offset]);
            if (buffer->start[buffer->offset] == sep) {
                ++buffer->offset;
                state = PRATTSTRINGSTATE_TYPE_END;
            } else {
                parserError(parser, "expected \"'\" terminator");
                ++buffer->offset;
                state = PRATTSTRINGSTATE_TYPE_END;
            }
            break;
        case PRATTSTRINGSTATE_TYPE_END:
            cant_happen("end state in loop");
        }
    }
    pushWCharArray(string, '\0');
    PrattToken *token = tokenFromString(
        lexer->bufList, string, parsingSingleChar ? TOK_CHAR() : TOK_STRING());
    advance(buffer);
    UNPROTECT(save);
    return token;
}

/**
 * @brief Advances the buffer past the next UTF8 character and returns the
 * character.
 *
 * This function updates the start pointer of the buffer to point past the next
 * character in the UTF-8 encoded string and returns the character.
 */
static inline Character nextCharacter(PrattBuffer *buffer) {
    Character dest = buffer->start[0];
    buffer->start++;
    return dest;
}

/**
 * @brief Parses the next token from the lexer.
 *
 * This function reads the next token from the lexer, handling whitespace,
 * comments, identifiers, numeric literals, strings, characters, and symbols. It
 * returns a PrattToken representing the next token in the input stream.
 */
PrattToken *next(PrattParser *parser) {
    PrattLexer *lexer = parser->lexer;
    PrattToken *lookahead = dequeueToken(lexer);
    if (lookahead != NULL) {
        return lookahead;
    }
    while (lexer->bufList != NULL) {
        PrattBuffer *buffer = lexer->bufList->buffer;
        if (buffer->start == NULL) {
            buffer->start = buffer->data->entries;
        }
        while (buffer->start[0]) {
            // whitespace
            if (unicode_isspace(buffer->start[0])) {
                if (buffer->start[0] == L'\n') {
                    ++lexer->bufList->lineNo;
                }
                nextCharacter(buffer);
                // comment
            } else if (buffer->start[0] == L'/' && buffer->start[1] == L'/') {
                while (buffer->start[0] && buffer->start[0] != L'\n') {
                    ++buffer->start;
                }
                if (buffer->start[0] == L'\n') {
                    ++buffer->start;
                    ++lexer->bufList->lineNo;
                }
                // alpha or combining mark
            } else if (unicode_isalpha(buffer->start[0]) ||
                       unicode_ismark(buffer->start[0])) {
                PrattToken *token = lookUpTrieSymbol(parser);
                if (token != NULL) {
                    return token;
                } else {
                    return parseIdentifier(parser);
                }
                // number
            } else if (unicode_isdigit(buffer->start[0])) {
                return parseNumeric(lexer);
                // string
            } else if (buffer->start[0] == L'"') {
                return parseString(parser, false, L'"');
                // char
            } else if (buffer->start[0] == L'\'') {
                return parseString(parser, true, L'\'');
                // punctuation and symbols
            } else if (unicode_ispunct(buffer->start[0]) ||
                       unicode_issymbol(buffer->start[0])) {
                PrattToken *token = lookUpTrieSymbol(parser);
                if (token != NULL) {
                    return token;
                }
                parserError(parser, "unrecognised operator %lc",
                            buffer->start[0]);
                buffer->start++;
                return tokenERROR(lexer);
                // unrecognised
            } else {
                parserError(parser, "unexpected character %lc",
                            buffer->start[0]);
                ++buffer->start;
                return tokenERROR(lexer);
            }
        }
        lexer->bufList = lexer->bufList->next;
        if (lexer->bufList) {
            DEBUG("next buffer %s", lexer->bufList->fileName->name);
        }
    }
    return tokenEOF();
}

/**
 * @brief Adds a symbol to the trie, creating new nodes as needed.
 *
 * This function recursively adds a symbol to the trie structure, creating new
 * nodes for each character in the symbol's name. If the end of the name is
 * reached, it marks the node as terminal with the given symbol.
 * @param symbol The symbol to add to the trie.
 * @param siblings The siblings of the current node in the trie.
 * @param bytes The remaining bytes of the symbol's name to process.
 * @return A pointer to the newly created PrattTrie node, or NULL if the end of
 * the name is reached.
 */
static PrattTrie *addToTrie(HashSymbol *symbol, PrattTrie *siblings,
                            Character *chars) {
    if (*chars == 0) {
        return NULL;
    }
    PrattTrie *next = addToTrie(symbol, NULL, chars + 1);
    if (next == NULL) {
        return newPrattTrie(*chars, symbol, siblings, NULL);
    }
    int save = PROTECT(next);
    PrattTrie *this = newPrattTrie(*chars, NULL, siblings, next);
    UNPROTECT(save);
    return this;
}

/**
 * @brief Inserts a symbol into the trie, creating new nodes if needed.
 *
 * This function recursively inserts a symbol into the trie structure, creating
 * new nodes for each character in the symbol's name. If the end of the name is
 * reached, it marks the node as terminal with the given symbol.
 *
 * If the symbol already exists in the trie, this function does nothing.
 *
 * @param current The current node in the trie.
 * @param symbol The symbol to insert into the trie.
 * @param bytes The remaining bytes of the symbol's name to process.
 * @return A pointer to the updated PrattTrie node, or NULL if no changes were
 * made.
 */
static PrattTrie *insertTrie(PrattTrie *current, HashSymbol *symbol,
                             Character *chars) {
    if (*chars == 0) {
        return NULL;
    }
    if (current == NULL) {
        return addToTrie(symbol, NULL, chars);
    }
    if (*chars < current->character) {
        PrattTrie *sibling = insertTrie(current->siblings, symbol, chars);
        int save = PROTECT(sibling);
        PrattTrie *this = newPrattTrie(current->character, current->terminal,
                                       sibling, current->children);
        UNPROTECT(save);
        return this;
    }
    if (*chars == current->character) {
        PrattTrie *child = insertTrie(current->children, symbol, chars + 1);
        if (child == NULL) {
            return newPrattTrie(current->character, symbol, current->siblings,
                                current->children);
        }
        int save = PROTECT(child);
        PrattTrie *this = newPrattTrie(current->character, current->terminal,
                                       current->siblings, child);
        UNPROTECT(save);
        return this;
    }
    return addToTrie(symbol, current, chars);
}

/**
 * @brief Inserts a symbol into the PrattTrie, skipping internal tokens.
 *
 * This function checks if the symbol is an internal token and skips it if so.
 * Otherwise, it inserts the symbol into the trie structure.
 *
 * @param current The current node in the PrattTrie.
 * @param symbol The symbol to insert into the PrattTrie.
 * @return A pointer to the updated PrattTrie node, or the current node if no
 * changes were made.
 */
PrattTrie *insertPrattTrie(PrattTrie *current, HashSymbol *symbol) {
    if (isInternal(symbol)) {
        return current; // skip internal tokens
    }
    size_t len = mbstowcs(NULL, symbol->name, 0) + 1;
    WCharVec *chars = newWCharVec(len);
    int save = PROTECT(chars);
    mbstowcs(chars->entries, symbol->name, len);
    PrattTrie *this = insertTrie(current, symbol, chars->entries);
    UNPROTECT(save);
    if (this == NULL) {
        return current;
    }
    return this;
}

/**
 * @brief Creates a new PrattBuffer from a string.
 *
 * This function allocates a new PrattBuffer and initializes it from a copy of
 * the given string. The copy is stored as a wchar_t array to simplify parsing.
 *
 * @param string The string to convert into a PrattBuffer.
 * @return A pointer to the newly created PrattBuffer.
 */
static PrattBuffer *prattBufferFromString(char *string) {
    size_t len = mbstowcs(NULL, string, 0);
    if (len == (size_t)-1) {
        return NULL;
    }
    WCharVec *data = newWCharVec(len + 1);
    int save = PROTECT(data);
    mbstowcs(data->entries, string, len + 1);
    PrattBuffer *res = newPrattBuffer(data);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Creates a new PrattBuffer from the contents of a file passed by name.
 *
 * This function reads the contents of the file specified by the path and
 * creates a new PrattBuffer initialized with the file's content.
 *
 * @param path The path to the file to read.
 * @return A pointer to the newly created PrattBuffer.
 */
static PrattBuffer *prattBufferFromFileName(char *path) {
    WCharVec *content = readFile(path);
    int save = PROTECT(content);
    PrattBuffer *res = newPrattBuffer(content);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Re-enqueues a token back into the lexer.
 *
 * @param parser The PrattParser instance containing the lexer.
 * @param token The PrattToken to re-enqueue.
 */
void poke(PrattParser *parser, PrattToken *token) {
    enqueueToken(parser->lexer, token);
}

/**
 * @brief Peeks at the next token without consuming it.
 */
PrattToken *peek(PrattParser *parser) {
    // DEBUG("peek");
    PrattToken *token = next(parser);
    int save = PROTECT(token);
    poke(parser, token);
    UNPROTECT(save);
    return token;
}

/**
 * @brief Checks if the next token matches the specified type, without consuming
 * the token.
 *
 * @param parser The PrattParser instance to check.
 * @param type The HashSymbol representing the expected token type.
 * @return true if the next token matches the type, false otherwise.
 */
bool check(PrattParser *parser, HashSymbol *type) {
    PrattToken *token = peek(parser);
    return token->type == type;
}

/**
 * @brief Matches the next token against the specified type and consumes it if
 * it matches.
 *
 * @param parser The PrattParser instance to check.
 * @param type The HashSymbol representing the expected token type.
 * @return true if the next token matches the type, false otherwise.
 */
bool match(PrattParser *parser, HashSymbol *type) {
    PrattToken *token = next(parser);
    if (token->type == type) {
        validateLastAlloc();
        return true;
    }
    int save = PROTECT(token);
    poke(parser, token);
    UNPROTECT(save);
    return false;
}

/**
 * @brief Consumes the next token if it matches the specified type.
 *
 * If the match is successful, it consumes the token and returns true.
 * If the match fails, it consumes the token, raises an error, and returns
 * false.
 */
static const char *friendlyExpectedName(HashSymbol *type) {
    if (type == TOK_ATOM())
        return "identifier";
    return type->name;
}

bool consume(PrattParser *parser, HashSymbol *type) {
    PrattToken *token = next(parser);
    validateLastAlloc();
    if (token->type == type) {
        return true;
    }
    const char *expectedName = friendlyExpectedName(type);
    const char *foundKind = NULL;
    const char *foundLexeme = NULL;
    if (token->type == TOK_ATOM()) {
        foundKind = "identifier";
        foundLexeme = token->value->val.atom->name;
    } else {
        // For operators and keywords the token type name is the lexeme
        foundKind = "token";
        foundLexeme = token->type->name;
    }
    if (token->type == TOK_EOF()) {
        parserError(parser, "unexpected EOF (expecting '%s')", expectedName);
    } else if (type == TOK_ATOM() && token->type != TOK_ATOM()) {
        // Helpful hint: common case is trying to name a function with an
        // operator symbol
        parserError(parser, "expected '%s' found operator '%s'", expectedName,
                    foundLexeme);
    } else {
        parserError(parser, "expected '%s' found %s '%s'", expectedName,
                    foundKind, foundLexeme);
    }
    return false;
}

/**
 * @brief Creates a new PrattBufList from a file name.
 *
 * This function creates a new PrattBufList containing a single PrattBuffer
 * initialized with the contents of the file specified by the file name and
 * pointing at the next PrattBufList.
 *
 * @param fileName The name of the file to read.
 * @param next The next PrattBufList in the chain, or NULL.
 * @return A pointer to the newly created PrattBufList.
 */
PrattBufList *prattBufListFromFileName(char *fileName, PrattBufList *next) {
    PrattBuffer *buffer = prattBufferFromFileName(fileName);
    int save = PROTECT(buffer);
    PrattBufList *res = newPrattBufList(1, newSymbol(fileName), buffer, next);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Creates a new PrattBufList from a string.
 *
 * This function creates a new PrattBufList containing a single PrattBuffer
 * initialized with the given string and pointing at the next PrattBufList.
 *
 * @param string The string to convert into a PrattBuffer.
 * @param origin The origin of the string, used for error reporting.
 * @param next The next PrattBufList in the chain, or NULL.
 * @return A pointer to the newly created PrattBufList.
 */
static PrattBufList *prattBufListFromMbString(char *string, char *origin,
                                              PrattBufList *next) {
    PrattBuffer *buffer = prattBufferFromString(string);
    if (buffer == NULL) {
        can_happen(NULLPI, "invalid encoding in %s", origin);
        buffer = prattBufferFromString("");
    }
    int save = PROTECT(buffer);
    PrattBufList *res = newPrattBufList(1, newSymbol(origin), buffer, next);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Creates a new PrattLexer from a string.
 *
 * This function initializes a new PrattBufList with the given string and
 * origin, then creates and returns a new PrattLexer using that buffer list.
 *
 * @param input The input string to create the lexer from.
 * @param origin The origin of the input string, used for error reporting.
 * @return A pointer to the newly created PrattLexer.
 */
PrattLexer *makePrattLexerFromMbString(char *input, char *origin) {
    PrattBufList *bl = prattBufListFromMbString(input, origin, NULL);
    int save = PROTECT(bl);
    PrattLexer *res = newPrattLexer(bl);
    UNPROTECT(save);
    return res;
}

/**
 * @brief Creates a new PrattLexer from a file name.
 *
 * This function creates a new PrattBufList from the argument file, and
 * initializes a new PrattLexer using that buffer list.
 *
 * @param fileName The name of the file to read.
 * @return A pointer to the newly created PrattLexer.
 */
PrattLexer *makePrattLexerFromFileName(char *fileName) {
    PrattBufList *bl = prattBufListFromFileName(fileName, NULL);
    int save = PROTECT(bl);
    PrattLexer *res = newPrattLexer(bl);
    UNPROTECT(save);
    return res;
}
