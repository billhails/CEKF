#include "regex_helper.h"

#include "memory.h"
#include "regex_source.h"
#include "unicode.h"

typedef Index RegexPosition;

#define REGEX_INLINE_FLAG_CASE_INSENSITIVE ((unsigned char)0x01)
#define REGEX_INLINE_FLAG_DOTALL ((unsigned char)0x02)

static void compileError(RegexStatus *status, Index *errorOffset,
                         RegexStatus code, Index offset);
static void ensureRegexMemoryReady(void);
static CharacterArray *copyNullTerminatedText(const Character *text);
static bool regexFlagEnabled(unsigned char flags, unsigned char mask);
static bool parseInlineFlag(Character flag, unsigned char *flags);
static bool parseLeadingInlineFlags(const Character *pattern, Index *cursor,
                                    unsigned char *flags, RegexStatus *status,
                                    Index *errorOffset);
static bool simpleFoldCharacter(Character c, Character *folded);
static bool matchCharacters(Character left, Character right,
                            unsigned char flags);
static bool matchRange(Character lower, Character upper, Character c,
                       unsigned char flags);
static RegexNode *newEmptyNode(void);
static RegexNode *newAtomNode(RegexAtom *atom);
static RegexNode *newConcatNode(void);
static RegexNode *newAlternationNode(void);
static RegexNode *newRepeatNode(RegexNode *child, Index min, Index max,
                                bool unlimited);
static bool appendNode(RegexNodeArray *nodes, RegexNode *child);
static bool appendClassItem(RegexCharClass *charClass, RegexClassItem *item);
static bool appendPosition(RegexPositionArray *set, RegexPosition position);
static bool appendPositions(RegexPositionArray *dest,
                            const RegexPositionArray *source);
static RegexPosition positionAt(const RegexPositionArray *set, Index index);
static bool parseEscapedAtom(const Character *pattern, Index *cursor,
                             RegexAtom **atom, RegexStatus *status,
                             Index *errorOffset);
static bool parseUnicodeEscape(const Character *pattern, Index *cursor,
                               Character *value, RegexStatus *status,
                               Index *errorOffset);
static bool parseEscapedClassItem(const Character *pattern, Index *cursor,
                                  RegexClassItem **item, RegexStatus *status,
                                  Index *errorOffset);
static bool parseCategoryName(const Character *name, Index length,
                              unsigned char *code, bool *exact);
static bool parseNamedCategory(const Character *pattern, Index *cursor,
                               RegexCategory **category, RegexStatus *status,
                               Index *errorOffset);
static RegexCharClass *parseCharClass(const Character *pattern, Index *cursor,
                                      RegexStatus *status, Index *errorOffset);
static RegexNode *parseExpression(const Character *pattern, Index *cursor,
                                  RegexStatus *status, Index *errorOffset);
static RegexNode *parseSequence(const Character *pattern, Index *cursor,
                                RegexStatus *status, Index *errorOffset);
static RegexNode *parseQuantified(const Character *pattern, Index *cursor,
                                  RegexStatus *status, Index *errorOffset);
static RegexNode *parsePrimary(const Character *pattern, Index *cursor,
                               RegexStatus *status, Index *errorOffset);
static bool matchCategory(const RegexCategory *category, Character c);
static bool matchMeta(RegexMetaType metaType, Character c);
static bool matchClassItem(const RegexClassItem *item, Character c,
                           unsigned char flags);
static bool matchCharClass(const RegexCharClass *charClass, Character c,
                           unsigned char flags);
static bool matchAtom(const RegexAtom *atom, Character c, unsigned char flags);
static bool matchNode(const RegexNode *node, RegexSource *source,
                      RegexPosition position, RegexPosition inputStart,
                      unsigned char flags, RegexPositionArray *out);
static bool matchSequence(const RegexNodeArray *list, RegexSource *source,
                          RegexPosition position, RegexPosition inputStart,
                          unsigned char flags, RegexPositionArray *out);
static bool matchAlternation(const RegexNodeArray *list, RegexSource *source,
                             RegexPosition position, RegexPosition inputStart,
                             unsigned char flags, RegexPositionArray *out);
static bool matchRepeat(const RegexRepeat *repeat, RegexSource *source,
                        RegexPosition position, RegexPosition inputStart,
                        unsigned char flags, RegexPositionArray *out);
static bool matchRepeatGreedy(const RegexRepeat *repeat, RegexSource *source,
                              RegexPosition position, RegexPosition inputStart,
                              unsigned char flags, Index count,
                              RegexPositionArray *out);

static void compileError(RegexStatus *status, Index *errorOffset,
                         RegexStatus code, Index offset) {
    if (status != NULL) {
        *status = code;
    }

    if (errorOffset != NULL) {
        *errorOffset = offset;
    }
}

static void ensureRegexMemoryReady(void) {
    if (!protectionInitialized()) {
        initProtection();
    }
}

static CharacterArray *copyNullTerminatedText(const Character *text) {
    CharacterArray *chars = newCharacterArray();
    int save = PROTECT(chars);

    while (true) {
        Character current = *text;

        pushCharacterArray(chars, current);
        if (current == L'\0') {
            break;
        }
        text++;
    }

    UNPROTECT(save);
    return chars;
}

static bool regexFlagEnabled(unsigned char flags, unsigned char mask) {
    return (flags & mask) != 0;
}

static bool parseInlineFlag(Character flag, unsigned char *flags) {
    switch (flag) {
    case L'i':
        *flags |= REGEX_INLINE_FLAG_CASE_INSENSITIVE;
        return true;
    default:
        return false;
    }
}

static bool parseLeadingInlineFlags(const Character *pattern, Index *cursor,
                                    unsigned char *flags, RegexStatus *status,
                                    Index *errorOffset) {
    Index scan;
    bool sawFlag = false;

    *flags = 0;

    if (pattern[*cursor] != L'(' || pattern[*cursor + 1] != L'?') {
        return true;
    }

    scan = *cursor + 2;

    while (true) {
        Character current = pattern[scan];

        if (current == L')') {
            if (!sawFlag) {
                compileError(status, errorOffset,
                             REGEX_STATUS_INVALID_INLINE_FLAG, scan);
                return false;
            }

            *cursor = scan + 1;
            return true;
        }

        if (current == L'\0') {
            compileError(status, errorOffset, REGEX_STATUS_INVALID_INLINE_FLAG,
                         scan);
            return false;
        }

        if ((current >= L'a' && current <= L'z') ||
            (current >= L'A' && current <= L'Z')) {
            if (!parseInlineFlag(current, flags)) {
                compileError(status, errorOffset,
                             REGEX_STATUS_INVALID_INLINE_FLAG, scan);
                return false;
            }

            sawFlag = true;
            scan++;
            continue;
        }

        compileError(status, errorOffset, REGEX_STATUS_INVALID_INLINE_FLAG,
                     scan);
        return false;
    }
}

static bool simpleFoldCharacter(Character c, Character *folded) {
    Character buffer[2];
    Index length = unicode_tolower(c, buffer, 2);

    if (length != 1) {
        return false;
    }

    *folded = buffer[0];
    return true;
}

static bool matchCharacters(Character left, Character right,
                            unsigned char flags) {
    Character leftFold;
    Character rightFold;

    if (left == right) {
        return true;
    }

    if (!regexFlagEnabled(flags, REGEX_INLINE_FLAG_CASE_INSENSITIVE)) {
        return false;
    }

    if (!simpleFoldCharacter(left, &leftFold) ||
        !simpleFoldCharacter(right, &rightFold)) {
        return false;
    }

    return leftFold == rightFold;
}

static bool matchRange(Character lower, Character upper, Character c,
                       unsigned char flags) {
    Character lowerFold;
    Character upperFold;
    Character cFold;

    if (c >= lower && c <= upper) {
        return true;
    }

    if (!regexFlagEnabled(flags, REGEX_INLINE_FLAG_CASE_INSENSITIVE)) {
        return false;
    }

    if (!simpleFoldCharacter(lower, &lowerFold) ||
        !simpleFoldCharacter(upper, &upperFold) ||
        !simpleFoldCharacter(c, &cFold)) {
        return false;
    }

    if (lowerFold <= upperFold) {
        return cFold >= lowerFold && cFold <= upperFold;
    }

    return cFold >= upperFold && cFold <= lowerFold;
}

static RegexNode *newEmptyNode(void) { return newRegexNode_Empty(); }

static RegexNode *newAtomNode(RegexAtom *atom) {
    int save = PROTECT(atom);
    RegexNode *node = newRegexNode_Atom(atom);
    UNPROTECT(save);
    return node;
}

static RegexNode *newConcatNode(void) {
    RegexNodeArray *items = newRegexNodeArray();
    int save = PROTECT(items);
    RegexNode *node = newRegexNode_Concat(items);
    UNPROTECT(save);
    return node;
}

static RegexNode *newAlternationNode(void) {
    RegexNodeArray *items = newRegexNodeArray();
    int save = PROTECT(items);
    RegexNode *node = newRegexNode_Alternation(items);
    UNPROTECT(save);
    return node;
}

static RegexNode *newRepeatNode(RegexNode *child, Index min, Index max,
                                bool unlimited) {
    int save = PROTECT(child);
    RegexRepeat *repeat = newRegexRepeat(child, min, max, unlimited);
    REPLACE_PROTECT(save, repeat);
    RegexNode *node = newRegexNode_Repeat(repeat);
    UNPROTECT(save);
    return node;
}

static bool appendNode(RegexNodeArray *nodes, RegexNode *child) {
    pushRegexNodeArray(nodes, child);
    return true;
}

static bool appendClassItem(RegexCharClass *charClass, RegexClassItem *item) {
    pushRegexClassItemArray(charClass->items, item);
    return true;
}

static bool appendPosition(RegexPositionArray *set, RegexPosition position) {
    pushRegexPositionArray(set, position);
    return true;
}

static bool appendPositions(RegexPositionArray *dest,
                            const RegexPositionArray *source) {
    Index i;

    for (i = 0; i < source->size; i++) {
        appendPosition(dest, positionAt(source, i));
    }

    return true;
}

static RegexPosition positionAt(const RegexPositionArray *set, Index index) {
    return set->entries[index];
}

static bool parseUnicodeEscape(const Character *pattern, Index *cursor,
                               Character *value, RegexStatus *status,
                               Index *errorOffset) {
    Character code = 0;
    bool hasDigits = false;

    (*cursor)++;

    while (true) {
        Character current = pattern[*cursor];

        switch (current) {
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
            code <<= 4;
            code |= current - L'0';
            hasDigits = true;
            (*cursor)++;
            break;
        case L'a':
        case L'b':
        case L'c':
        case L'd':
        case L'e':
        case L'f':
            code <<= 4;
            code |= 10 + current - L'a';
            hasDigits = true;
            (*cursor)++;
            break;
        case L'A':
        case L'B':
        case L'C':
        case L'D':
        case L'E':
        case L'F':
            code <<= 4;
            code |= 10 + current - L'A';
            hasDigits = true;
            (*cursor)++;
            break;
        case L';':
            if (!hasDigits) {
                compileError(status, errorOffset,
                             REGEX_STATUS_INVALID_UNICODE_ESCAPE, *cursor);
                return false;
            }
            *value = code;
            return true;
        case L'\0':
            compileError(status, errorOffset,
                         REGEX_STATUS_INVALID_UNICODE_ESCAPE, *cursor);
            return false;
        default:
            compileError(status, errorOffset,
                         REGEX_STATUS_INVALID_UNICODE_ESCAPE, *cursor);
            return false;
        }
    }
}

static bool parseEscapedAtom(const Character *pattern, Index *cursor,
                             RegexAtom **atom, RegexStatus *status,
                             Index *errorOffset) {
    Character escaped;
    Character value;

    (*cursor)++;
    escaped = pattern[*cursor];
    if (escaped == L'\0') {
        compileError(status, errorOffset, REGEX_STATUS_TRAILING_ESCAPE,
                     *cursor - 1);
        return false;
    }

    switch (escaped) {
    case L'd':
        *atom = newRegexAtom_Meta(REGEXMETATYPE_TYPE_DIGIT);
        return true;
    case L'D':
        *atom = newRegexAtom_Meta(REGEXMETATYPE_TYPE_NOT_DIGIT);
        return true;
    case L's':
        *atom = newRegexAtom_Meta(REGEXMETATYPE_TYPE_WHITESPACE);
        return true;
    case L'S':
        *atom = newRegexAtom_Meta(REGEXMETATYPE_TYPE_NOT_WHITESPACE);
        return true;
    case L'w':
        *atom = newRegexAtom_Meta(REGEXMETATYPE_TYPE_WORD);
        return true;
    case L'W':
        *atom = newRegexAtom_Meta(REGEXMETATYPE_TYPE_NOT_WORD);
        return true;
    case L'n':
        *atom = newRegexAtom_Literal(L'\n');
        return true;
    case L'r':
        *atom = newRegexAtom_Literal(L'\r');
        return true;
    case L't':
        *atom = newRegexAtom_Literal(L'\t');
        return true;
    case L'u':
    case L'U':
        if (!parseUnicodeEscape(pattern, cursor, &value, status, errorOffset)) {
            return false;
        }
        *atom = newRegexAtom_Literal(value);
        return true;
    default:
        *atom = newRegexAtom_Literal(escaped);
        return true;
    }
}

static bool parseEscapedClassItem(const Character *pattern, Index *cursor,
                                  RegexClassItem **item, RegexStatus *status,
                                  Index *errorOffset) {
    RegexAtom *atom;
    int save;

    if (!parseEscapedAtom(pattern, cursor, &atom, status, errorOffset)) {
        return false;
    }

    save = PROTECT(atom);

    if (atom->type == REGEXATOM_TYPE_LITERAL) {
        *item = newRegexClassItem_Literal(getRegexAtom_Literal(atom));
    } else {
        *item = newRegexClassItem_Meta(getRegexAtom_Meta(atom));
    }

    UNPROTECT(save);
    return true;
}

static bool parseCategoryName(const Character *name, Index length,
                              unsigned char *code, bool *exact) {
    if (length == 1) {
        *exact = false;
        switch (name[0]) {
        case L'L':
            *code = GC_L;
            return true;
        case L'M':
            *code = GC_M;
            return true;
        case L'N':
            *code = GC_N;
            return true;
        case L'P':
            *code = GC_P;
            return true;
        case L'S':
            *code = GC_S;
            return true;
        case L'Z':
            *code = GC_Z;
            return true;
        case L'C':
            *code = GC_C;
            return true;
        default:
            return false;
        }
    }

    if (length == 2) {
        *exact = true;
        switch (name[0]) {
        case L'L':
            switch (name[1]) {
            case L'l':
                *code = GC_Ll;
                return true;
            case L'm':
                *code = GC_Lm;
                return true;
            case L'o':
                *code = GC_Lo;
                return true;
            case L't':
                *code = GC_Lt;
                return true;
            case L'u':
                *code = GC_Lu;
                return true;
            default:
                return false;
            }
        case L'M':
            switch (name[1]) {
            case L'c':
                *code = GC_Mc;
                return true;
            case L'e':
                *code = GC_Me;
                return true;
            case L'n':
                *code = GC_Mn;
                return true;
            default:
                return false;
            }
        case L'N':
            switch (name[1]) {
            case L'd':
                *code = GC_Nd;
                return true;
            case L'l':
                *code = GC_Nl;
                return true;
            case L'o':
                *code = GC_No;
                return true;
            default:
                return false;
            }
        case L'P':
            switch (name[1]) {
            case L'c':
                *code = GC_Pc;
                return true;
            case L'd':
                *code = GC_Pd;
                return true;
            case L'e':
                *code = GC_Pe;
                return true;
            case L'f':
                *code = GC_Pf;
                return true;
            case L'i':
                *code = GC_Pi;
                return true;
            case L'o':
                *code = GC_Po;
                return true;
            case L's':
                *code = GC_Ps;
                return true;
            default:
                return false;
            }
        case L'S':
            switch (name[1]) {
            case L'c':
                *code = GC_Sc;
                return true;
            case L'k':
                *code = GC_Sk;
                return true;
            case L'm':
                *code = GC_Sm;
                return true;
            case L'o':
                *code = GC_So;
                return true;
            default:
                return false;
            }
        case L'Z':
            switch (name[1]) {
            case L'l':
                *code = GC_Zl;
                return true;
            case L'p':
                *code = GC_Zp;
                return true;
            case L's':
                *code = GC_Zs;
                return true;
            default:
                return false;
            }
        case L'C':
            switch (name[1]) {
            case L'c':
                *code = GC_Cc;
                return true;
            case L'f':
                *code = GC_Cf;
                return true;
            case L'n':
                *code = GC_Cn;
                return true;
            case L'o':
                *code = GC_Co;
                return true;
            case L's':
                *code = GC_Cs;
                return true;
            default:
                return false;
            }
        default:
            return false;
        }
    }

    return false;
}

static bool parseNamedCategory(const Character *pattern, Index *cursor,
                               RegexCategory **category, RegexStatus *status,
                               Index *errorOffset) {
    Index nameStart;
    Index nameEnd;
    unsigned char code;
    bool exact;

    if (pattern[*cursor] != L'[' || pattern[*cursor + 1] != L'[') {
        return false;
    }

    nameStart = *cursor + 2;
    nameEnd = nameStart;
    while (pattern[nameEnd] != L'\0') {
        if (pattern[nameEnd] == L']' && pattern[nameEnd + 1] == L']') {
            break;
        }
        nameEnd++;
    }

    if (pattern[nameEnd] == L'\0') {
        compileError(status, errorOffset, REGEX_STATUS_UNTERMINATED_NAMED_CLASS,
                     *cursor);
        return false;
    }

    if (!parseCategoryName(&pattern[nameStart], nameEnd - nameStart, &code,
                           &exact)) {
        compileError(status, errorOffset, REGEX_STATUS_UNKNOWN_CATEGORY,
                     nameStart);
        return false;
    }

    *category = newRegexCategory(code, exact);
    *cursor = nameEnd + 2;
    return true;
}

static RegexCharClass *parseCharClass(const Character *pattern, Index *cursor,
                                      RegexStatus *status, Index *errorOffset) {
    int save = STARTPROTECT();
    RegexClassItemArray *items = newRegexClassItemArray();
    int charClassSave = PROTECT(items);
    RegexCharClass *charClass = newRegexCharClass(false, items);
    Index i = *cursor + 1;

    REPLACE_PROTECT(charClassSave, charClass);

    if (pattern[i] == L'^') {
        charClass->inverted = true;
        i++;
    }

    while (pattern[i] != L']') {
        RegexClassItem *item;
        RegexCategory *category;
        RegexRange *range;

        if (pattern[i] == L'\0') {
            compileError(status, errorOffset, REGEX_STATUS_UNTERMINATED_CLASS,
                         *cursor);
            UNPROTECT(save);
            return NULL;
        }

        if (pattern[i] == L'[' && pattern[i + 1] == L'[') {
            int itemSave;

            if (!parseNamedCategory(pattern, &i, &category, status,
                                    errorOffset)) {
                UNPROTECT(save);
                return NULL;
            }
            itemSave = PROTECT(category);
            item = newRegexClassItem_Category(category);
            REPLACE_PROTECT(itemSave, item);
            appendClassItem(charClass, item);
            UNPROTECT(itemSave);
        } else if (pattern[i] == L'\\') {
            int itemSave;

            if (!parseEscapedClassItem(pattern, &i, &item, status,
                                       errorOffset)) {
                UNPROTECT(save);
                return NULL;
            }
            itemSave = PROTECT(item);
            appendClassItem(charClass, item);
            UNPROTECT(itemSave);
            i++;
        } else if (pattern[i + 1] == L'-' && pattern[i + 2] != L'\0' &&
                   pattern[i + 2] != L']' && pattern[i + 2] != L'\\' &&
                   !(pattern[i + 2] == L'[' && pattern[i + 3] == L'[')) {
            int itemSave;

            if (pattern[i] > pattern[i + 2]) {
                compileError(status, errorOffset, REGEX_STATUS_INVALID_RANGE,
                             i);
                UNPROTECT(save);
                return NULL;
            }
            range = newRegexRange(pattern[i], pattern[i + 2]);
            itemSave = PROTECT(range);
            item = newRegexClassItem_Range(range);
            REPLACE_PROTECT(itemSave, item);
            appendClassItem(charClass, item);
            UNPROTECT(itemSave);
            i += 3;
        } else {
            item = newRegexClassItem_Literal(pattern[i]);
            int itemSave = PROTECT(item);

            appendClassItem(charClass, item);
            UNPROTECT(itemSave);
            i++;
        }
    }

    if (charClass->items->size == 0) {
        compileError(status, errorOffset, REGEX_STATUS_EMPTY_CLASS, *cursor);
        UNPROTECT(save);
        return NULL;
    }

    *cursor = i + 1;
    UNPROTECT(save);
    return charClass;
}

static RegexNode *parsePrimary(const Character *pattern, Index *cursor,
                               RegexStatus *status, Index *errorOffset) {
    Character current = pattern[*cursor];
    int save = STARTPROTECT();
    RegexAtom *atom;
    RegexNode *node;
    RegexCharClass *charClass;
    RegexCategory *category;

    if (current == L'*' || current == L'+' || current == L'?') {
        compileError(status, errorOffset,
                     REGEX_STATUS_QUANTIFIER_WITHOUT_TARGET, *cursor);
        return NULL;
    }

    switch (current) {
    case L'(':
        (*cursor)++;
        node = parseExpression(pattern, cursor, status, errorOffset);
        if (node == NULL) {
            return NULL;
        }
        if (pattern[*cursor] != L')') {
            compileError(status, errorOffset, REGEX_STATUS_UNTERMINATED_GROUP,
                         *cursor);
            UNPROTECT(save);
            return NULL;
        }
        (*cursor)++;
        UNPROTECT(save);
        return node;
    case L'.':
        (*cursor)++;
        atom = newRegexAtom_Dot();
        node = newAtomNode(atom);
        UNPROTECT(save);
        return node;
    case L'^':
        (*cursor)++;
        UNPROTECT(save);
        return newRegexNode_Begin();
    case L'$':
        (*cursor)++;
        UNPROTECT(save);
        return newRegexNode_End();
    case L'[':
        if (pattern[*cursor + 1] == L'[') {
            int atomSave;

            if (!parseNamedCategory(pattern, cursor, &category, status,
                                    errorOffset)) {
                UNPROTECT(save);
                return NULL;
            }
            atomSave = PROTECT(category);
            atom = newRegexAtom_Category(category);
            REPLACE_PROTECT(atomSave, atom);
            node = newAtomNode(atom);
            UNPROTECT(save);
            return node;
        }
        charClass = parseCharClass(pattern, cursor, status, errorOffset);
        if (charClass == NULL) {
            UNPROTECT(save);
            return NULL;
        }
        {
            int atomSave = PROTECT(charClass);

            atom = newRegexAtom_CharClass(charClass);
            REPLACE_PROTECT(atomSave, atom);
            node = newAtomNode(atom);
            UNPROTECT(save);
            return node;
        }
    case L'\\':
        if (!parseEscapedAtom(pattern, cursor, &atom, status, errorOffset)) {
            UNPROTECT(save);
            return NULL;
        }
        (*cursor)++;
        node = newAtomNode(atom);
        UNPROTECT(save);
        return node;
    default:
        (*cursor)++;
        atom = newRegexAtom_Literal(current);
        node = newAtomNode(atom);
        UNPROTECT(save);
        return node;
    }
}

static RegexNode *parseQuantified(const Character *pattern, Index *cursor,
                                  RegexStatus *status, Index *errorOffset) {
    RegexNode *node = parsePrimary(pattern, cursor, status, errorOffset);
    RegexNode *repeat;
    Character quantifier;

    if (node == NULL) {
        return NULL;
    }

    quantifier = pattern[*cursor];
    if (quantifier != L'*' && quantifier != L'+' && quantifier != L'?') {
        return node;
    }

    if (quantifier == L'*') {
        repeat = newRepeatNode(node, 0, 0, true);
    } else if (quantifier == L'+') {
        repeat = newRepeatNode(node, 1, 0, true);
    } else {
        repeat = newRepeatNode(node, 0, 1, false);
    }

    (*cursor)++;
    if (pattern[*cursor] == L'*' || pattern[*cursor] == L'+' ||
        pattern[*cursor] == L'?') {
        compileError(status, errorOffset, REGEX_STATUS_CONSECUTIVE_QUANTIFIERS,
                     *cursor);
        return NULL;
    }

    return repeat;
}

static RegexNode *parseSequence(const Character *pattern, Index *cursor,
                                RegexStatus *status, Index *errorOffset) {
    int save = STARTPROTECT();
    RegexNode *sequence = newConcatNode();
    PROTECT(sequence);
    RegexNodeArray *items = getRegexNode_Concat(sequence);
    RegexNode *child;

    while (pattern[*cursor] != L'\0' && pattern[*cursor] != L'|' &&
           pattern[*cursor] != L')') {
        child = parseQuantified(pattern, cursor, status, errorOffset);
        if (child == NULL) {
            UNPROTECT(save);
            return NULL;
        }
        {
            int childSave = PROTECT(child);

            appendNode(items, child);
            UNPROTECT(childSave);
        }
    }

    if (items->size == 0) {
        RegexNode *empty = newEmptyNode();

        UNPROTECT(save);
        return empty;
    }

    if (items->size == 1) {
        child = items->entries[0];
        UNPROTECT(save);
        return child;
    }

    UNPROTECT(save);
    return sequence;
}

static RegexNode *parseExpression(const Character *pattern, Index *cursor,
                                  RegexStatus *status, Index *errorOffset) {
    RegexNode *left = parseSequence(pattern, cursor, status, errorOffset);
    int save = STARTPROTECT();
    RegexNode *alternation;
    RegexNodeArray *items;
    RegexNode *right;

    if (left == NULL) {
        return NULL;
    }

    if (pattern[*cursor] != L'|') {
        UNPROTECT(save);
        return left;
    }

    PROTECT(left);
    alternation = newAlternationNode();
    PROTECT(alternation);
    items = getRegexNode_Alternation(alternation);
    appendNode(items, left);

    while (pattern[*cursor] == L'|') {
        (*cursor)++;
        right = parseSequence(pattern, cursor, status, errorOffset);
        if (right == NULL) {
            UNPROTECT(save);
            return NULL;
        }
        {
            int rightSave = PROTECT(right);

            appendNode(items, right);
            UNPROTECT(rightSave);
        }
    }

    UNPROTECT(save);
    return alternation;
}

Regex *regexCompile(const Character *pattern, RegexStatus *status,
                    Index *errorOffset) {
    Regex *regex;
    RegexNode *root;
    Index cursor = 0;
    unsigned char flags = 0;
    int save;

    compileError(status, errorOffset, REGEX_STATUS_OK, 0);
    if (pattern == NULL) {
        return NULL;
    }

    ensureRegexMemoryReady();
    if (!parseLeadingInlineFlags(pattern, &cursor, &flags, status,
                                 errorOffset)) {
        return NULL;
    }

    root = parseExpression(pattern, &cursor, status, errorOffset);
    if (root == NULL) {
        return NULL;
    }

    save = PROTECT(root);

    if (pattern[cursor] == L')') {
        compileError(status, errorOffset, REGEX_STATUS_UNEXPECTED_CLOSE_PAREN,
                     cursor);
        UNPROTECT(save);
        return NULL;
    }

    regex = newRegex(root, flags);
    UNPROTECT(save);
    return regex;
}

void regexFree(Regex *regex) { (void)regex; }

static bool matchCategory(const RegexCategory *category, Character c) {
    int actual = unicode_category(c);

    if (category->exact) {
        return actual == category->code;
    }

    return (actual & GC_MASK) == category->code;
}

static bool matchMeta(RegexMetaType metaType, Character c) {
    switch (metaType) {
    case REGEXMETATYPE_TYPE_DIGIT:
        return unicode_isdigit(c);
    case REGEXMETATYPE_TYPE_NOT_DIGIT:
        return !unicode_isdigit(c);
    case REGEXMETATYPE_TYPE_WORD:
        return unicode_isalnum(c) || c == L'_';
    case REGEXMETATYPE_TYPE_NOT_WORD:
        return !unicode_isalnum(c) && c != L'_';
    case REGEXMETATYPE_TYPE_WHITESPACE:
        return unicode_isspace(c);
    case REGEXMETATYPE_TYPE_NOT_WHITESPACE:
        return !unicode_isspace(c);
    default:
        return false;
    }
}

static bool matchClassItem(const RegexClassItem *item, Character c,
                           unsigned char flags) {
    switch (item->type) {
    case REGEXCLASSITEM_TYPE_LITERAL:
        return matchCharacters(
            getRegexClassItem_Literal((RegexClassItem *)item), c, flags);
    case REGEXCLASSITEM_TYPE_RANGE: {
        RegexRange *range = getRegexClassItem_Range((RegexClassItem *)item);
        return matchRange(range->lower, range->upper, c, flags);
    }
    case REGEXCLASSITEM_TYPE_META:
        return matchMeta(getRegexClassItem_Meta((RegexClassItem *)item), c);
    case REGEXCLASSITEM_TYPE_CATEGORY:
        return matchCategory(getRegexClassItem_Category((RegexClassItem *)item),
                             c);
    default:
        return false;
    }
}

static bool matchCharClass(const RegexCharClass *charClass, Character c,
                           unsigned char flags) {
    bool matched = false;
    Index i;

    for (i = 0; i < charClass->items->size; i++) {
        if (matchClassItem(charClass->items->entries[i], c, flags)) {
            matched = true;
            break;
        }
    }

    return charClass->inverted ? !matched : matched;
}

static bool matchAtom(const RegexAtom *atom, Character c, unsigned char flags) {
    switch (atom->type) {
    case REGEXATOM_TYPE_LITERAL:
        return matchCharacters(getRegexAtom_Literal((RegexAtom *)atom), c,
                               flags);
    case REGEXATOM_TYPE_DOT:
        return c != L'\n' && c != L'\r';
    case REGEXATOM_TYPE_CHARCLASS:
        return matchCharClass(getRegexAtom_CharClass((RegexAtom *)atom), c,
                              flags);
    case REGEXATOM_TYPE_META:
        return matchMeta(getRegexAtom_Meta((RegexAtom *)atom), c);
    case REGEXATOM_TYPE_CATEGORY:
        return matchCategory(getRegexAtom_Category((RegexAtom *)atom), c);
    default:
        return false;
    }
}

static bool matchSequence(const RegexNodeArray *list, RegexSource *source,
                          RegexPosition position, RegexPosition inputStart,
                          unsigned char flags, RegexPositionArray *out) {
    int save = STARTPROTECT();
    RegexPositionArray *current = newRegexPositionArray();
    int currentSave = PROTECT(current);
    RegexPositionArray *next = newRegexPositionArray();
    int nextSave = PROTECT(next);
    Index i;
    Index j;
    bool ok;

    appendPosition(current, position);

    for (i = 0; i < list->size; i++) {
        for (j = 0; j < current->size; j++) {
            RegexPositionArray *childMatches = newRegexPositionArray();
            int childSave = PROTECT(childMatches);

            ok = matchNode(list->entries[i], source, positionAt(current, j),
                           inputStart, flags, childMatches);
            if (!ok) {
                UNPROTECT(save);
                return false;
            }
            ok = appendPositions(next, childMatches);
            UNPROTECT(childSave);
            if (!ok) {
                UNPROTECT(save);
                return false;
            }
        }

        current = next;
        REPLACE_PROTECT(currentSave, current);
        next = newRegexPositionArray();
        REPLACE_PROTECT(nextSave, next);

        if (current->size == 0) {
            break;
        }
    }

    ok = appendPositions(out, current);
    UNPROTECT(save);
    return ok;
}

static bool matchAlternation(const RegexNodeArray *list, RegexSource *source,
                             RegexPosition position, RegexPosition inputStart,
                             unsigned char flags, RegexPositionArray *out) {
    Index i;
    bool ok;

    for (i = 0; i < list->size; i++) {
        RegexPositionArray *branchMatches = newRegexPositionArray();
        int branchSave = PROTECT(branchMatches);

        ok = matchNode(list->entries[i], source, position, inputStart, flags,
                       branchMatches);
        if (!ok) {
            UNPROTECT(branchSave);
            return false;
        }
        ok = appendPositions(out, branchMatches);
        UNPROTECT(branchSave);
        if (!ok) {
            return false;
        }
    }

    return true;
}

static bool matchRepeatGreedy(const RegexRepeat *repeat, RegexSource *source,
                              RegexPosition position, RegexPosition inputStart,
                              unsigned char flags, Index count,
                              RegexPositionArray *out) {
    Index i;
    bool ok;

    if (repeat->unlimited || count < repeat->max) {
        RegexPositionArray *childMatches = newRegexPositionArray();
        int childSave = PROTECT(childMatches);

        ok = matchNode(repeat->child, source, position, inputStart, flags,
                       childMatches);
        if (!ok) {
            UNPROTECT(childSave);
            return false;
        }
        for (i = 0; i < childMatches->size; i++) {
            RegexPosition next = positionAt(childMatches, i);

            if (next == position) {
                continue;
            }
            if (!matchRepeatGreedy(repeat, source, next, inputStart, flags,
                                   count + 1, out)) {
                UNPROTECT(childSave);
                return false;
            }
        }
        UNPROTECT(childSave);
    }

    if (count >= repeat->min) {
        return appendPosition(out, position);
    }

    return true;
}

static bool matchRepeat(const RegexRepeat *repeat, RegexSource *source,
                        RegexPosition position, RegexPosition inputStart,
                        unsigned char flags, RegexPositionArray *out) {
    return matchRepeatGreedy(repeat, source, position, inputStart, flags, 0,
                             out);
}

static bool matchNode(const RegexNode *node, RegexSource *source,
                      RegexPosition position, RegexPosition inputStart,
                      unsigned char flags, RegexPositionArray *out) {
    switch (node->type) {
    case REGEXNODE_TYPE_EMPTY:
        return appendPosition(out, position);
    case REGEXNODE_TYPE_ATOM:
        if (regexSourceGet(source, position) != L'\0' &&
            matchAtom(getRegexNode_Atom((RegexNode *)node),
                      regexSourceGet(source, position), flags)) {
            return appendPosition(out, position + 1);
        }
        return true;
    case REGEXNODE_TYPE_BEGIN:
        if (position == inputStart) {
            return appendPosition(out, position);
        }
        return true;
    case REGEXNODE_TYPE_END:
        if (regexSourceGet(source, position) == L'\0') {
            return appendPosition(out, position);
        }
        return true;
    case REGEXNODE_TYPE_CONCAT:
        return matchSequence(getRegexNode_Concat((RegexNode *)node), source,
                             position, inputStart, flags, out);
    case REGEXNODE_TYPE_ALTERNATION:
        return matchAlternation(getRegexNode_Alternation((RegexNode *)node),
                                source, position, inputStart, flags, out);
    case REGEXNODE_TYPE_REPEAT:
        return matchRepeat(getRegexNode_Repeat((RegexNode *)node), source,
                           position, inputStart, flags, out);
    default:
        return false;
    }
}

int regexMatchSourcep(const Regex *pattern, RegexSource *source,
                      Index *matchLength) {
    RegexPosition index = 0;

    if (matchLength != NULL) {
        *matchLength = 0;
    }

    if (pattern == NULL || source == NULL) {
        return -1;
    }

    while (true) {
        RegexPositionArray *matches = newRegexPositionArray();
        int matchesSave = PROTECT(matches);

        if (!matchNode(pattern->root, source, index, 0, pattern->flags,
                       matches)) {
            regexSourceSetPosition(source, 0);
            return -1;
        }

        if (matches->size > 0) {
            Index matchedLength = positionAt(matches, 0) - index;

            if (matchLength != NULL) {
                *matchLength = matchedLength;
            }
            regexSourceSetPosition(source, index + matchedLength);
            return (int)index;
        }

        UNPROTECT(matchesSave);
        if (regexSourceGet(source, index) == L'\0') {
            regexSourceSetPosition(source, 0);
            return -1;
        }
        index++;
    }
}

int regexMatchCharArrayp(const Regex *pattern, CharacterArray *text,
                         Index *matchLength) {
    RegexSource *source;
    int save;

    if (matchLength != NULL) {
        *matchLength = 0;
    }

    if (pattern == NULL || text == NULL) {
        return -1;
    }

    ensureRegexMemoryReady();
    save = STARTPROTECT();
    PROTECT((Regex *)pattern);
    PROTECT(text);
    source = regexSourceFromCharArray(text);
    PROTECT(source);

    int result = regexMatchSourcep(pattern, source, matchLength);
    UNPROTECT(save);
    return result;
}

int regexMatchp(const Regex *pattern, const Character *text,
                Index *matchLength) {
    CharacterArray *chars;
    int save;

    if (matchLength != NULL) {
        *matchLength = 0;
    }

    if (pattern == NULL || text == NULL) {
        return -1;
    }

    ensureRegexMemoryReady();
    save = STARTPROTECT();
    PROTECT((Regex *)pattern);
    chars = copyNullTerminatedText(text);
    PROTECT(chars);
    int result = regexMatchCharArrayp(pattern, chars, matchLength);
    UNPROTECT(save);
    return result;
}

int regexMatch(const Character *pattern, const Character *text,
               Index *matchLength, RegexStatus *status, Index *errorOffset) {
    Regex *compiled = regexCompile(pattern, status, errorOffset);
    int result;

    if (compiled == NULL) {
        if (matchLength != NULL) {
            *matchLength = 0;
        }
        return -1;
    }

    result = regexMatchp(compiled, text, matchLength);
    regexFree(compiled);
    return result;
}
