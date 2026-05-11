#include "regex_helper.h"

#include "memory.h"
#include "unicode.h"

typedef const Character *RegexPosition;

static void compileError(RegexStatus *status, Index *errorOffset,
                         RegexStatus code, Index offset);
static void ensureRegexMemoryReady(void);
static void restoreGC(bool previous);
static RegexNode *newEmptyNode(void);
static RegexNode *newAtomNode(RegexAtom *atom);
static RegexNode *newConcatNode(void);
static RegexNode *newAlternationNode(void);
static RegexNode *newRepeatNode(RegexNode *child, Index min, Index max,
                                bool unlimited);
static void freeRegexClassItemDeep(RegexClassItem *item);
static void freeRegexCharClassDeep(RegexCharClass *charClass);
static void freeRegexAtomDeep(RegexAtom *atom);
static void freeRegexRepeatDeep(RegexRepeat *repeat);
static void freeRegexNodeDeep(RegexNode *node);
static bool appendNode(RegexNodeArray *nodes, RegexNode *child);
static bool appendClassItem(RegexCharClass *charClass, RegexClassItem *item);
static bool appendPosition(RegexPositionArray *set, RegexPosition position);
static bool appendPositions(RegexPositionArray *dest,
                            const RegexPositionArray *source);
static RegexPosition positionAt(const RegexPositionArray *set, Index index);
static bool parseEscapedAtom(const Character *pattern, Index *cursor,
                             RegexAtom **atom, RegexStatus *status,
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
static bool matchClassItem(const RegexClassItem *item, Character c);
static bool matchCharClass(const RegexCharClass *charClass, Character c);
static bool matchAtom(const RegexAtom *atom, Character c);
static bool matchNode(const RegexNode *node, RegexPosition text,
                      RegexPosition inputStart, RegexPositionArray *out);
static bool matchSequence(const RegexNodeArray *list, RegexPosition text,
                          RegexPosition inputStart, RegexPositionArray *out);
static bool matchAlternation(const RegexNodeArray *list, RegexPosition text,
                             RegexPosition inputStart, RegexPositionArray *out);
static bool matchRepeat(const RegexRepeat *repeat, RegexPosition text,
                        RegexPosition inputStart, RegexPositionArray *out);
static bool matchRepeatGreedy(const RegexRepeat *repeat, RegexPosition text,
                              RegexPosition inputStart, Index count,
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

static void restoreGC(bool previous) {
    if (previous) {
        enableGC();
    } else {
        disableGC();
    }
}

static RegexNode *newEmptyNode(void) { return newRegexNode_Empty(); }

static RegexNode *newAtomNode(RegexAtom *atom) {
    return newRegexNode_Atom(atom);
}

static RegexNode *newConcatNode(void) {
    return newRegexNode_Concat(newRegexNodeArray());
}

static RegexNode *newAlternationNode(void) {
    return newRegexNode_Alternation(newRegexNodeArray());
}

static RegexNode *newRepeatNode(RegexNode *child, Index min, Index max,
                                bool unlimited) {
    return newRegexNode_Repeat(newRegexRepeat(child, min, max, unlimited));
}

static void freeRegexClassItemDeep(RegexClassItem *item) {
    if (item == NULL) {
        return;
    }

    switch (item->type) {
    case REGEXCLASSITEM_TYPE_RANGE:
        freeRegexRange(getRegexClassItem_Range(item));
        break;
    case REGEXCLASSITEM_TYPE_META:
        break;
    case REGEXCLASSITEM_TYPE_CATEGORY:
        freeRegexCategory(getRegexClassItem_Category(item));
        break;
    default:
        break;
    }

    freeRegexClassItem(item);
}

static void freeRegexCharClassDeep(RegexCharClass *charClass) {
    Index i;

    if (charClass == NULL) {
        return;
    }

    for (i = 0; i < charClass->items->size; i++) {
        freeRegexClassItemDeep(charClass->items->entries[i]);
    }
    freeRegexClassItemArray(charClass->items);
    freeRegexCharClass(charClass);
}

static void freeRegexAtomDeep(RegexAtom *atom) {
    if (atom == NULL) {
        return;
    }

    switch (atom->type) {
    case REGEXATOM_TYPE_CHARCLASS:
        freeRegexCharClassDeep(getRegexAtom_CharClass(atom));
        break;
    case REGEXATOM_TYPE_META:
        break;
    case REGEXATOM_TYPE_CATEGORY:
        freeRegexCategory(getRegexAtom_Category(atom));
        break;
    default:
        break;
    }

    freeRegexAtom(atom);
}

static void freeRegexRepeatDeep(RegexRepeat *repeat) {
    if (repeat == NULL) {
        return;
    }

    freeRegexNodeDeep(repeat->child);
    freeRegexRepeat(repeat);
}

static void freeRegexNodeDeep(RegexNode *node) {
    if (node == NULL) {
        return;
    }

    switch (node->type) {
    case REGEXNODE_TYPE_ATOM:
        freeRegexAtomDeep(getRegexNode_Atom(node));
        break;
    case REGEXNODE_TYPE_CONCAT: {
        RegexNodeArray *children = getRegexNode_Concat(node);
        Index i;

        for (i = 0; i < children->size; i++) {
            freeRegexNodeDeep(children->entries[i]);
        }
        freeRegexNodeArray(children);
        break;
    }
    case REGEXNODE_TYPE_ALTERNATION: {
        RegexNodeArray *children = getRegexNode_Alternation(node);
        Index i;

        for (i = 0; i < children->size; i++) {
            freeRegexNodeDeep(children->entries[i]);
        }
        freeRegexNodeArray(children);
        break;
    }
    case REGEXNODE_TYPE_REPEAT:
        freeRegexRepeatDeep(getRegexNode_Repeat(node));
        break;
    default:
        break;
    }

    freeRegexNode(node);
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
    pushRegexPositionArray(set, (void *)position);
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
    return (RegexPosition)set->entries[index];
}

static bool parseEscapedAtom(const Character *pattern, Index *cursor,
                             RegexAtom **atom, RegexStatus *status,
                             Index *errorOffset) {
    Character escaped;

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
    default:
        *atom = newRegexAtom_Literal(escaped);
        return true;
    }
}

static bool parseEscapedClassItem(const Character *pattern, Index *cursor,
                                  RegexClassItem **item, RegexStatus *status,
                                  Index *errorOffset) {
    RegexAtom *atom;

    if (!parseEscapedAtom(pattern, cursor, &atom, status, errorOffset)) {
        return false;
    }

    if (atom->type == REGEXATOM_TYPE_LITERAL) {
        *item = newRegexClassItem_Literal(getRegexAtom_Literal(atom));
    } else {
        *item = newRegexClassItem_Meta(getRegexAtom_Meta(atom));
    }

    freeRegexAtom(atom);
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
    RegexCharClass *charClass =
        newRegexCharClass(false, newRegexClassItemArray());
    Index i = *cursor + 1;

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
            freeRegexCharClassDeep(charClass);
            return NULL;
        }

        if (pattern[i] == L'[' && pattern[i + 1] == L'[') {
            if (!parseNamedCategory(pattern, &i, &category, status,
                                    errorOffset)) {
                freeRegexCharClassDeep(charClass);
                return NULL;
            }
            item = newRegexClassItem_Category(category);
        } else if (pattern[i] == L'\\') {
            if (!parseEscapedClassItem(pattern, &i, &item, status,
                                       errorOffset)) {
                freeRegexCharClassDeep(charClass);
                return NULL;
            }
            i++;
        } else if (pattern[i + 1] == L'-' && pattern[i + 2] != L'\0' &&
                   pattern[i + 2] != L']' && pattern[i + 2] != L'\\' &&
                   !(pattern[i + 2] == L'[' && pattern[i + 3] == L'[')) {
            if (pattern[i] > pattern[i + 2]) {
                compileError(status, errorOffset, REGEX_STATUS_INVALID_RANGE,
                             i);
                freeRegexCharClassDeep(charClass);
                return NULL;
            }
            range = newRegexRange(pattern[i], pattern[i + 2]);
            item = newRegexClassItem_Range(range);
            i += 3;
        } else {
            item = newRegexClassItem_Literal(pattern[i]);
            i++;
        }

        appendClassItem(charClass, item);
    }

    if (charClass->items->size == 0) {
        compileError(status, errorOffset, REGEX_STATUS_EMPTY_CLASS, *cursor);
        freeRegexCharClassDeep(charClass);
        return NULL;
    }

    *cursor = i + 1;
    return charClass;
}

static RegexNode *parsePrimary(const Character *pattern, Index *cursor,
                               RegexStatus *status, Index *errorOffset) {
    Character current = pattern[*cursor];
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
            freeRegexNodeDeep(node);
            return NULL;
        }
        (*cursor)++;
        return node;
    case L'.':
        (*cursor)++;
        return newAtomNode(newRegexAtom_Dot());
    case L'^':
        (*cursor)++;
        return newRegexNode_Begin();
    case L'$':
        (*cursor)++;
        return newRegexNode_End();
    case L'[':
        if (pattern[*cursor + 1] == L'[') {
            if (!parseNamedCategory(pattern, cursor, &category, status,
                                    errorOffset)) {
                return NULL;
            }
            return newAtomNode(newRegexAtom_Category(category));
        }
        charClass = parseCharClass(pattern, cursor, status, errorOffset);
        if (charClass == NULL) {
            return NULL;
        }
        return newAtomNode(newRegexAtom_CharClass(charClass));
    case L'\\':
        if (!parseEscapedAtom(pattern, cursor, &atom, status, errorOffset)) {
            return NULL;
        }
        (*cursor)++;
        return newAtomNode(atom);
    default:
        (*cursor)++;
        return newAtomNode(newRegexAtom_Literal(current));
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
        freeRegexNodeDeep(repeat);
        return NULL;
    }

    return repeat;
}

static RegexNode *parseSequence(const Character *pattern, Index *cursor,
                                RegexStatus *status, Index *errorOffset) {
    RegexNode *sequence = newConcatNode();
    RegexNodeArray *items = getRegexNode_Concat(sequence);
    RegexNode *child;

    while (pattern[*cursor] != L'\0' && pattern[*cursor] != L'|' &&
           pattern[*cursor] != L')') {
        child = parseQuantified(pattern, cursor, status, errorOffset);
        if (child == NULL) {
            freeRegexNodeDeep(sequence);
            return NULL;
        }
        appendNode(items, child);
    }

    if (items->size == 0) {
        freeRegexNodeArray(items);
        freeRegexNode(sequence);
        return newEmptyNode();
    }

    if (items->size == 1) {
        child = items->entries[0];
        freeRegexNodeArray(items);
        freeRegexNode(sequence);
        return child;
    }

    return sequence;
}

static RegexNode *parseExpression(const Character *pattern, Index *cursor,
                                  RegexStatus *status, Index *errorOffset) {
    RegexNode *left = parseSequence(pattern, cursor, status, errorOffset);
    RegexNode *alternation;
    RegexNodeArray *items;
    RegexNode *right;

    if (left == NULL) {
        return NULL;
    }

    if (pattern[*cursor] != L'|') {
        return left;
    }

    alternation = newAlternationNode();
    items = getRegexNode_Alternation(alternation);
    appendNode(items, left);

    while (pattern[*cursor] == L'|') {
        (*cursor)++;
        right = parseSequence(pattern, cursor, status, errorOffset);
        if (right == NULL) {
            freeRegexNodeDeep(alternation);
            return NULL;
        }
        appendNode(items, right);
    }

    return alternation;
}

Regex *regexCompile(const Character *pattern, RegexStatus *status,
                    Index *errorOffset) {
    Regex *regex;
    RegexNode *root;
    Index cursor = 0;
    bool previousGc;

    compileError(status, errorOffset, REGEX_STATUS_OK, 0);
    if (pattern == NULL) {
        return NULL;
    }

    ensureRegexMemoryReady();
    previousGc = disableGC();
    root = parseExpression(pattern, &cursor, status, errorOffset);
    if (root == NULL) {
        restoreGC(previousGc);
        return NULL;
    }

    if (pattern[cursor] == L')') {
        compileError(status, errorOffset, REGEX_STATUS_UNEXPECTED_CLOSE_PAREN,
                     cursor);
        freeRegexNodeDeep(root);
        restoreGC(previousGc);
        return NULL;
    }

    regex = newRegex(root);
    restoreGC(previousGc);
    return regex;
}

void regexFree(Regex *regex) {
    if (regex == NULL) {
        return;
    }

    freeRegexNodeDeep(regex->root);
    freeRegex(regex);
}

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
        return unicode_isalnum(c);
    case REGEXMETATYPE_TYPE_NOT_WORD:
        return !unicode_isalnum(c);
    case REGEXMETATYPE_TYPE_WHITESPACE:
        return unicode_isspace(c);
    case REGEXMETATYPE_TYPE_NOT_WHITESPACE:
        return !unicode_isspace(c);
    default:
        return false;
    }
}

static bool matchClassItem(const RegexClassItem *item, Character c) {
    switch (item->type) {
    case REGEXCLASSITEM_TYPE_LITERAL:
        return c == getRegexClassItem_Literal((RegexClassItem *)item);
    case REGEXCLASSITEM_TYPE_RANGE: {
        RegexRange *range = getRegexClassItem_Range((RegexClassItem *)item);
        return c >= range->lower && c <= range->upper;
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

static bool matchCharClass(const RegexCharClass *charClass, Character c) {
    bool matched = false;
    Index i;

    for (i = 0; i < charClass->items->size; i++) {
        if (matchClassItem(charClass->items->entries[i], c)) {
            matched = true;
            break;
        }
    }

    return charClass->inverted ? !matched : matched;
}

static bool matchAtom(const RegexAtom *atom, Character c) {
    switch (atom->type) {
    case REGEXATOM_TYPE_LITERAL:
        return getRegexAtom_Literal((RegexAtom *)atom) == c;
    case REGEXATOM_TYPE_DOT:
        return c != L'\n' && c != L'\r';
    case REGEXATOM_TYPE_CHARCLASS:
        return matchCharClass(getRegexAtom_CharClass((RegexAtom *)atom), c);
    case REGEXATOM_TYPE_META:
        return matchMeta(getRegexAtom_Meta((RegexAtom *)atom), c);
    case REGEXATOM_TYPE_CATEGORY:
        return matchCategory(getRegexAtom_Category((RegexAtom *)atom), c);
    default:
        return false;
    }
}

static bool matchSequence(const RegexNodeArray *list, RegexPosition text,
                          RegexPosition inputStart, RegexPositionArray *out) {
    RegexPositionArray *current = newRegexPositionArray();
    RegexPositionArray *next = newRegexPositionArray();
    Index i;
    Index j;
    bool ok;

    appendPosition(current, text);

    for (i = 0; i < list->size; i++) {
        for (j = 0; j < current->size; j++) {
            RegexPositionArray *childMatches = newRegexPositionArray();

            ok = matchNode(list->entries[i], positionAt(current, j), inputStart,
                           childMatches);
            if (!ok) {
                freeRegexPositionArray(current);
                freeRegexPositionArray(next);
                freeRegexPositionArray(childMatches);
                return false;
            }
            ok = appendPositions(next, childMatches);
            freeRegexPositionArray(childMatches);
            if (!ok) {
                freeRegexPositionArray(current);
                freeRegexPositionArray(next);
                return false;
            }
        }

        freeRegexPositionArray(current);
        current = next;
        next = newRegexPositionArray();

        if (current->size == 0) {
            break;
        }
    }

    ok = appendPositions(out, current);
    freeRegexPositionArray(current);
    freeRegexPositionArray(next);
    return ok;
}

static bool matchAlternation(const RegexNodeArray *list, RegexPosition text,
                             RegexPosition inputStart,
                             RegexPositionArray *out) {
    Index i;
    bool ok;

    for (i = 0; i < list->size; i++) {
        RegexPositionArray *branchMatches = newRegexPositionArray();

        ok = matchNode(list->entries[i], text, inputStart, branchMatches);
        if (!ok) {
            freeRegexPositionArray(branchMatches);
            return false;
        }
        ok = appendPositions(out, branchMatches);
        freeRegexPositionArray(branchMatches);
        if (!ok) {
            return false;
        }
    }

    return true;
}

static bool matchRepeatGreedy(const RegexRepeat *repeat, RegexPosition text,
                              RegexPosition inputStart, Index count,
                              RegexPositionArray *out) {
    Index i;
    bool ok;

    if (repeat->unlimited || count < repeat->max) {
        RegexPositionArray *childMatches = newRegexPositionArray();

        ok = matchNode(repeat->child, text, inputStart, childMatches);
        if (!ok) {
            freeRegexPositionArray(childMatches);
            return false;
        }
        for (i = 0; i < childMatches->size; i++) {
            RegexPosition next = positionAt(childMatches, i);

            if (next == text) {
                continue;
            }
            if (!matchRepeatGreedy(repeat, next, inputStart, count + 1, out)) {
                freeRegexPositionArray(childMatches);
                return false;
            }
        }
        freeRegexPositionArray(childMatches);
    }

    if (count >= repeat->min) {
        return appendPosition(out, text);
    }

    return true;
}

static bool matchRepeat(const RegexRepeat *repeat, RegexPosition text,
                        RegexPosition inputStart, RegexPositionArray *out) {
    return matchRepeatGreedy(repeat, text, inputStart, 0, out);
}

static bool matchNode(const RegexNode *node, RegexPosition text,
                      RegexPosition inputStart, RegexPositionArray *out) {
    switch (node->type) {
    case REGEXNODE_TYPE_EMPTY:
        return appendPosition(out, text);
    case REGEXNODE_TYPE_ATOM:
        if (*text != L'\0' &&
            matchAtom(getRegexNode_Atom((RegexNode *)node), *text)) {
            return appendPosition(out, text + 1);
        }
        return true;
    case REGEXNODE_TYPE_BEGIN:
        if (text == inputStart) {
            return appendPosition(out, text);
        }
        return true;
    case REGEXNODE_TYPE_END:
        if (*text == L'\0') {
            return appendPosition(out, text);
        }
        return true;
    case REGEXNODE_TYPE_CONCAT:
        return matchSequence(getRegexNode_Concat((RegexNode *)node), text,
                             inputStart, out);
    case REGEXNODE_TYPE_ALTERNATION:
        return matchAlternation(getRegexNode_Alternation((RegexNode *)node),
                                text, inputStart, out);
    case REGEXNODE_TYPE_REPEAT:
        return matchRepeat(getRegexNode_Repeat((RegexNode *)node), text,
                           inputStart, out);
    default:
        return false;
    }
}

int regexMatchp(const Regex *pattern, const Character *text,
                Index *matchLength) {
    int index = 0;
    bool previousGc;

    if (matchLength != NULL) {
        *matchLength = 0;
    }

    if (pattern == NULL || text == NULL) {
        return -1;
    }

    ensureRegexMemoryReady();
    previousGc = disableGC();

    while (true) {
        RegexPositionArray *matches = newRegexPositionArray();

        if (!matchNode(pattern->root, text + index, text, matches)) {
            freeRegexPositionArray(matches);
            restoreGC(previousGc);
            return -1;
        }

        if (matches->size > 0) {
            if (matchLength != NULL) {
                *matchLength = (Index)(positionAt(matches, 0) - (text + index));
            }
            freeRegexPositionArray(matches);
            restoreGC(previousGc);
            return index;
        }

        freeRegexPositionArray(matches);
        if (text[index] == L'\0') {
            restoreGC(previousGc);
            return -1;
        }
        index++;
    }
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
