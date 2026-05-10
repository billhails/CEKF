#include "regex.h"

#include "memory.h"
#include "unicode.h"

typedef enum {
    REGEX_ATOM_LITERAL = 0,
    REGEX_ATOM_DOT,
    REGEX_ATOM_CLASS,
    REGEX_ATOM_DIGIT,
    REGEX_ATOM_NOT_DIGIT,
    REGEX_ATOM_WORD,
    REGEX_ATOM_NOT_WORD,
    REGEX_ATOM_WHITESPACE,
    REGEX_ATOM_NOT_WHITESPACE,
    REGEX_ATOM_CATEGORY,
} RegexAtomType;

typedef enum {
    REGEX_CLASS_ITEM_LITERAL = 0,
    REGEX_CLASS_ITEM_RANGE,
    REGEX_CLASS_ITEM_META,
    REGEX_CLASS_ITEM_CATEGORY,
} RegexClassItemType;

typedef enum {
    REGEX_NODE_EMPTY = 0,
    REGEX_NODE_ATOM,
    REGEX_NODE_BEGIN,
    REGEX_NODE_END,
    REGEX_NODE_CONCAT,
    REGEX_NODE_ALTERNATION,
    REGEX_NODE_REPEAT,
} RegexNodeType;

typedef struct RegexNode RegexNode;
typedef RegexNode *RegexNodePtr;
typedef const Character *RegexPosition;

typedef struct {
    unsigned char code;
    bool exact;
} RegexCategory;

typedef struct {
    Character lower;
    Character upper;
} RegexRange;

typedef struct {
    unsigned char type;
    union {
        Character literal;
        unsigned char metaType;
        RegexRange range;
        RegexCategory category;
    } data;
} RegexClassItem;

typedef struct {
    bool inverted;
    Index size;
    Index capacity;
    RegexClassItem *items;
} RegexCharClass;

typedef struct {
    unsigned char type;
    union {
        Character literal;
        RegexCharClass *charClass;
        RegexCategory category;
    } data;
} RegexAtom;

typedef struct {
    Index size;
    Index capacity;
    RegexNodePtr *items;
} RegexNodeList;

typedef struct {
    RegexNode *child;
    Index min;
    Index max;
    bool unlimited;
} RegexRepeat;

typedef struct {
    const Character *pattern;
    Index cursor;
    RegexStatus *status;
    Index *errorOffset;
} RegexParser;

typedef struct {
    Index size;
    Index capacity;
    RegexPosition *items;
} RegexPositionSet;

struct RegexNode {
    unsigned char type;
    union {
        RegexAtom atom;
        RegexNodeList list;
        RegexRepeat repeat;
    } data;
};

struct Regex {
    RegexNode *root;
};

static void compileError(RegexStatus *status, Index *errorOffset,
                         RegexStatus code, Index offset);
static RegexNode *newNode(unsigned char type);
static RegexNode *newEmptyNode(void);
static RegexNode *newAtomNode(RegexAtom atom);
static RegexNode *newListNode(unsigned char type);
static RegexNode *newRepeatNode(RegexNode *child, Index min, Index max,
                                bool unlimited);
static bool appendNode(RegexNode *node, RegexNode *child);
static void freeCharClass(RegexCharClass *charClass);
static void freeNode(RegexNode *node);
static bool appendClassItem(RegexCharClass *charClass, RegexClassItem item);
static bool appendPosition(RegexPositionSet *set, RegexPosition position);
static bool appendPositions(RegexPositionSet *dest,
                            const RegexPositionSet *source);
static void freePositionSet(RegexPositionSet *set);
static bool parseEscapedAtom(const Character *pattern, Index *cursor,
                             RegexAtom *atom, RegexStatus *status,
                             Index *errorOffset);
static bool parseEscapedClassItem(const Character *pattern, Index *cursor,
                                  RegexClassItem *item, RegexStatus *status,
                                  Index *errorOffset);
static bool parseCategoryName(const Character *name, Index length,
                              RegexCategory *category);
static bool parseNamedCategory(const Character *pattern, Index *cursor,
                               RegexCategory *category, RegexStatus *status,
                               Index *errorOffset);
static RegexCharClass *parseCharClass(const Character *pattern, Index *cursor,
                                      RegexStatus *status, Index *errorOffset);
static RegexNode *parseExpression(RegexParser *parser);
static RegexNode *parseSequence(RegexParser *parser);
static RegexNode *parseQuantified(RegexParser *parser);
static RegexNode *parsePrimary(RegexParser *parser);
static bool matchCategory(RegexCategory category, Character c);
static bool matchMeta(unsigned char metaType, Character c);
static bool matchClassItem(const RegexClassItem *item, Character c);
static bool matchCharClass(const RegexCharClass *charClass, Character c);
static bool matchAtom(const RegexAtom *atom, Character c);
static bool matchNode(const RegexNode *node, RegexPosition text,
                      RegexPosition inputStart, RegexPositionSet *out);
static bool matchSequence(const RegexNodeList *list, RegexPosition text,
                          RegexPosition inputStart, RegexPositionSet *out);
static bool matchAlternation(const RegexNodeList *list, RegexPosition text,
                             RegexPosition inputStart, RegexPositionSet *out);
static bool matchRepeat(const RegexRepeat *repeat, RegexPosition text,
                        RegexPosition inputStart, RegexPositionSet *out);
static bool matchRepeatGreedy(const RegexRepeat *repeat, RegexPosition text,
                              RegexPosition inputStart, Index count,
                              RegexPositionSet *out);

static void compileError(RegexStatus *status, Index *errorOffset,
                         RegexStatus code, Index offset) {
    if (status != NULL) {
        *status = code;
    }

    if (errorOffset != NULL) {
        *errorOffset = offset;
    }
}

static RegexNode *newNode(unsigned char type) {
    RegexNode *node = ALLOCATE(RegexNode);

    if (node == NULL) {
        return NULL;
    }

    node->type = type;
    return node;
}

static RegexNode *newEmptyNode(void) { return newNode(REGEX_NODE_EMPTY); }

static RegexNode *newAtomNode(RegexAtom atom) {
    RegexNode *node = newNode(REGEX_NODE_ATOM);

    if (node == NULL) {
        return NULL;
    }

    node->data.atom = atom;
    return node;
}

static RegexNode *newListNode(unsigned char type) {
    RegexNode *node = newNode(type);

    if (node == NULL) {
        return NULL;
    }

    node->data.list.size = 0;
    node->data.list.capacity = 0;
    node->data.list.items = NULL;
    return node;
}

static RegexNode *newRepeatNode(RegexNode *child, Index min, Index max,
                                bool unlimited) {
    RegexNode *node = newNode(REGEX_NODE_REPEAT);

    if (node == NULL) {
        return NULL;
    }

    node->data.repeat.child = child;
    node->data.repeat.min = min;
    node->data.repeat.max = max;
    node->data.repeat.unlimited = unlimited;
    return node;
}

static bool appendNode(RegexNode *node, RegexNode *child) {
    Index oldCapacity;
    Index newCapacity;
    RegexNodePtr *grown;

    if (node->data.list.size == node->data.list.capacity) {
        oldCapacity = node->data.list.capacity;
        newCapacity = oldCapacity < 4 ? 4 : oldCapacity * 2;
        grown = GROW_ARRAY(RegexNodePtr, node->data.list.items, oldCapacity,
                           newCapacity);
        if (grown == NULL) {
            return false;
        }

        node->data.list.items = grown;
        node->data.list.capacity = newCapacity;
    }

    node->data.list.items[node->data.list.size++] = child;
    return true;
}

static void freeCharClass(RegexCharClass *charClass) {
    if (charClass == NULL) {
        return;
    }

    if (charClass->items != NULL) {
        FREE_ARRAY(RegexClassItem, charClass->items, charClass->capacity);
    }

    FREE(charClass, RegexCharClass);
}

static void freeNode(RegexNode *node) {
    Index i;

    if (node == NULL) {
        return;
    }

    switch (node->type) {
    case REGEX_NODE_ATOM:
        if (node->data.atom.type == REGEX_ATOM_CLASS) {
            freeCharClass(node->data.atom.data.charClass);
        }
        break;
    case REGEX_NODE_CONCAT:
    case REGEX_NODE_ALTERNATION:
        for (i = 0; i < node->data.list.size; i++) {
            freeNode(node->data.list.items[i]);
        }
        if (node->data.list.items != NULL) {
            FREE_ARRAY(RegexNodePtr, node->data.list.items,
                       node->data.list.capacity);
        }
        break;
    case REGEX_NODE_REPEAT:
        freeNode(node->data.repeat.child);
        break;
    default:
        break;
    }

    FREE(node, RegexNode);
}

static bool appendClassItem(RegexCharClass *charClass, RegexClassItem item) {
    Index oldCapacity;
    Index newCapacity;
    RegexClassItem *grown;

    if (charClass->size == charClass->capacity) {
        oldCapacity = charClass->capacity;
        newCapacity = oldCapacity < 4 ? 4 : oldCapacity * 2;
        grown = GROW_ARRAY(RegexClassItem, charClass->items, oldCapacity,
                           newCapacity);
        if (grown == NULL) {
            return false;
        }

        charClass->items = grown;
        charClass->capacity = newCapacity;
    }

    charClass->items[charClass->size++] = item;
    return true;
}

static bool appendPosition(RegexPositionSet *set, RegexPosition position) {
    Index i;
    Index oldCapacity;
    Index newCapacity;
    RegexPosition *grown;

    for (i = 0; i < set->size; i++) {
        if (set->items[i] == position) {
            return true;
        }
    }

    if (set->size == set->capacity) {
        oldCapacity = set->capacity;
        newCapacity = oldCapacity < 4 ? 4 : oldCapacity * 2;
        grown = GROW_ARRAY(RegexPosition, set->items, oldCapacity, newCapacity);
        if (grown == NULL) {
            return false;
        }

        set->items = grown;
        set->capacity = newCapacity;
    }

    set->items[set->size++] = position;
    return true;
}

static bool appendPositions(RegexPositionSet *dest,
                            const RegexPositionSet *source) {
    Index i;

    for (i = 0; i < source->size; i++) {
        if (!appendPosition(dest, source->items[i])) {
            return false;
        }
    }

    return true;
}

static void freePositionSet(RegexPositionSet *set) {
    if (set->items != NULL) {
        FREE_ARRAY(RegexPosition, set->items, set->capacity);
    }

    set->items = NULL;
    set->size = 0;
    set->capacity = 0;
}

static bool parseEscapedAtom(const Character *pattern, Index *cursor,
                             RegexAtom *atom, RegexStatus *status,
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
        atom->type = REGEX_ATOM_DIGIT;
        return true;
    case L'D':
        atom->type = REGEX_ATOM_NOT_DIGIT;
        return true;
    case L's':
        atom->type = REGEX_ATOM_WHITESPACE;
        return true;
    case L'S':
        atom->type = REGEX_ATOM_NOT_WHITESPACE;
        return true;
    case L'w':
        atom->type = REGEX_ATOM_WORD;
        return true;
    case L'W':
        atom->type = REGEX_ATOM_NOT_WORD;
        return true;
    default:
        atom->type = REGEX_ATOM_LITERAL;
        atom->data.literal = escaped;
        return true;
    }
}

static bool parseEscapedClassItem(const Character *pattern, Index *cursor,
                                  RegexClassItem *item, RegexStatus *status,
                                  Index *errorOffset) {
    RegexAtom atom;

    if (!parseEscapedAtom(pattern, cursor, &atom, status, errorOffset)) {
        return false;
    }

    if (atom.type == REGEX_ATOM_LITERAL) {
        item->type = REGEX_CLASS_ITEM_LITERAL;
        item->data.literal = atom.data.literal;
    } else {
        item->type = REGEX_CLASS_ITEM_META;
        item->data.metaType = atom.type;
    }

    return true;
}

static bool parseCategoryName(const Character *name, Index length,
                              RegexCategory *category) {
    if (length == 1) {
        category->exact = false;
        switch (name[0]) {
        case L'L':
            category->code = GC_L;
            return true;
        case L'M':
            category->code = GC_M;
            return true;
        case L'N':
            category->code = GC_N;
            return true;
        case L'P':
            category->code = GC_P;
            return true;
        case L'S':
            category->code = GC_S;
            return true;
        case L'Z':
            category->code = GC_Z;
            return true;
        case L'C':
            category->code = GC_C;
            return true;
        default:
            return false;
        }
    }

    if (length == 2) {
        category->exact = true;
        switch (name[0]) {
        case L'L':
            switch (name[1]) {
            case L'l':
                category->code = GC_Ll;
                return true;
            case L'm':
                category->code = GC_Lm;
                return true;
            case L'o':
                category->code = GC_Lo;
                return true;
            case L't':
                category->code = GC_Lt;
                return true;
            case L'u':
                category->code = GC_Lu;
                return true;
            default:
                return false;
            }
        case L'M':
            switch (name[1]) {
            case L'c':
                category->code = GC_Mc;
                return true;
            case L'e':
                category->code = GC_Me;
                return true;
            case L'n':
                category->code = GC_Mn;
                return true;
            default:
                return false;
            }
        case L'N':
            switch (name[1]) {
            case L'd':
                category->code = GC_Nd;
                return true;
            case L'l':
                category->code = GC_Nl;
                return true;
            case L'o':
                category->code = GC_No;
                return true;
            default:
                return false;
            }
        case L'P':
            switch (name[1]) {
            case L'c':
                category->code = GC_Pc;
                return true;
            case L'd':
                category->code = GC_Pd;
                return true;
            case L'e':
                category->code = GC_Pe;
                return true;
            case L'f':
                category->code = GC_Pf;
                return true;
            case L'i':
                category->code = GC_Pi;
                return true;
            case L'o':
                category->code = GC_Po;
                return true;
            case L's':
                category->code = GC_Ps;
                return true;
            default:
                return false;
            }
        case L'S':
            switch (name[1]) {
            case L'c':
                category->code = GC_Sc;
                return true;
            case L'k':
                category->code = GC_Sk;
                return true;
            case L'm':
                category->code = GC_Sm;
                return true;
            case L'o':
                category->code = GC_So;
                return true;
            default:
                return false;
            }
        case L'Z':
            switch (name[1]) {
            case L'l':
                category->code = GC_Zl;
                return true;
            case L'p':
                category->code = GC_Zp;
                return true;
            case L's':
                category->code = GC_Zs;
                return true;
            default:
                return false;
            }
        case L'C':
            switch (name[1]) {
            case L'c':
                category->code = GC_Cc;
                return true;
            case L'f':
                category->code = GC_Cf;
                return true;
            case L'n':
                category->code = GC_Cn;
                return true;
            case L'o':
                category->code = GC_Co;
                return true;
            case L's':
                category->code = GC_Cs;
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
                               RegexCategory *category, RegexStatus *status,
                               Index *errorOffset) {
    Index nameStart;
    Index nameEnd;

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

    if (!parseCategoryName(&pattern[nameStart], nameEnd - nameStart,
                           category)) {
        compileError(status, errorOffset, REGEX_STATUS_UNKNOWN_CATEGORY,
                     nameStart);
        return false;
    }

    *cursor = nameEnd + 2;
    return true;
}

static RegexCharClass *parseCharClass(const Character *pattern, Index *cursor,
                                      RegexStatus *status, Index *errorOffset) {
    RegexCharClass *charClass = ALLOCATE(RegexCharClass);
    Index i = *cursor + 1;

    if (charClass == NULL) {
        compileError(status, errorOffset, REGEX_STATUS_OUT_OF_MEMORY, *cursor);
        return NULL;
    }

    charClass->inverted = false;
    charClass->size = 0;
    charClass->capacity = 0;
    charClass->items = NULL;

    if (pattern[i] == L'^') {
        charClass->inverted = true;
        i++;
    }

    while (pattern[i] != L']') {
        RegexClassItem item;

        if (pattern[i] == L'\0') {
            compileError(status, errorOffset, REGEX_STATUS_UNTERMINATED_CLASS,
                         *cursor);
            freeCharClass(charClass);
            return NULL;
        }

        if (pattern[i] == L'[' && pattern[i + 1] == L'[') {
            item.type = REGEX_CLASS_ITEM_CATEGORY;
            if (!parseNamedCategory(pattern, &i, &item.data.category, status,
                                    errorOffset)) {
                freeCharClass(charClass);
                return NULL;
            }
        } else if (pattern[i] == L'\\') {
            if (!parseEscapedClassItem(pattern, &i, &item, status,
                                       errorOffset)) {
                freeCharClass(charClass);
                return NULL;
            }
            i++;
        } else if (pattern[i + 1] == L'-' && pattern[i + 2] != L'\0' &&
                   pattern[i + 2] != L']' && pattern[i + 2] != L'\\' &&
                   !(pattern[i + 2] == L'[' && pattern[i + 3] == L'[')) {
            item.type = REGEX_CLASS_ITEM_RANGE;
            item.data.range.lower = pattern[i];
            item.data.range.upper = pattern[i + 2];
            if (item.data.range.lower > item.data.range.upper) {
                compileError(status, errorOffset, REGEX_STATUS_INVALID_RANGE,
                             i);
                freeCharClass(charClass);
                return NULL;
            }
            i += 3;
        } else {
            item.type = REGEX_CLASS_ITEM_LITERAL;
            item.data.literal = pattern[i];
            i++;
        }

        if (!appendClassItem(charClass, item)) {
            compileError(status, errorOffset, REGEX_STATUS_OUT_OF_MEMORY, i);
            freeCharClass(charClass);
            return NULL;
        }
    }

    if (charClass->size == 0) {
        compileError(status, errorOffset, REGEX_STATUS_EMPTY_CLASS, *cursor);
        freeCharClass(charClass);
        return NULL;
    }

    *cursor = i + 1;
    return charClass;
}

static RegexNode *parsePrimary(RegexParser *parser) {
    Character current = parser->pattern[parser->cursor];
    RegexAtom atom;
    RegexNode *node;
    RegexCharClass *charClass;

    if (current == L'*' || current == L'+' || current == L'?') {
        compileError(parser->status, parser->errorOffset,
                     REGEX_STATUS_QUANTIFIER_WITHOUT_TARGET, parser->cursor);
        return NULL;
    }

    switch (current) {
    case L'(':
        parser->cursor++;
        node = parseExpression(parser);
        if (node == NULL) {
            return NULL;
        }
        if (parser->pattern[parser->cursor] != L')') {
            compileError(parser->status, parser->errorOffset,
                         REGEX_STATUS_UNTERMINATED_GROUP, parser->cursor);
            freeNode(node);
            return NULL;
        }
        parser->cursor++;
        return node;
    case L'.':
        atom.type = REGEX_ATOM_DOT;
        parser->cursor++;
        return newAtomNode(atom);
    case L'^':
        parser->cursor++;
        return newNode(REGEX_NODE_BEGIN);
    case L'$':
        parser->cursor++;
        return newNode(REGEX_NODE_END);
    case L'[':
        if (parser->pattern[parser->cursor + 1] == L'[') {
            atom.type = REGEX_ATOM_CATEGORY;
            if (!parseNamedCategory(parser->pattern, &parser->cursor,
                                    &atom.data.category, parser->status,
                                    parser->errorOffset)) {
                return NULL;
            }
            return newAtomNode(atom);
        }
        atom.type = REGEX_ATOM_CLASS;
        charClass = parseCharClass(parser->pattern, &parser->cursor,
                                   parser->status, parser->errorOffset);
        if (charClass == NULL) {
            return NULL;
        }
        atom.data.charClass = charClass;
        return newAtomNode(atom);
    case L'\\':
        if (!parseEscapedAtom(parser->pattern, &parser->cursor, &atom,
                              parser->status, parser->errorOffset)) {
            return NULL;
        }
        parser->cursor++;
        return newAtomNode(atom);
    default:
        atom.type = REGEX_ATOM_LITERAL;
        atom.data.literal = current;
        parser->cursor++;
        return newAtomNode(atom);
    }
}

static RegexNode *parseQuantified(RegexParser *parser) {
    RegexNode *node = parsePrimary(parser);
    Character quantifier;
    RegexNode *repeat;

    if (node == NULL) {
        return NULL;
    }

    quantifier = parser->pattern[parser->cursor];
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

    if (repeat == NULL) {
        compileError(parser->status, parser->errorOffset,
                     REGEX_STATUS_OUT_OF_MEMORY, parser->cursor);
        freeNode(node);
        return NULL;
    }

    parser->cursor++;
    if (parser->pattern[parser->cursor] == L'*' ||
        parser->pattern[parser->cursor] == L'+' ||
        parser->pattern[parser->cursor] == L'?') {
        compileError(parser->status, parser->errorOffset,
                     REGEX_STATUS_CONSECUTIVE_QUANTIFIERS, parser->cursor);
        freeNode(repeat);
        return NULL;
    }

    return repeat;
}

static RegexNode *parseSequence(RegexParser *parser) {
    RegexNode *sequence = newListNode(REGEX_NODE_CONCAT);
    RegexNode *child;

    if (sequence == NULL) {
        compileError(parser->status, parser->errorOffset,
                     REGEX_STATUS_OUT_OF_MEMORY, parser->cursor);
        return NULL;
    }

    while (parser->pattern[parser->cursor] != L'\0' &&
           parser->pattern[parser->cursor] != L'|' &&
           parser->pattern[parser->cursor] != L')') {
        child = parseQuantified(parser);
        if (child == NULL) {
            freeNode(sequence);
            return NULL;
        }
        if (!appendNode(sequence, child)) {
            compileError(parser->status, parser->errorOffset,
                         REGEX_STATUS_OUT_OF_MEMORY, parser->cursor);
            freeNode(child);
            freeNode(sequence);
            return NULL;
        }
    }

    if (sequence->data.list.size == 0) {
        freeNode(sequence);
        return newEmptyNode();
    }

    if (sequence->data.list.size == 1) {
        child = sequence->data.list.items[0];
        FREE_ARRAY(RegexNodePtr, sequence->data.list.items,
                   sequence->data.list.capacity);
        FREE(sequence, RegexNode);
        return child;
    }

    return sequence;
}

static RegexNode *parseExpression(RegexParser *parser) {
    RegexNode *left = parseSequence(parser);
    RegexNode *alternation;
    RegexNode *right;

    if (left == NULL) {
        return NULL;
    }

    if (parser->pattern[parser->cursor] != L'|') {
        return left;
    }

    alternation = newListNode(REGEX_NODE_ALTERNATION);
    if (alternation == NULL) {
        compileError(parser->status, parser->errorOffset,
                     REGEX_STATUS_OUT_OF_MEMORY, parser->cursor);
        freeNode(left);
        return NULL;
    }

    if (!appendNode(alternation, left)) {
        compileError(parser->status, parser->errorOffset,
                     REGEX_STATUS_OUT_OF_MEMORY, parser->cursor);
        freeNode(left);
        freeNode(alternation);
        return NULL;
    }

    while (parser->pattern[parser->cursor] == L'|') {
        parser->cursor++;
        right = parseSequence(parser);
        if (right == NULL) {
            freeNode(alternation);
            return NULL;
        }
        if (!appendNode(alternation, right)) {
            compileError(parser->status, parser->errorOffset,
                         REGEX_STATUS_OUT_OF_MEMORY, parser->cursor);
            freeNode(right);
            freeNode(alternation);
            return NULL;
        }
    }

    return alternation;
}

Regex *regexCompile(const Character *pattern, RegexStatus *status,
                    Index *errorOffset) {
    RegexParser parser;
    Regex *regex;
    RegexNode *root;

    compileError(status, errorOffset, REGEX_STATUS_OK, 0);
    parser.pattern = pattern;
    parser.cursor = 0;
    parser.status = status;
    parser.errorOffset = errorOffset;

    root = parseExpression(&parser);
    if (root == NULL) {
        return NULL;
    }

    if (pattern[parser.cursor] == L')') {
        compileError(status, errorOffset, REGEX_STATUS_UNEXPECTED_CLOSE_PAREN,
                     parser.cursor);
        freeNode(root);
        return NULL;
    }

    regex = ALLOCATE(Regex);
    if (regex == NULL) {
        compileError(status, errorOffset, REGEX_STATUS_OUT_OF_MEMORY,
                     parser.cursor);
        freeNode(root);
        return NULL;
    }

    regex->root = root;
    return regex;
}

void regexFree(Regex *regex) {
    if (regex == NULL) {
        return;
    }

    freeNode(regex->root);
    FREE(regex, Regex);
}

static bool matchCategory(RegexCategory category, Character c) {
    int actual = unicode_category(c);

    if (category.exact) {
        return actual == category.code;
    }

    return (actual & GC_MASK) == category.code;
}

static bool matchMeta(unsigned char metaType, Character c) {
    switch (metaType) {
    case REGEX_ATOM_DIGIT:
        return unicode_isdigit(c);
    case REGEX_ATOM_NOT_DIGIT:
        return !unicode_isdigit(c);
    case REGEX_ATOM_WORD:
        return unicode_isalnum(c);
    case REGEX_ATOM_NOT_WORD:
        return !unicode_isalnum(c);
    case REGEX_ATOM_WHITESPACE:
        return unicode_isspace(c);
    case REGEX_ATOM_NOT_WHITESPACE:
        return !unicode_isspace(c);
    default:
        return false;
    }
}

static bool matchClassItem(const RegexClassItem *item, Character c) {
    switch (item->type) {
    case REGEX_CLASS_ITEM_LITERAL:
        return c == item->data.literal;
    case REGEX_CLASS_ITEM_RANGE:
        return c >= item->data.range.lower && c <= item->data.range.upper;
    case REGEX_CLASS_ITEM_META:
        return matchMeta(item->data.metaType, c);
    case REGEX_CLASS_ITEM_CATEGORY:
        return matchCategory(item->data.category, c);
    default:
        return false;
    }
}

static bool matchCharClass(const RegexCharClass *charClass, Character c) {
    bool matched = false;
    Index i;

    for (i = 0; i < charClass->size; i++) {
        if (matchClassItem(&charClass->items[i], c)) {
            matched = true;
            break;
        }
    }

    return charClass->inverted ? !matched : matched;
}

static bool matchAtom(const RegexAtom *atom, Character c) {
    switch (atom->type) {
    case REGEX_ATOM_LITERAL:
        return atom->data.literal == c;
    case REGEX_ATOM_DOT:
        return c != L'\n' && c != L'\r';
    case REGEX_ATOM_CLASS:
        return matchCharClass(atom->data.charClass, c);
    case REGEX_ATOM_DIGIT:
    case REGEX_ATOM_NOT_DIGIT:
    case REGEX_ATOM_WORD:
    case REGEX_ATOM_NOT_WORD:
    case REGEX_ATOM_WHITESPACE:
    case REGEX_ATOM_NOT_WHITESPACE:
        return matchMeta(atom->type, c);
    case REGEX_ATOM_CATEGORY:
        return matchCategory(atom->data.category, c);
    default:
        return false;
    }
}

static bool matchSequence(const RegexNodeList *list, RegexPosition text,
                          RegexPosition inputStart, RegexPositionSet *out) {
    RegexPositionSet current = {0, 0, NULL};
    RegexPositionSet next = {0, 0, NULL};
    RegexPositionSet childMatches = {0, 0, NULL};
    Index i;
    Index j;
    bool ok;

    if (!appendPosition(&current, text)) {
        freePositionSet(&current);
        return false;
    }

    for (i = 0; i < list->size; i++) {
        for (j = 0; j < current.size; j++) {
            childMatches.size = 0;
            childMatches.capacity = 0;
            childMatches.items = NULL;
            ok = matchNode(list->items[i], current.items[j], inputStart,
                           &childMatches);
            if (!ok) {
                freePositionSet(&current);
                freePositionSet(&next);
                freePositionSet(&childMatches);
                return false;
            }
            ok = appendPositions(&next, &childMatches);
            freePositionSet(&childMatches);
            if (!ok) {
                freePositionSet(&current);
                freePositionSet(&next);
                return false;
            }
        }

        freePositionSet(&current);
        current = next;
        next.items = NULL;
        next.size = 0;
        next.capacity = 0;

        if (current.size == 0) {
            break;
        }
    }

    ok = appendPositions(out, &current);
    freePositionSet(&current);
    return ok;
}

static bool matchAlternation(const RegexNodeList *list, RegexPosition text,
                             RegexPosition inputStart, RegexPositionSet *out) {
    RegexPositionSet branchMatches = {0, 0, NULL};
    Index i;
    bool ok;

    for (i = 0; i < list->size; i++) {
        branchMatches.size = 0;
        branchMatches.capacity = 0;
        branchMatches.items = NULL;
        ok = matchNode(list->items[i], text, inputStart, &branchMatches);
        if (!ok) {
            freePositionSet(&branchMatches);
            return false;
        }
        ok = appendPositions(out, &branchMatches);
        freePositionSet(&branchMatches);
        if (!ok) {
            return false;
        }
    }

    return true;
}

static bool matchRepeatGreedy(const RegexRepeat *repeat, RegexPosition text,
                              RegexPosition inputStart, Index count,
                              RegexPositionSet *out) {
    RegexPositionSet childMatches = {0, 0, NULL};
    Index i;
    bool ok;

    if (repeat->unlimited || count < repeat->max) {
        ok = matchNode(repeat->child, text, inputStart, &childMatches);
        if (!ok) {
            freePositionSet(&childMatches);
            return false;
        }
        for (i = 0; i < childMatches.size; i++) {
            if (childMatches.items[i] == text) {
                continue;
            }
            if (!matchRepeatGreedy(repeat, childMatches.items[i], inputStart,
                                   count + 1, out)) {
                freePositionSet(&childMatches);
                return false;
            }
        }
        freePositionSet(&childMatches);
    }

    if (count >= repeat->min) {
        return appendPosition(out, text);
    }

    return true;
}

static bool matchRepeat(const RegexRepeat *repeat, RegexPosition text,
                        RegexPosition inputStart, RegexPositionSet *out) {
    return matchRepeatGreedy(repeat, text, inputStart, 0, out);
}

static bool matchNode(const RegexNode *node, RegexPosition text,
                      RegexPosition inputStart, RegexPositionSet *out) {
    switch (node->type) {
    case REGEX_NODE_EMPTY:
        return appendPosition(out, text);
    case REGEX_NODE_ATOM:
        if (*text != L'\0' && matchAtom(&node->data.atom, *text)) {
            return appendPosition(out, text + 1);
        }
        return true;
    case REGEX_NODE_BEGIN:
        if (text == inputStart) {
            return appendPosition(out, text);
        }
        return true;
    case REGEX_NODE_END:
        if (*text == L'\0') {
            return appendPosition(out, text);
        }
        return true;
    case REGEX_NODE_CONCAT:
        return matchSequence(&node->data.list, text, inputStart, out);
    case REGEX_NODE_ALTERNATION:
        return matchAlternation(&node->data.list, text, inputStart, out);
    case REGEX_NODE_REPEAT:
        return matchRepeat(&node->data.repeat, text, inputStart, out);
    default:
        return false;
    }
}

int regexMatchp(const Regex *pattern, const Character *text,
                Index *matchLength) {
    RegexPositionSet matches = {0, 0, NULL};
    int index = 0;

    if (matchLength != NULL) {
        *matchLength = 0;
    }

    if (pattern == NULL || text == NULL) {
        return -1;
    }

    while (true) {
        matches.size = 0;
        matches.capacity = 0;
        matches.items = NULL;

        if (!matchNode(pattern->root, text + index, text, &matches)) {
            freePositionSet(&matches);
            return -1;
        }

        if (matches.size > 0) {
            if (matchLength != NULL) {
                *matchLength = (Index)(matches.items[0] - (text + index));
            }
            freePositionSet(&matches);
            return index;
        }

        freePositionSet(&matches);
        if (text[index] == L'\0') {
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

const char *regexStatusName(RegexStatus status) {
    switch (status) {
    case REGEX_STATUS_OK:
        return "REGEX_STATUS_OK";
    case REGEX_STATUS_OUT_OF_MEMORY:
        return "REGEX_STATUS_OUT_OF_MEMORY";
    case REGEX_STATUS_TRAILING_ESCAPE:
        return "REGEX_STATUS_TRAILING_ESCAPE";
    case REGEX_STATUS_UNTERMINATED_CLASS:
        return "REGEX_STATUS_UNTERMINATED_CLASS";
    case REGEX_STATUS_UNTERMINATED_NAMED_CLASS:
        return "REGEX_STATUS_UNTERMINATED_NAMED_CLASS";
    case REGEX_STATUS_UNTERMINATED_GROUP:
        return "REGEX_STATUS_UNTERMINATED_GROUP";
    case REGEX_STATUS_UNEXPECTED_CLOSE_PAREN:
        return "REGEX_STATUS_UNEXPECTED_CLOSE_PAREN";
    case REGEX_STATUS_EMPTY_CLASS:
        return "REGEX_STATUS_EMPTY_CLASS";
    case REGEX_STATUS_UNKNOWN_CATEGORY:
        return "REGEX_STATUS_UNKNOWN_CATEGORY";
    case REGEX_STATUS_INVALID_RANGE:
        return "REGEX_STATUS_INVALID_RANGE";
    case REGEX_STATUS_QUANTIFIER_WITHOUT_TARGET:
        return "REGEX_STATUS_QUANTIFIER_WITHOUT_TARGET";
    case REGEX_STATUS_CONSECUTIVE_QUANTIFIERS:
        return "REGEX_STATUS_CONSECUTIVE_QUANTIFIERS";
    default:
        return "REGEX_STATUS_UNKNOWN";
    }
}