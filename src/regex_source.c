#include "regex_source.h"

#include "memory.h"
#include "value.h"

static void ensureRegexSourceMemoryReady(void);
static Value charArraySliceToList(const CharacterArray *source, Index start,
                                  Index end, Value tail);

static void ensureRegexSourceMemoryReady(void) {
    if (!protectionInitialized()) {
        initProtection();
    }
}

Character regexSourceGet(RegexSource *source, Index position) {
    switch (source->type) {
    case REGEXSOURCE_TYPE_STRING: {
        RegexStringSource *stringSource = getRegexSource_String(source);

        while (position >= stringSource->cache->size &&
               !stringSource->exhausted) {
            Vec *tail = stringSource->tail;

            if (tail == NULL) {
                stringSource->exhausted = true;
                break;
            }
#ifdef SAFETY_CHECKS
            if (tail->size != 3 && tail->size != 1) {
                cant_happen("unexpected string source node size %d",
                            tail->size);
            }
#endif
            if (tail->size == 1) {
                stringSource->tail = NULL;
                stringSource->exhausted = true;
                break;
            }

            Value character = tail->entries[1];
            Value next = tail->entries[2];

#ifdef SAFETY_CHECKS
            if (character.type != VALUE_TYPE_CHARACTER) {
                cant_happen("unexpected string source character %s",
                            valueTypeName(character.type));
            }
            if (next.type != VALUE_TYPE_VEC) {
                cant_happen("unexpected string source tail %s",
                            valueTypeName(next.type));
            }
#endif
            pushCharacterArray(stringSource->cache, character.val.character);
            stringSource->tail = next.val.vec;
        }

        if (position < stringSource->cache->size) {
            return getCharacterArray(stringSource->cache, position);
        }
        return L'\0';
    }
    case REGEXSOURCE_TYPE_FILE:
        cant_happen("regex file source get not implemented yet");
    default:
        cant_happen("unrecognised regex source type %d", source->type);
    }
}

static Value charArraySliceToList(const CharacterArray *source, Index start,
                                  Index end, Value tail) {
    Value list = tail;
    int save = protectValue(list);

    for (Index i = end; i > start; i--) {
        list = makePair(value_Character(source->entries[i - 1]), list);
        protectValue(list);
    }

    UNPROTECT(save);
    return list;
}

void regexSourceSplitAt(RegexSource *source, Index offset, Value *prefix,
                        Value *rest) {
    int save;

    ensureRegexSourceMemoryReady();

    if (prefix != NULL) {
        *prefix = makeNull();
    }
    if (rest != NULL) {
        *rest = makeNull();
    }

    if (source == NULL) {
        return;
    }

    save = STARTPROTECT();
    PROTECT(source);

    if (offset > 0) {
        (void)regexSourceGet(source, offset - 1);
    }

    switch (source->type) {
    case REGEXSOURCE_TYPE_STRING: {
        RegexStringSource *stringSource = getRegexSource_String(source);
        Value empty = makeNull();
        protectValue(empty);
        Value restTail =
            stringSource->tail != NULL ? value_Vec(stringSource->tail) : empty;

        if (prefix != NULL) {
            *prefix =
                charArraySliceToList(stringSource->cache, 0, offset, empty);
            protectValue(*prefix);
        }
        if (rest != NULL) {
            *rest = charArraySliceToList(stringSource->cache, offset,
                                         stringSource->cache->size, restTail);
            protectValue(*rest);
        }
        break;
    }
    case REGEXSOURCE_TYPE_FILE:
        cant_happen("regex file source split not implemented yet");
    default:
        cant_happen("unrecognised regex source type %d", source->type);
    }

    UNPROTECT(save);
}

RegexSource *regexSourceFromStringList(Vec *tail) {
    CharacterArray *cache;
    RegexSource *source;
    int save;

    ensureRegexSourceMemoryReady();
    cache = newCharacterArray();
    save = PROTECT(cache);
    source = makeRegexSource_String(tail, cache);
    UNPROTECT(save);
    return source;
}

RegexSource *regexSourceFromCharArray(CharacterArray *text) {
    RegexSource *source;
    int save;

    ensureRegexSourceMemoryReady();
    source = makeRegexSource_String(NULL, text);
    save = PROTECT(source);
    getRegexSource_String(source)->exhausted = true;
    UNPROTECT(save);
    return source;
}