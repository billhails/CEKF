#include "regex_source.h"

#include "memory.h"
#include "value.h"

#include <limits.h>
#include <string.h>
#include <wchar.h>

static void ensureRegexSourceMemoryReady(void);
static Value charArraySliceToList(const CharacterArray *source, Index start,
                                  Index end, Value tail);
static Value textSliceToList(const Character *source, Index start, Index end,
                             Value tail);
static bool regexFileSourceReadNext(RegexFileSource *fileSource);

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
    case REGEXSOURCE_TYPE_FILE: {
        RegexFileSource *fileSource = getRegexSource_File(source);

        while (position >= fileSource->cache->size && !fileSource->exhausted) {
            if (!regexFileSourceReadNext(fileSource)) {
                break;
            }
        }

        if (position < fileSource->cache->size) {
            return getCharacterArray(fileSource->cache, position);
        }
        return L'\0';
    }
    case REGEXSOURCE_TYPE_TEXT: {
        RegexTextSource *textSource = getRegexSource_Text(source);

        return textSource->start[position];
    }
    default:
        cant_happen("unrecognised regex source type %d", source->type);
    }
}

static bool regexFileSourceReadNext(RegexFileSource *fileSource) {
    Character wc = 0;
    char buf[MB_LEN_MAX];
    int bytesRead = 0;
    bool consumed = false;
    fpos_t nextPos;

    while (bytesRead < MB_LEN_MAX) {
        int byte = fgetc(fileSource->handle);
        if (byte == EOF) {
            fileSource->exhausted = true;
            break;
        }
        buf[bytesRead++] = (char)byte;
        consumed = true;

        mbstate_t state;
        memset(&state, 0, sizeof(state));

        size_t result = mbrtowc(&wc, buf, (size_t)bytesRead, &state);
        if (result == (size_t)-1) {
            wc = 0xFFFD;
            break;
        }
        if (result == (size_t)-2) {
            continue;
        }
        break;
    }

    if (!consumed) {
        return false;
    }

    if (fgetpos(fileSource->handle, &nextPos) != 0) {
        cant_happen("unable to capture regex file position after read");
    }

    pushCharacterArray(fileSource->cache, wc);
    pushRegexFilePosArray(fileSource->positions, nextPos);
    return true;
}

void regexSourceSetPosition(RegexSource *source, Index position) {
    if (source == NULL) {
        return;
    }

    switch (source->type) {
    case REGEXSOURCE_TYPE_STRING:
    case REGEXSOURCE_TYPE_TEXT:
        return;
    case REGEXSOURCE_TYPE_FILE: {
        RegexFileSource *fileSource = getRegexSource_File(source);

        while (position > fileSource->cache->size && !fileSource->exhausted) {
            if (!regexFileSourceReadNext(fileSource)) {
                break;
            }
        }

#ifdef SAFETY_CHECKS
        if (position >= fileSource->positions->size) {
            cant_happen(
                "regex file source position %u exceeds cached boundary %u",
                position, fileSource->positions->size - 1);
        }
#endif
        if (fsetpos(fileSource->handle,
                    &fileSource->positions->entries[position]) != 0) {
            cant_happen("unable to restore regex file position");
        }
        return;
    }
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

static Value textSliceToList(const Character *source, Index start, Index end,
                             Value tail) {
    Value list = tail;
    int save = protectValue(list);

    for (Index i = end; i > start; i--) {
        list = makePair(value_Character(source[i - 1]), list);
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
    case REGEXSOURCE_TYPE_FILE: {
        RegexFileSource *fileSource = getRegexSource_File(source);
        Value empty = makeNull();

        protectValue(empty);
        if (rest != NULL) {
            cant_happen("regex file source rest split not implemented yet");
        }
        if (prefix != NULL) {
            *prefix = charArraySliceToList(fileSource->cache, 0, offset, empty);
            protectValue(*prefix);
        }
        break;
    }
    case REGEXSOURCE_TYPE_TEXT: {
        RegexTextSource *textSource = getRegexSource_Text(source);
        Value empty = makeNull();
        Index length = (Index)wcslen(textSource->start);

        protectValue(empty);
        if (prefix != NULL) {
            *prefix = textSliceToList(textSource->start, 0, offset, empty);
            protectValue(*prefix);
        }
        if (rest != NULL) {
            *rest = textSliceToList(textSource->start, offset, length, empty);
            protectValue(*rest);
        }
        break;
    }
    default:
        cant_happen("unrecognised regex source type %d", source->type);
    }

    UNPROTECT(save);
}

RegexSource *regexSourceFromFileHandle(FILE *handle) {
    CharacterArray *cache;
    RegexFilePosArray *positions;
    RegexSource *source;
    fpos_t startPos;
    int save;

    ensureRegexSourceMemoryReady();
    if (fgetpos(handle, &startPos) != 0) {
        cant_happen("unable to capture initial regex file position");
    }

    cache = newCharacterArray();
    save = PROTECT(cache);
    positions = newRegexFilePosArray();
    PROTECT(positions);
    pushRegexFilePosArray(positions, startPos);
    source = makeRegexSource_File(handle, startPos, positions, cache);
    UNPROTECT(save);
    return source;
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

RegexSource *regexSourceFromWCharVecSlice(WCharVec *text, Character *start) {
    RegexTextSource *textSource;
    RegexSource *source;
    int save;

    ensureRegexSourceMemoryReady();
    save = PROTECT(text);
    textSource = newRegexTextSource(text);
    PROTECT(textSource);
    textSource->start = start;
    source = newRegexSource_Text(textSource);
    UNPROTECT(save);
    return source;
}