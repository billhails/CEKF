#ifndef cekf_regex_h
#define cekf_regex_h

#include "types.h"
#include <stdbool.h>

typedef enum {
    REGEX_STATUS_OK = 0,
    REGEX_STATUS_OUT_OF_MEMORY,
    REGEX_STATUS_TRAILING_ESCAPE,
    REGEX_STATUS_UNTERMINATED_CLASS,
    REGEX_STATUS_UNTERMINATED_NAMED_CLASS,
    REGEX_STATUS_UNTERMINATED_GROUP,
    REGEX_STATUS_UNEXPECTED_CLOSE_PAREN,
    REGEX_STATUS_EMPTY_CLASS,
    REGEX_STATUS_UNKNOWN_CATEGORY,
    REGEX_STATUS_INVALID_RANGE,
    REGEX_STATUS_QUANTIFIER_WITHOUT_TARGET,
    REGEX_STATUS_CONSECUTIVE_QUANTIFIERS,
} RegexStatus;

typedef struct Regex Regex;

Regex *regexCompile(const Character *pattern, RegexStatus *status,
                    Index *errorOffset);
void regexFree(Regex *regex);

int regexMatchp(const Regex *pattern, const Character *text,
                Index *matchLength);
int regexMatch(const Character *pattern, const Character *text,
               Index *matchLength, RegexStatus *status, Index *errorOffset);

const char *regexStatusName(RegexStatus status);

#endif