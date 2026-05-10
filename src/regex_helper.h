#ifndef cekf_regex_helper_h
#define cekf_regex_helper_h

#include "regex.h"
#include <stdbool.h>

#define REGEX_STATUS_OK REGEXSTATUS_TYPE_OK
#define REGEX_STATUS_OUT_OF_MEMORY REGEXSTATUS_TYPE_OUT_OF_MEMORY
#define REGEX_STATUS_TRAILING_ESCAPE REGEXSTATUS_TYPE_TRAILING_ESCAPE
#define REGEX_STATUS_UNTERMINATED_CLASS REGEXSTATUS_TYPE_UNTERMINATED_CLASS
#define REGEX_STATUS_UNTERMINATED_NAMED_CLASS                                  \
    REGEXSTATUS_TYPE_UNTERMINATED_NAMED_CLASS
#define REGEX_STATUS_UNTERMINATED_GROUP REGEXSTATUS_TYPE_UNTERMINATED_GROUP
#define REGEX_STATUS_UNEXPECTED_CLOSE_PAREN                                    \
    REGEXSTATUS_TYPE_UNEXPECTED_CLOSE_PAREN
#define REGEX_STATUS_EMPTY_CLASS REGEXSTATUS_TYPE_EMPTY_CLASS
#define REGEX_STATUS_UNKNOWN_CATEGORY REGEXSTATUS_TYPE_UNKNOWN_CATEGORY
#define REGEX_STATUS_INVALID_RANGE REGEXSTATUS_TYPE_INVALID_RANGE
#define REGEX_STATUS_QUANTIFIER_WITHOUT_TARGET                                 \
    REGEXSTATUS_TYPE_QUANTIFIER_WITHOUT_TARGET
#define REGEX_STATUS_CONSECUTIVE_QUANTIFIERS                                   \
    REGEXSTATUS_TYPE_CONSECUTIVE_QUANTIFIERS

Regex *regexCompile(const Character *pattern, RegexStatus *status,
                    Index *errorOffset);
void regexFree(Regex *regex);

int regexMatchp(const Regex *pattern, const Character *text,
                Index *matchLength);
int regexMatch(const Character *pattern, const Character *text,
               Index *matchLength, RegexStatus *status, Index *errorOffset);

#endif