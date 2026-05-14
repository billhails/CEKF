#ifndef cekf_regex_source_h
#define cekf_regex_source_h

#include "cekf.h"
#include "regex.h"

Character regexSourceGet(RegexSource *source, Index position);
void regexSourceSetPosition(RegexSource *source, Index position);
void regexSourceSplitAt(RegexSource *source, Index offset, Value *prefix,
                        Value *rest);
RegexSource *regexSourceFromFileHandle(FILE *handle);
RegexSource *regexSourceFromStringList(Vec *tail);
RegexSource *regexSourceFromCharArray(CharacterArray *text);

#endif