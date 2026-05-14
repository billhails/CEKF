#ifndef cekf_regex_source_h
#define cekf_regex_source_h

#include "cekf.h"
#include "regex.h"

Character regexSourceGet(RegexSource *source, Index position);
void regexSourceSplitAt(RegexSource *source, Index offset, Value *prefix,
                        Value *rest);
RegexSource *regexSourceFromStringList(Vec *tail);
RegexSource *regexSourceFromCharArray(CharacterArray *text);

#endif