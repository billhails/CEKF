#ifndef cekf_regex_filepos_h
#define cekf_regex_filepos_h

#include "cmp.h"
#include <stdio.h>

void fprintRegexFilePos(FILE *fp, fpos_t pos, int depth);
Cmp cmpRegexFilePos(fpos_t a, fpos_t b);

#endif