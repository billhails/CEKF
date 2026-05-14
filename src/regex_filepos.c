#include "regex_filepos.h"

#include <string.h>

void fprintRegexFilePos(FILE *fp, fpos_t pos, int depth) {
    (void)pos;
    (void)depth;
    fprintf(fp, "<filepos>");
}

Cmp cmpRegexFilePos(fpos_t a, fpos_t b) {
    int result = memcmp(&a, &b, sizeof(fpos_t));

    if (result < 0) {
        return CMP_LT;
    }
    if (result > 0) {
        return CMP_GT;
    }
    return CMP_EQ;
}