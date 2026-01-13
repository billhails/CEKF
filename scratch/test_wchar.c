#include <wchar.h>
#include <wctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <locale.h>

void testwc(char *name, wchar_t character, bool result) {
    printf("%s('%lc') == %s\n", name, character, result ? "true" : "false");
}

#define TESTWC(func, char) testwc(#func, char, func(char))

/**
 * Not really a test, just a clarification of wide string functions.
 */
int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    setlocale(LC_ALL, "C.UTF-8");
    TESTWC(iswalpha, L'λ');
    TESTWC(iswalpha, L'A');
    TESTWC(iswalpha, L'0');
    TESTWC(iswdigit, L'5');
    TESTWC(iswdigit, L'A');
    TESTWC(iswspace, L' ');
    TESTWC(iswspace, L'\n');
    TESTWC(iswspace, L'\t');
    TESTWC(iswspace, L'A');
    TESTWC(iswpunct, L'!');
    TESTWC(iswdigit, 0x661); // Arabic-Indic digit 1
}