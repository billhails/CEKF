#include "test.h"

#include "regex.h"

static void testAsciiAndAnchors(void) {
    RegexStatus status;
    Index errorOffset;
    Index matchLength;
    Regex *regex = regexCompile(L"^ab+$", &status, &errorOffset);

    assert(regex != NULL);
    assert(status == REGEX_STATUS_OK);
    assert(regexMatchp(regex, L"abbb", &matchLength) == 0);
    assert(matchLength == 4);
    assert(regexMatchp(regex, L"cabbb", &matchLength) == -1);
    regexFree(regex);
}

static void testUnicodeCategoriesAsAtoms(void) {
    RegexStatus status;
    Index errorOffset;
    Index matchLength;
    Regex *regex = regexCompile(L"^[[Lu]][[Ll]]+$", &status, &errorOffset);

    assert(regex != NULL);
    assert(status == REGEX_STATUS_OK);
    assert(regexMatchp(regex, L"\u0411\u0438\u043b\u043b", &matchLength) == 0);
    assert(matchLength == 4);
    assert(regexMatchp(regex, L"\u0431\u0438\u043b\u043b", &matchLength) == -1);
    regexFree(regex);
}

static void testUnicodeMajorCategoriesAndDigits(void) {
    Index matchLength;

    assert(regexMatch(L"^[[L]]+$", L"A\u3042\u0411", &matchLength, NULL,
                      NULL) == 0);
    assert(matchLength == 3);

    assert(regexMatch(L"^\\d+$", L"\u0661\u0662\u0663", &matchLength, NULL,
                      NULL) == 0);
    assert(matchLength == 3);
}

static void testClassNegationAndCategoryItems(void) {
    RegexStatus status;
    Index errorOffset;
    Index matchLength;
    Regex *regex = regexCompile(L"^[^[[Lu]]]+$", &status, &errorOffset);

    assert(regex != NULL);
    assert(regexMatchp(regex, L"abc\u3042", &matchLength) == 0);
    assert(matchLength == 4);
    assert(regexMatchp(regex, L"abcA", &matchLength) == -1);
    regexFree(regex);
}

static void testGroupingAlternationAndClosure(void) {
    RegexStatus status;
    Index errorOffset;
    Index matchLength;
    Regex *regex = regexCompile(L"^((ab)|c)+d$", &status, &errorOffset);

    assert(regex != NULL);
    assert(status == REGEX_STATUS_OK);
    assert(regexMatchp(regex, L"ababcd", &matchLength) == 0);
    assert(matchLength == 6);
    assert(regexMatchp(regex, L"cccd", &matchLength) == 0);
    assert(matchLength == 4);
    assert(regexMatchp(regex, L"abac", &matchLength) == -1);
    regexFree(regex);
}

static void testGroupedQuestionAndWildcard(void) {
    Index matchLength;

    assert(regexMatch(L"^(a.)?(bc)$", L"axbc", &matchLength, NULL, NULL) == 0);
    assert(matchLength == 4);
    assert(regexMatch(L"^(a.)?(bc)$", L"bc", &matchLength, NULL, NULL) == 0);
    assert(matchLength == 2);
    assert(regexMatch(L"^(a.)?(bc)$", L"ab", &matchLength, NULL, NULL) == -1);
}

static void testEscapedMetacharactersMatchLiterally(void) {
    Index matchLength;

    assert(regexMatch(L"^a\\+b\\*c\\?$", L"a+b*c?", &matchLength, NULL, NULL) ==
           0);
    assert(matchLength == 6);

    assert(regexMatch(L"^\\(a\\|b\\)$", L"(a|b)", &matchLength, NULL, NULL) ==
           0);
    assert(matchLength == 5);

    assert(regexMatch(L"^\\\\path$", L"\\path", &matchLength, NULL, NULL) == 0);
    assert(matchLength == 5);
}

static void testInvalidCategoryReportsOffset(void) {
    RegexStatus status;
    Index errorOffset;
    Regex *regex = regexCompile(L"[[Qx]]", &status, &errorOffset);

    assert(regex == NULL);
    assert(status == REGEX_STATUS_UNKNOWN_CATEGORY);
    assert(errorOffset == 2);
}

static void testUnterminatedGroupReportsError(void) {
    RegexStatus status;
    Index errorOffset;
    Regex *regex = regexCompile(L"(ab|c", &status, &errorOffset);

    assert(regex == NULL);
    assert(status == REGEX_STATUS_UNTERMINATED_GROUP);
    assert(errorOffset == 5);
}

static void testTrailingEscapeReportsError(void) {
    RegexStatus status;
    Index errorOffset;
    Regex *regex = regexCompile(L"abc\\", &status, &errorOffset);

    assert(regex == NULL);
    assert(status == REGEX_STATUS_TRAILING_ESCAPE);
    assert(errorOffset == 3);
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    testAsciiAndAnchors();
    testUnicodeCategoriesAsAtoms();
    testUnicodeMajorCategoriesAndDigits();
    testClassNegationAndCategoryItems();
    testGroupingAlternationAndClosure();
    testGroupedQuestionAndWildcard();
    testEscapedMetacharactersMatchLiterally();
    testInvalidCategoryReportsOffset();
    testUnterminatedGroupReportsError();
    testTrailingEscapeReportsError();
    return 0;
}