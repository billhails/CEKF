#include "test.h"

#include "bigint.h"
#include "common.h"
#include "init.h"
#include "pratt_scanner.h"

#include <math.h>
#include <string.h>

#ifdef SAFETY_CHECKS
extern int forceGcFlag;
#endif

static PrattParser *makeScannerParser(char *input) {
    PrattLexer *lexer = makePrattLexerFromMbString(input, "test_pratt_scanner");
    int save = PROTECT(lexer);
    PrattParser *parser = newPrattParser(NULL);
    PROTECT(parser);
    parser->lexer = lexer;
    UNPROTECT(save);
    return parser;
}

static PrattToken *scanSingleToken(char *input) {
    int save = STARTPROTECT();
    PrattParser *parser = makeScannerParser(input);
    PROTECT(parser);

    clearErrors();
    PrattToken *token = next(parser);
    PROTECT(token);
    PrattToken *eof = next(parser);
    PROTECT(eof);

    assert(token->type == TOK_NUMBER());
    assert(token->value != NULL);
    assert(token->value->type == PRATTVALUE_TYPE_NUMBER);
    assert(eof->type == TOK_EOF());
    assert(!hadErrors());

    UNPROTECT(save);
    return token;
}

static void assertSmallNumberToken(char *input, int expected, bool imag) {
    int save = STARTPROTECT();
    PrattToken *token = scanSingleToken(input);
    PROTECT(token);

    MaybeBigInt *number = getPrattValue_Number(token->value);
    assert(number->type == BI_SMALL);
    assert(number->small == expected);
    assert(number->imag == imag);

    UNPROTECT(save);
}

static void assertIrrationalToken(char *input, Double expected, bool imag) {
    int save = STARTPROTECT();
    PrattToken *token = scanSingleToken(input);
    PROTECT(token);

    MaybeBigInt *number = getPrattValue_Number(token->value);
    assert(number->type == BI_IRRATIONAL);
    assert(fabs(number->irrational - expected) < 1e-12);
    assert(number->imag == imag);

    UNPROTECT(save);
}

static void assertPrintedNumberToken(char *input, const char *expected) {
    int save = STARTPROTECT();
    PrattToken *token = scanSingleToken(input);
    PROTECT(token);

    MaybeBigInt *number = getPrattValue_Number(token->value);
    char actual[256];

    sprintMaybeBigInt(actual, number);
    assert(strcmp(actual, expected) == 0);

    UNPROTECT(save);
}

static void testDecimalNumber(void) {
    assertSmallNumberToken("123", 123, false);
}

static void testFloatWithTrailingDot(void) {
    assertIrrationalToken("1.", 1.0, false);
}

static void testHexadecimalNumber(void) {
    assertSmallNumberToken("0xff", 255, false);
    assertSmallNumberToken("0XFF", 255, false);
}

static void testImaginaryNumbers(void) {
    assertSmallNumberToken("7i", 7, true);
    assertPrintedNumberToken("0x_afa_e20d_cab2_6000i", "791193233420083200i");
}

static void testUnicodeDecimalDigits(void) {
    assertSmallNumberToken("\u0661\u0662\u0663", 123, false);
}

static void testPermissiveEdgeCases(void) {
    assertSmallNumberToken("0x", 0, false);
    assertSmallNumberToken("1__2", 12, false);
}

static void testCachedNumericRegexesSurviveForcedGc(void) {
    assertSmallNumberToken("123", 123, false);
    assertSmallNumberToken("0xff", 255, false);

#ifdef SAFETY_CHECKS
    // Keep forced GC scoped to the cached scans under test; enabling it for the
    // whole binary makes this test suite much slower.
    int previousForceGcFlag = forceGcFlag;
    forceGcFlag = 1;
#endif

    assertSmallNumberToken("456", 456, false);
    assertSmallNumberToken("0x10", 16, false);

#ifdef SAFETY_CHECKS
    forceGcFlag = previousForceGcFlag;
#endif
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    initAll();

    testDecimalNumber();
    testFloatWithTrailingDot();
    testHexadecimalNumber();
    testImaginaryNumbers();
    testUnicodeDecimalDigits();
    testPermissiveEdgeCases();
    testCachedNumericRegexesSurviveForcedGc();

    return 0;
}