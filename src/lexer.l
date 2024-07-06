%{
#include "parser.h"
#include "module.h"
#include "utf8.h"

void parseCS(char *text, char delimeter);
%}

%option nodefault
%option debug
%option reentrant
%option bison-bridge
%option noyywrap
%option extra-type="struct PmModule *"

/* 0x80 - 0x9F Control characters */
UNICODE_CONTROL \xC2[\x80-\x9F]

/* 0xA0 - 0xD7FF Normal characters */
UNICODE_CHARACTERS_1 \xC2[\xA0-\xBF]|[\xC3-\xDF][\x80-\xBF]|(\xE0[\xA0-\xBF]|\xED[\x80-\x9F]|[\xE1-\xEC][\x80-\xBF])[\x80-\xBF]

/* 0xD800 - 0xDFFF Reserved */
UNICODE_RESERVED_1 \xED[\xA0-\xBF][\x80-\xBF]

/* 0xE000 - 0xFFFD Normal Characters */
UNICODE_CHARACTERS_2 \xEF\xBF[\x80-\xBD]|(\xEF[\x80-\xBE]|\xEE[\x80-\xBF])[\x80-\xBF]

/* 0xFFFE 0xFFFF Reserved */
UNICODE_RESERVED_2 \xEF\xBF[\xBE-\xBF]

/* 0x1000 - 0x10FFFF Normal Characters */
UNICODE_CHARACTERS_3 (\xF0[\x90-\xBF]|\xF4[\x80-\x8F]|[\xF1-\xF3][\x80-\xBF])[\x80-\xBF][\x80-\xBF]

/* 0x80 - 0x10FFFF All high-bit Unicode */
UNICODE_ALL [\xC2-\xDF][\x80-\xBF]|(\xE0[\xA0-\xBF]|[\xE1-\xEF][\x80-\xBF])[\x80-\xBF]|(\xF0[\x90-\xBF]|\xF4[\x80-\x8F]|[\xF1-\xF3][\x80-\xBF])[\x80-\xBF][\x80-\xBF]

UNICODE_RESERVED {UNICODE_RESERVED_1}|{UNICODE_RESERVED_2}

UNICODE_CHARACTERS {UNICODE_CHARACTERS_1}|{UNICODE_CHARACTERS_2}|{UNICODE_CHARACTERS_3}

ALPHA [a-zA-Z_]
ALNUM [a-zA-Z_0-9]

ID ({ALPHA}|{UNICODE_CHARACTERS})({ALNUM}|{UNICODE_CHARACTERS})*

CODEPOINT {UNICODE_ALL}|.

%%

%{
struct PmModule *mod = yyextra;
%}

[ \t]+ {}
{UNICODE_CONTROL} {}
[\n] { incLineNo(mod); }
\/\/.* { }

{UNICODE_RESERVED} { cant_happen("invalid unicode character detected"); }

[0-9]*\.[0-9]+ { yylval->s = yytext; return IRRATIONAL; }
[0-9]*\.[0-9]+i { yylval->s = yytext; return IRRATIONAL_I; }
[0-9]+         { yylval->s = yytext; return NUMBER; }
[0-9]+i         { yylval->s = yytext; return NUMBER_I; }

"__namespace__" { return NAMESPACE_TOKEN; }
"namespace"     { return NAMESPACE; }
"as"            { return AS; }
"link"          { return LINK; }
"and"           { return AND; }
"back"          { return BACK; }
"char"          { return KW_CHAR; }
"else"          { return ELSE; }
"fn"            { return FN; }
"unsafe"        { return UNSAFE; }
"here"          { return HERE; }
"if"            { return IF; }
"in"            { return IN; }
"int"           { return KW_INT; }
"let"           { return LET; }
"not"           { return NOT; }
"or"            { return OR; }
"xor"           { return XOR; }
"print"         { return PRINT; }
"_"             { return WILDCARD; }
"switch"        { return SWITCH; }
"then"          { return THEN; }
"typedef"       { return TYPEDEF; }
"assert"        { return ASSERT; }
"alias"         { return ALIAS; }

\"((\\{CODEPOINT})|[^"])*\" { parseCS(yytext, '"'); yylval->s = yytext; return STRING; }
\'((\\{CODEPOINT})|[^'])\'  { parseCS(yytext, '\''); utf8_to_unicode_char(&yylval->c, yytext); return CHAR; }
\'({UNICODE_ALL}|[^'])\'    { parseCS(yytext, '\''); utf8_to_unicode_char(&yylval->c, yytext); return CHAR; }
\'\\u[0-9a-fA-F]+;\'        { parseCS(yytext, '\''); utf8_to_unicode_char(&yylval->c, yytext); return CHAR; }

"->" { return ARROW; }
"==" { return EQ; }
"!=" { return NE; }
">"  { return GT; }
"<"  { return LT; }
">=" { return GE; }
"<=" { return LE; }
"**" { return POW; }
"@"  { return CONS; }
"@@" { return APPEND; }

"<=>" { return CMP; }

{ID}  { yylval->s = yytext; return VAR; }
#{ID} { yylval->s = yytext; return TYPE_VAR; }

<<EOF>> { if (!popPmFile(mod)) yyterminate(); }

. { return yytext[0]; }

%%

void parseCS(char *text, char delimiter) {
    char *lead = text;
    char *follow = text;
    while (*lead != '\0') {
        if (*lead == delimiter) {
            lead++;
        } else if (*lead == '\\') {
            lead++;
            switch (*lead) {
                case 'n':
                    *follow = '\n';
                    break;
                case 't':
                    *follow = '\t';
                    break;
                case 'u': {
                    *lead++;
                    Character unicode = 0;
                    bool finished = false;
                    while (!finished) {
                        switch (*lead) {
                            case '0':
                                unicode <<= 4;
                                lead++;
                                break;
                            case '1':
                                unicode <<= 4;
                                unicode |= 0x01;
                                lead++;
                                break;
                            case '2':
                                unicode <<= 4;
                                unicode |= 0x02;
                                lead++;
                                break;
                            case '3':
                                unicode <<= 4;
                                unicode |= 0x03;
                                lead++;
                                break;
                            case '4':
                                unicode <<= 4;
                                unicode |= 0x04;
                                lead++;
                                break;
                            case '5':
                                unicode <<= 4;
                                unicode |= 0x05;
                                lead++;
                                break;
                            case '6':
                                unicode <<= 4;
                                unicode |= 0x06;
                                lead++;
                                break;
                            case '7':
                                unicode <<= 4;
                                unicode |= 0x07;
                                lead++;
                                break;
                            case '8':
                                unicode <<= 4;
                                unicode |= 0x08;
                                lead++;
                                break;
                            case '9':
                                unicode <<= 4;
                                unicode |= 0x09;
                                lead++;
                                break;
                            case 'a':
                            case 'A':
                                unicode <<= 4;
                                unicode |= 0x0A;
                                lead++;
                                break;
                            case 'b':
                            case 'B':
                                unicode <<= 4;
                                unicode |= 0x0B;
                                lead++;
                                break;
                            case 'c':
                            case 'C':
                                unicode <<= 4;
                                unicode |= 0x0C;
                                lead++;
                                break;
                            case 'd':
                            case 'D':
                                unicode <<= 4;
                                unicode |= 0x0D;
                                lead++;
                                break;
                            case 'e':
                            case 'E':
                                unicode <<= 4;
                                unicode |= 0x0E;
                                lead++;
                                break;
                            case 'f':
                            case 'F':
                                unicode <<= 4;
                                unicode |= 0x0F;
                                lead++;
                                break;
                            case ';':
                                finished = true;
                                break;
                            default:
                                cant_happen("error parsing unicode escape");
                        }
                    }
                    follow = writeChar(follow, unicode);
                    follow--;
                }
                break;
                default:
                    *follow = *lead;
            }
            follow++;
            lead++;
        } else {
            *(follow++) = *(lead++);
        }
    }
    *follow = '\0';
}
