%{
#include "parser.h"
#include "module.h"

void parseCS(char *text, char delimeter);
%}

%option nodefault
%option debug
%option reentrant
%option bison-bridge
%option noyywrap
%option extra-type="struct PmModule *"

ID [a-zA-Z_][a-zA-Z_0-9]*

%%

%{
struct PmModule *mod = yyextra;
%}

[ \t]+ {}
[\n] {}
\/\/.* { }

-?[0-9]+ { yylval->s = yytext; return NUMBER; }

"and"       { return AND; }
"as"        { return AS; }
"back"      { return BACK; }
"char"      { return KW_CHAR; }
"else"      { return ELSE; }
"env"       { return ENV; }
"extends"   { return EXTENDS; }
"fn"        { return FN; }
"here"      { return HERE; }
"if"        { return IF; }
"in"        { return IN; }
"int"       { return KW_INT; }
"let"       { return LET; }
"load"      { return LOAD; }
"not"       { return NOT; }
"or"        { return OR; }
"xor"       { return XOR; }
"prototype" { return PROTOTYPE; }
"_"         { return WILDCARD; }
"switch"    { return SWITCH; }
"then"      { return THEN; }
"typedef"   { return TYPEDEF; }

\"((\\.)|[^"])*\" { parseCS(yytext, '"'); yylval->s = yytext; return STRING; }
\'((\\.)|[^'])\'  { parseCS(yytext, '\''); yylval->c = *yytext; return CHAR; }

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
