%{
#include <stdlib.h> /* for atoi call */
#include "ast.h"
#include "parser.h"
#include "parser_management.h"
extern int lineNum;

void parseCS(char delimeter);
%}

%option nodefault
%option noyywrap

ID [a-zA-Z_][a-zA-Z_0-9]*

%%

[ \t]+ {}
[\n] { lineNum++; }
\/\/.* { }

[0-9]+ {
    yylval.i = atoi(yytext);
    return NUMBER;
}

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
"list"      { return KW_LIST; }
"load"      { return LOAD; }
"not"       { return NOT; }
"or"        { return OR; }
"prototype" { return PROTOTYPE; }
"_"         { return WILDCARD; }
"string"    { return KW_STRING; }
"switch"    { return SWITCH; }
"then"      { return THEN; }
"typedef"   { return TYPEDEF; }

\"((\\.)|[^"])*\" { parseCS('"'); yylval.s = yytext; return STRING; }
\'((\\.)|[^'])\'  { parseCS('\''); yylval.c = *yytext; return CHAR; }

"->" { return ARROW; }
"==" { return EQ; }
"!=" { return NE; }
">"  { return GT; }
"<"  { return LT; }
">=" { return GE; }
"<=" { return LE; }
"@"  { return CONS; }
"@@" { return APPEND; }

-?[0-9]+ { yylval.i = atoi(yytext); return NUMBER; }

{ID}  { yylval.s = yytext; return VAR; }
#{ID} { yylval.s = yytext; return TYPE_VAR; }

. { return yytext[0]; }

<<EOF>> { if (!pm_popFile()) yyterminate(); }
%%

void parseCS(char delimiter) {
    char *lead = yytext;
    char *follow = yytext;
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
