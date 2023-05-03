%{
#include <stdlib.h> /* for atoi call */
#include "parser.h"
#include "common.h"
extern int lineNum;

%}

%%

[ \t]+ {}

[\n] { lineNum++; }

"(" {
    return '(';
}

")" {
    return ')';
}

"+" {
    return '+';
}

"*" {
    return '*';
}

[0-9]+ {
    yylval.d = atoi(yytext);
    return NUMBER;
}

%%

int yywrap() { /* need this to avoid link problem */
    return 1;
}
