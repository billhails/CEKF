%{
#include <stdlib.h> /* for atoi call */
#define DEBUG /* for debuging: print tokens and their line numbers */
#define NUMBER 258 /* copy this from cal.tab.c */
typedef union { /* copy this from cal.tab.c */
    int d;
} YYSTYPE;
extern YYSTYPE yylval; /* for passing value to parser */
extern int lineNum; /* line number from cal.tab.c */
%}

%%

[ \t]+ {}

[\n] { lineNum++; }

"(" {
#ifdef DEBUG
    printf("token '(' at line %d\n", lineNum);
#endif
    return '(';
}

")" {
#ifdef DEBUG
    printf("token ')' at line %d\n", lineNum);
#endif
    return ')';
}

"+" {
#ifdef DEBUG
    printf("token '+' at line %d\n", lineNum);
#endif
    return '+';
}

"*" {
#ifdef DEBUG
    printf("token '*' at line %d\n", lineNum);
#endif
    return '*';
}

[0-9]+ {
#ifdef DEBUG
    printf("token %s at line %d\n", yytext, lineNum);
#endif
    yylval.d = atoi(yytext);
    return NUMBER;
}

%%

int yywrap() { /* need this to avoid link problem */
    return 1;
}
