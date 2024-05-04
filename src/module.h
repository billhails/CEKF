#ifndef cekf_module_h
#  define cekf_module_h

#  include <stdio.h>
#  include "common.h"
#  include "ast.h"

typedef struct PmModule {
    struct PmBufStack *bufStack;
    void *scanner;
    AstNest *nest;
} PmModule;

AstDefinitions *parseNameSpaceFromString(const char *namespace, const char *origin);
AstDefinitions *parseNameSpaceFromFileHandle(FILE *f, const char *origin);
AstDefinitions *parseNameSpaceFromFileName(const char *fileName);
AstNest *parseTopLevelFromFileHandle(FILE *f, const char *origin);
AstNest *parseTopLevelFromFileName(const char *fileName);
AstNest *parseTopLevelFromString(const char *string, const char *origin);
AstNest *parseSingleString(char *string, char *origin);

void incLineNo(PmModule *mod);
int popPmFile(PmModule *mod);
void showModuleState(FILE *fp, PmModule *mod);
char *currentPmFile(PmModule *mod);

#endif
