#ifndef cekf_module_h
#  define cekf_module_h

#  include <stdio.h>
#  include "common.h"
#  include "ast.h"

typedef struct PmModule {
    struct PmBufStack *bufStack;
    void *scanner;
    AstNest *nest;
    AstIntTable *namespaces;
} PmModule;

#define PIM(m) ((struct ParserInfo){.filename = currentPmFile(m), .lineno = currentPmLine(m)})

AstDefinitions *parseNamespaceFromString(const char *namespace, const char *origin);
AstDefinitions *parseNamespaceFromFileHandle(FILE *f, const char *origin);
AstDefinitions *parseNamespaceFromFileName(const char *fileName);
AstNest *parseTopLevelFromFileHandle(FILE *f, const char *origin);
AstNest *parseTopLevelFromFileName(const char *fileName);
AstNest *parseTopLevelFromString(const char *string, const char *origin);
AstNest *parseSingleString(char *string, char *origin);

void incLineNo(PmModule *mod);
int popPmFile(PmModule *mod);
void showModuleState(FILE *fp, PmModule *mod);
char *currentPmFile(PmModule *mod);
int currentPmLine(PmModule *mod);

#endif
