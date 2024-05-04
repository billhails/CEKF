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

PmModule *newPmModuleFromFileHandle(FILE *f, const char *origin);
PmModule *newPmModuleFromStdin(void);
PmModule *newPmModuleFromFile(const char *filename);
PmModule *newPmModuleFromString(char *src, char *id);

PmModule *newPmToplevelFromFileHandle(FILE *f, const char *origin);
PmModule *newPmNameSpaceFromFileHandle(FILE *f, const char *origin);
PmModule *newPmToplevelFromStdin(void);
PmModule *newPmToplevelFromFile(const char *filename);
PmModule *newPmToplevelFromString(char *src, char *id);

void freePmModule(PmModule *mod);
int pmParseModule(PmModule *mod);
void incLineNo(PmModule *mod);
int popPmFile(PmModule *mod);
void showModuleState(FILE *fp, PmModule *mod);
char *currentPmFile(PmModule *mod);

#endif
