#ifndef cekf_module_h
#define cekf_module_h

#include <stdio.h>
#include "ast.h"
#include "memory.h"

typedef struct PmModule {
    Header header;
	struct PmBufStack *bufStack;
    void *scanner;
	AstNest *nest;
} PmModule;

PmModule *newPmModuleFromFileHandle(FILE *f, const char *origin);
PmModule *newPmModuleFromStdin();
PmModule *new_PmModuleFromFile(const char *filename);
PmModule *new_PmModuleFromString(char *src, char *id);

PmModule *newPmToplevelFromFileHandle(FILE *f, const char *origin);
PmModule *newPmToplevelFromStdin();
PmModule *newPmToplevelFromFile(const char *filename);
PmModule *newPmToplevelFromString(char *src, char *id);

void markPmModule(Header *h);
void freePmModule(Header *h);
int pmParseModule(PmModule *mod);
int popPmFile(PmModule *mod);
void showModuleState(FILE *fp, PmModule *mod);

#endif
