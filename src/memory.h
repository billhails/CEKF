#ifndef cekf_memory_h
#define cekf_memory_h

#include <stddef.h>

void *safe_malloc(size_t size);

#define EXIT_OOM 2

#define NEW(thing) ((thing *)safe_malloc(sizeof(thing)))

#endif
