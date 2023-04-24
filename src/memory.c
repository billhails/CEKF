#include <stdlib.h>

#include "memory.h"

void *safe_malloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        exit(EXIT_OOM);
    }
    return ptr;
}
