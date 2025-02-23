# Simple Unit Testing Strategy

SUT: `src/myfile.c`

```C
/* myfile.c */

#ifdef UNIT_TESTS
#include "tests/myfile.h"
#endif

/* rest of myfile.c */

#ifdef UNIT_TESTS
#include "tests/myfile.c"
#endif
```

Test header: `src/tests/myfile.h`

```C
...
bool test_myfile(void);
...
```

Test implementation: `src/tests/myfile.c`

```C
#include "tests/myfile.h"

bool test_myfile(void) {
...
}
...
```

Basically by including the test file at the end of the SUT we gain access
to any static functions declared in that compilation unit, and at the same
time we're not bloating the source file with tests.

`main.c` can include a `tests.h` that collects all the test headers and a
`tests.c` that will run all the tests. A binary with tests compiled in can
have an extra command-line option to run them.
