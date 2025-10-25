/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2023  Bill Hails
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * common error handling.
 */


#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <unistd.h>
#include <stdlib.h>

#include "common.h"

static bool errors = false;

/**
 * Handle a "can't happen" error by printing message and exiting.
 * Used by the `cant_happen` macro from common.h.
 * 
 * @param file the source file name (supplied by the macro)
 * @param line the source line number (supplied by the macro)
 * @param message the error message format string
 * @param ... the error message arguments
 * @return does not return
 */
void _cant_happen(char *file, int line, const char *message, ...) {
    va_list args;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    eprintf(" at +%d %s\n", line, file);
#ifdef DEBUG_DUMP_CORE
    abort();
#else
    exit(1);
#endif
}

/**
 * Handle a "can happen" error by printing message and setting error flag.
 * 
 * @param message the error message format string
 * @param ... the error message arguments
 */
void can_happen(const char *message, ...) {
    va_list args;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
    eprintf("\n");
    errors = true;
}

/**
 * Print a message to the error output.
 * 
 * @param message the message format string
 * @param ... the message arguments
 */
void eprintf(const char *message, ...) {
    va_list args;
    va_start(args, message);
    vfprintf(errout, message, args);
    va_end(args);
}

/**
 * Check if any errors have occurred.
 * 
 * @return true if errors have occurred, false otherwise
 */
bool hadErrors() {
    return errors;
}

/**
 * Clear the error flag.
 */
void clearErrors() {
    errors = false;
}
