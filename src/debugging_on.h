#ifndef cekf_debugging
#  define cekf_debugging
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
 */

static int _debugInvocationId __attribute__((unused)) = 0;
static bool _debuggingOn __attribute__((unused)) = true;
static int _debuggingDepth __attribute__((unused)) = 0;

#  define __DEBUGPAD__() do { for (int pads = _debuggingDepth / 4; pads > 0; pads--) { eprintf("   |"); } eprintf("%*s", _debuggingDepth % 4, ""); } while (false)

#  define DEBUG(...) do { \
    if (_debuggingOn) { \
        eprintf("%s:%-5d", __FILE__, __LINE__); \
        __DEBUGPAD__(); \
        eprintf(__VA_ARGS__); \
        eprintf("\n"); \
    } \
} while(0)

#  define DEBUGN(...) do { \
    if (_debuggingOn) { \
        eprintf("%s:%-5d", __FILE__, __LINE__); \
        __DEBUGPAD__(); \
        eprintf(__VA_ARGS__); \
    } \
} while(0)

#  define ENTER(name) int _debugMyId = _debugInvocationId++; DEBUG("ENTER " #name " #%d", _debugMyId); _debuggingDepth++

#  define LEAVE(name) _debuggingDepth--; DEBUG("LEAVE " #name " #%d", _debugMyId)

#  define NEWLINE() do { if (_debuggingOn) eprintf("\n"); } while(0)

#  define IFDEBUG(x) do { if (_debuggingOn) { eprintf("%s:%-5d", __FILE__, __LINE__); __DEBUGPAD__(); x; NEWLINE(); } } while(0)

#  define IFDEBUGN(x) do { if (_debuggingOn) { x; NEWLINE(); } } while(0)

#  define DEBUGGING_ON() do { _debuggingOn = true; } while (0)

#  define DEBUGGING_OFF() do { _debuggingOn = false; } while (0)

#endif
