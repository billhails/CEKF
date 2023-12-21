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

static int _debugInvocationId = 0;
#define DEBUG(...) do { \
    eprintf("**** %s:%-5d ", __FILE__, __LINE__); \
    eprintf(__VA_ARGS__); eprintf("\n"); \
} while(0)
#define ENTER(name) int _debugMyId = _debugInvocationId++; \
    DEBUG("ENTER " #name " #%d", _debugMyId)
#define LEAVE(name) DEBUG("LEAVE " #name " #%d", _debugMyId)
#define NEWLINE() eprintf("\n")
#define IFDEBUG(x) do { x; NEWLINE(); } while(0)
