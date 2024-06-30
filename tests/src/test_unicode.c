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

#include "test.h"
#include "utf8.h"

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    unsigned char bytes[128];
    wchar_t character;
#define TEST(c, s) do { \
        printf("%x %d\n", c, s); \
        character = c; \
        assert(byteSize(character) == s); \
        char *ptr = writeChar(bytes, character); \
        *ptr = 0; \
        assert(strlen(bytes) == s); \
        assert(utf8_len(bytes) == 1); \
        character = 0; \
        utf8_to_unicode_char(&character, bytes); \
        assert(character == c); \
    } while(0);
    TEST(0x1, 1);
    TEST(0x7f, 1);
    TEST(0x80, 2);
    TEST(0x7FF, 2);
    TEST(0x800, 3);
    TEST(0xFFFF, 3);
    TEST(0x10000, 4);
    TEST(0x10FFFF, 4);
}
