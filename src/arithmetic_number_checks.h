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

#ifndef cekf_arithmetic_number_checks_h
#define cekf_arithmetic_number_checks_h

#include "common.h"
#include "value.h"

#define IS_COMPLEX(x) ((x).type == VALUE_TYPE_COMPLEX)
#define IS_BIGINT(x) ((x).type == VALUE_TYPE_BIGINT)
#define IS_BIGINT_IMAG(x) ((x).type == VALUE_TYPE_BIGINT_IMAG)
#define IS_IRRATIONAL(x) ((x).type == VALUE_TYPE_IRRATIONAL)
#define IS_RATIONAL(x) ((x).type == VALUE_TYPE_RATIONAL)
#define IS_STDINT(x) ((x).type == VALUE_TYPE_STDINT)
#define IS_STDINT_IMAG(x) ((x).type == VALUE_TYPE_STDINT_IMAG)
#define IS_INT(x) (IS_STDINT(x) || IS_BIGINT(x))
#define IS_IMAGINT(x) (IS_STDINT_IMAG(x) || IS_BIGINT_IMAG(x))
#define IS_RATIONAL_OR_INT(x) (IS_RATIONAL(x) || IS_INT(x))
#define IS_REAL(x) (IS_RATIONAL_OR_INT(x) || IS_IRRATIONAL(x))
#define IS_NOT_REAL(x) (!IS_REAL(x))

#define IS_IMAG_TYPE(type)                                                     \
    ((type) == VALUE_TYPE_STDINT_IMAG || (type) == VALUE_TYPE_BIGINT_IMAG ||   \
     (type) == VALUE_TYPE_RATIONAL_IMAG ||                                     \
     (type) == VALUE_TYPE_IRRATIONAL_IMAG)

#define IS_COMPLEX_LIKE_TYPE(type)                                             \
    ((type) == VALUE_TYPE_COMPLEX || IS_IMAG_TYPE(type))

#ifdef SAFETY_CHECKS
#define ASSERT_COMPLEX(x) ASSERT(IS_COMPLEX(x))
#define ASSERT_RATIONAL(x) ASSERT(IS_RATIONAL(x))
#define ASSERT_IRRATIONAL(x) ASSERT(IS_IRRATIONAL(x))
#define ASSERT_BIGINT(x) ASSERT(IS_BIGINT(x))
#define ASSERT_STDINT(x) ASSERT(IS_STDINT(x))
#define ASSERT_INT(x) ASSERT(IS_INT(x))
#define ASSERT_RATIONAL_OR_INT(x) ASSERT(IS_RATIONAL_OR_INT(x))
#define ASSERT_REAL(x) ASSERT(IS_REAL(x))
#define ASSERT_NOT_REAL(x) ASSERT(IS_NOT_REAL(x))
#else
#define ASSERT_COMPLEX(x)
#define ASSERT_RATIONAL(x)
#define ASSERT_IRRATIONAL(x)
#define ASSERT_BIGINT(x)
#define ASSERT_STDINT(x)
#define ASSERT_INT(x)
#define ASSERT_RATIONAL_OR_INT(x)
#define ASSERT_REAL(x)
#define ASSERT_NOT_REAL(x)
#endif

#endif