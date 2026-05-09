#!/usr/bin/env python3
#
# CEKF - VM supporting amb
# Copyright (C) 2022-2024  Bill Hails
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import argparse
import csv
from dataclasses import dataclass

UNICODE_TABLE_SIZE = 0x110000
PAGE_BITS = 8
PAGE_SIZE = 1 << PAGE_BITS
PAGE_COUNT = UNICODE_TABLE_SIZE >> PAGE_BITS

GC_L = 0x00
GC_M = 0x01
GC_N = 0x02
GC_P = 0x03
GC_S = 0x04
GC_Z = 0x05
GC_C = 0x06


def gc(base, ordinal):
    return base | (ordinal << 3)


CATEGORY_VALUES = {
    "Ll": gc(GC_L, 0),
    "Lm": gc(GC_L, 1),
    "Lo": gc(GC_L, 2),
    "Lt": gc(GC_L, 3),
    "Lu": gc(GC_L, 4),
    "Mc": gc(GC_M, 0),
    "Me": gc(GC_M, 1),
    "Mn": gc(GC_M, 2),
    "Nd": gc(GC_N, 0),
    "Nl": gc(GC_N, 1),
    "No": gc(GC_N, 2),
    "Pc": gc(GC_P, 0),
    "Pd": gc(GC_P, 1),
    "Pe": gc(GC_P, 2),
    "Pf": gc(GC_P, 3),
    "Pi": gc(GC_P, 4),
    "Po": gc(GC_P, 5),
    "Ps": gc(GC_P, 6),
    "Sc": gc(GC_S, 0),
    "Sk": gc(GC_S, 1),
    "Sm": gc(GC_S, 2),
    "So": gc(GC_S, 3),
    "Zl": gc(GC_Z, 0),
    "Zp": gc(GC_Z, 1),
    "Zs": gc(GC_Z, 2),
    "Cc": gc(GC_C, 0),
    "Cf": gc(GC_C, 1),
    "Co": gc(GC_C, 2),
    "Cs": gc(GC_C, 3),
    "Cn": gc(GC_C, 4),
}
DEFAULT_CATEGORY = CATEGORY_VALUES["Cn"]
CATEGORY_NAMES = {value: f"GC_{key}" for key, value in CATEGORY_VALUES.items()}


@dataclass(frozen=True)
class SpecialCasing:
    lower: tuple
    title: tuple
    upper: tuple


@dataclass
class UnicodeTables:
    categories: bytearray
    decimalValues: dict
    numericValues: dict
    simpleCaseDeltas: dict
    specialCasing: dict
    unicodeDataPath: str
    specialCasingPath: str


def parseArgs(defaultEmit):
    parser = argparse.ArgumentParser(description="Generate CEKF Unicode lookup data.")
    parser.add_argument(
        "unicodeData",
        nargs="?",
        default="unicode/UnicodeData.txt",
        help="Path to UnicodeData.txt",
    )
    parser.add_argument(
        "specialCasing",
        nargs="?",
        default="unicode/SpecialCasing.txt",
        help="Path to SpecialCasing.txt",
    )
    parser.add_argument(
        "--emit",
        choices=("categories", "digits", "casing", "numbers"),
        default=defaultEmit,
        help="Generated output to emit",
    )
    return parser.parse_args()


def parseCodeSequence(field):
    stripped = field.strip()
    if not stripped:
        return ()
    return tuple(int(code, 16) for code in stripped.split())


def parseCaseDelta(code, field):
    stripped = field.strip()
    if not stripped:
        return 0
    return int(stripped, 16) - code


def parseNumericValue(field):
    stripped = field.strip()
    if not stripped:
        return None
    if "/" in stripped:
        return stripped
    return int(stripped)


def loadSpecialCasing(path):
    mappings = {}

    with open(path, encoding="utf-8") as handle:
        for rawLine in handle:
            line = rawLine.split("#", 1)[0].strip()
            if not line:
                continue

            fields = [field.strip() for field in line.split(";")]
            if len(fields) < 5:
                raise ValueError(f"Malformed SpecialCasing.txt line: {rawLine.rstrip()}")

            if fields[4]:
                continue

            code = int(fields[0], 16)
            mappings[code] = SpecialCasing(
                lower=parseCodeSequence(fields[1]),
                title=parseCodeSequence(fields[2]),
                upper=parseCodeSequence(fields[3]),
            )

    return mappings


def applyCategoryRange(categories, start, end, category):
    categories[start : end + 1] = bytes([category]) * (end - start + 1)


def loadUnicodeTables(unicodeDataPath, specialCasingPath):
    categories = bytearray([DEFAULT_CATEGORY]) * UNICODE_TABLE_SIZE
    decimalValues = {}
    numericValues = {}
    simpleCaseDeltas = {}
    specialCasing = loadSpecialCasing(specialCasingPath)

    with open(unicodeDataPath, newline="", encoding="utf-8") as csvin:
        reader = csv.reader(csvin, delimiter=";")
        for row in reader:
            if not row:
                continue

            code = int(row[0], 16)
            name = row[1]
            category = CATEGORY_VALUES[row[2]]

            if name.endswith("First>"):
                endRow = next(reader, None)
                if endRow is None:
                    raise ValueError(f"Missing range terminator for {row[0]}")
                endCode = int(endRow[0], 16)
                applyCategoryRange(categories, code, endCode, category)
                continue

            if name.endswith("Last>"):
                continue

            categories[code] = category

            if row[6]:
                decimalValues[code] = int(row[6])

            numericValue = parseNumericValue(row[8])
            if numericValue is not None:
                numericValues[code] = numericValue

            upper = parseCaseDelta(code, row[12])
            lower = parseCaseDelta(code, row[13])
            title = parseCaseDelta(code, row[14])
            if upper or lower or title:
                simpleCaseDeltas[code] = (upper, lower, title)

    return UnicodeTables(
        categories=categories,
        decimalValues=decimalValues,
        numericValues=numericValues,
        simpleCaseDeltas=simpleCaseDeltas,
        specialCasing=specialCasing,
        unicodeDataPath=unicodeDataPath,
        specialCasingPath=specialCasingPath,
    )


def buildPages(categories):
    pageCache = {}
    pages = []
    pageIndex = []

    for pageNumber in range(PAGE_COUNT):
        start = pageNumber * PAGE_SIZE
        page = bytes(categories[start : start + PAGE_SIZE])
        index = pageCache.get(page)
        if index is None:
            index = len(pages)
            pageCache[page] = index
            pages.append(page)
        pageIndex.append(index)

    return pageIndex, pages


def unsignedType(maxValue):
    if maxValue <= 0xFF:
        return "uint8_t"
    if maxValue <= 0xFFFF:
        return "uint16_t"
    return "uint32_t"


def emitIntegers(values, width, indent):
    for offset in range(0, len(values), width):
        chunk = values[offset : offset + width]
        line = ", ".join(str(value) for value in chunk)
        print(f"{indent}{line},")


def emitQuotedStrings(values, width, indent):
    for offset in range(0, len(values), width):
        chunk = values[offset : offset + width]
        line = ", ".join(valuesToCString(value) for value in chunk)
        print(f"{indent}{line},")


def valuesToCString(value):
    return f'"{value}"' if value is not None else "NULL"


def emitCategoryPage(page):
    print("    {")
    for offset in range(0, PAGE_SIZE, 16):
        chunk = page[offset : offset + 16]
        line = ", ".join(CATEGORY_NAMES[value] for value in chunk)
        print(f"        {line},")
    print("    },")


def sequenceNeedsStorage(code, sequence, delta):
    if not sequence:
        return False
    if len(sequence) != 1:
        return True
    return sequence[0] != code + delta


def packSequence(sequence, pool, offsets):
    offset = offsets.get(sequence)
    if offset is not None:
        return offset

    offset = len(pool)
    offsets[sequence] = offset
    pool.append(len(sequence))
    pool.extend(sequence)
    return offset


def emitCategories(tables):
    pageIndex, pages = buildPages(tables.categories)
    pageIndexType = unsignedType(len(pages) - 1)

    print("/* Generated by tools/unicode_table.py")
    print(f" * Source: {tables.unicodeDataPath}")
    print(f" * Source: {tables.specialCasingPath}")
    print(f" * Unique category pages: {len(pages)}")
    print(" */")
    print("")
    print(
        f"static const {pageIndexType} unicodeCategoryPageIndex[{len(pageIndex)}] = {{"
    )
    emitIntegers(pageIndex, 16, "    ")
    print("};")
    print("")
    print(
        f"static const unsigned char unicodeCategoryPages[{len(pages)}][{PAGE_SIZE}] = {{"
    )
    for page in pages:
        emitCategoryPage(page)
    print("};")


def emitDigits(tables):
    digits = sorted(tables.decimalValues.items())

    print("#define UNICODE_DIGITS_CASES \\")
    for code, value in digits:
        print(f"    case 0x{code:04X}: return {value}; \\")
    print("    default: return -1;")
    print(f"#define NUM_UNICODE_DIGITS {len(digits)}")


def emitCasing(tables):
    specialCasePool = [0]
    specialCaseOffsets = {}
    records = []
    allCodes = sorted(set(tables.simpleCaseDeltas) | set(tables.specialCasing))

    for code in allCodes:
        upperDelta, lowerDelta, titleDelta = tables.simpleCaseDeltas.get(code, (0, 0, 0))
        special = tables.specialCasing.get(code, SpecialCasing((), (), ()))

        upperSpecial = 0
        lowerSpecial = 0
        titleSpecial = 0

        if sequenceNeedsStorage(code, special.upper, upperDelta):
            upperSpecial = packSequence(special.upper, specialCasePool, specialCaseOffsets)

        if sequenceNeedsStorage(code, special.lower, lowerDelta):
            lowerSpecial = packSequence(special.lower, specialCasePool, specialCaseOffsets)

        if sequenceNeedsStorage(code, special.title, titleDelta):
            titleSpecial = packSequence(special.title, specialCasePool, specialCaseOffsets)

        if upperDelta or lowerDelta or titleDelta or upperSpecial or lowerSpecial or titleSpecial:
            records.append(
                (
                    code,
                    upperDelta,
                    lowerDelta,
                    titleDelta,
                    upperSpecial,
                    lowerSpecial,
                    titleSpecial,
                )
            )

    print("/* Generated by tools/unicode_table.py */")
    print("typedef struct {")
    print("    uint32_t code;")
    print("    int32_t upperDelta;")
    print("    int32_t lowerDelta;")
    print("    int32_t titleDelta;")
    print("    uint32_t upperSpecial;")
    print("    uint32_t lowerSpecial;")
    print("    uint32_t titleSpecial;")
    print("} UnicodeCaseEntry;")
    print("")
    print(f"static const uint32_t unicodeSpecialCaseData[{len(specialCasePool)}] = {{")
    emitIntegers(specialCasePool, 12, "    ")
    print("};")
    print("")
    print(f"static const UnicodeCaseEntry unicodeCaseEntries[{len(records)}] = {{")
    for code, upperDelta, lowerDelta, titleDelta, upperSpecial, lowerSpecial, titleSpecial in records:
        print(
            "    {"
            f"0x{code:04X}, {upperDelta}, {lowerDelta}, {titleDelta}, "
            f"{upperSpecial}, {lowerSpecial}, {titleSpecial}"
            "},"
        )
    print("};")
    print(f"static const uint32_t unicodeCaseEntryCount = {len(records)};")


def emitNumbers(tables):
    codes = sorted(set(tables.decimalValues) | set(tables.numericValues))

    print("/* Generated by tools/unicode_table.py */")
    print("typedef struct {")
    print("    uint32_t code;")
    print("    int32_t decimalValue;")
    print("    int64_t integerValue;")
    print("    const char *fractionValue;")
    print("    uint8_t hasIntegerValue;")
    print("} UnicodeNumberEntry;")
    print("")
    print(f"static const UnicodeNumberEntry unicodeNumberEntries[{len(codes)}] = {{")
    for code in codes:
        decimalValue = tables.decimalValues.get(code, -1)
        numericValue = tables.numericValues.get(code)
        integerValue = 0
        fractionValue = None
        hasIntegerValue = 0

        if isinstance(numericValue, int):
            integerValue = numericValue
            hasIntegerValue = 1
        elif isinstance(numericValue, str):
            fractionValue = numericValue

        print(
            "    {"
            f"0x{code:04X}, {decimalValue}, {integerValue}, {valuesToCString(fractionValue)}, {hasIntegerValue}"
            "},"
        )
    print("};")
    print(f"static const uint32_t unicodeNumberEntryCount = {len(codes)};")


def emit(tables, kind):
    if kind == "categories":
        emitCategories(tables)
        return
    if kind == "digits":
        emitDigits(tables)
        return
    if kind == "casing":
        emitCasing(tables)
        return
    if kind == "numbers":
        emitNumbers(tables)
        return
    raise ValueError(f"Unsupported emit kind: {kind}")


def main(defaultEmit="categories"):
    args = parseArgs(defaultEmit)
    tables = loadUnicodeTables(args.unicodeData, args.specialCasing)
    emit(tables, args.emit)


if __name__ == "__main__":
    main()