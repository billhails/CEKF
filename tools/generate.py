#!/usr/bin/env python3
#
# CEKF - VM supporting amb
# Copyright (C) 2022-2025  Bill Hails
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
#

"""
makeAST - Code generator for C structures from YAML schemas

This script uses the refactored generate package to generate C code from YAML schemas.
All classes have been migrated into focused modules under tools/generate/.
"""

import sys
import os
import argparse

# Import from the new generate package
from generate.catalog import Catalog
from generate.primitives import Primitive
from generate.hashes import SimpleHash
from generate.simple_enum import SimpleEnum
from generate.simple_array import SimpleArray
from generate.simple_stack import SimpleStack
from generate.inline_array import InlineArray
from generate.vectors import SimpleVector
from generate.simple_struct import SimpleStruct
from generate.inline_struct import InlineStruct
from generate.discriminated_union import DiscriminatedUnion
from generate.inline_discriminated_union import InlineDiscriminatedUnion
from generate.discriminated_union_union import DiscriminatedUnionUnion

from generate.loader import Loader
from generate.utils import printGpl, printSection


def main():
    parser = argparse.ArgumentParser(description='Generate C code from YAML schemas')
    parser.add_argument("yaml", help="input yaml file")
    parser.add_argument("type",
                        type=str,
                        choices=["h", "c", "objtypes_h", "debug_h", "debug_c", "md", "visitor", "kont_impl_inc"],
                        help="the type of output to produce")
    parser.add_argument("--suffix",
                        type=str,
                        default="",
                        required=False,
                        help="suffix (visitor only)")
    args = parser.parse_args()

    import yaml
    stream = open(args.yaml, 'r')
    document = yaml.load(stream, Loader)

    typeName = document['config']['name']
    description = document['config']['description']
    includes = document['config'].get('includes', [])
    limited_includes = document['config'].get('limited_includes', [])
    parserInfo = document['config'].get('parserInfo', False)

    catalog = Catalog(typeName)

    if parserInfo:
        catalog.noteParserInfo()

    # Build catalog from YAML document
    if "hashes" in document:
        for name in document["hashes"]:
            catalog.add(SimpleHash(name, document["hashes"][name]))

    if "structs" in document:
        for name in document["structs"]:
            catalog.add(SimpleStruct(name, document["structs"][name]))

    if "vectors" in document:
        for name in document["vectors"]:
            catalog.add(SimpleVector(name, document["vectors"][name]))

    if "inline" in document:
        if "structs" in document["inline"]:
            for name in document["inline"]["structs"]:
                catalog.add(InlineStruct(name, document["inline"]["structs"][name]))
        if "unions" in document["inline"]:
            for name in document["inline"]["unions"]:
                catalog.add(InlineDiscriminatedUnion(name, document["inline"]["unions"][name]))
        if "arrays" in document["inline"]:
            for name in document["inline"]["arrays"]:
                catalog.add(InlineArray(name, document["inline"]["arrays"][name]))

    if "unions" in document:
        for name in document["unions"]:
            catalog.add(DiscriminatedUnion(name, document["unions"][name]))

    if "stacks" in document:
        for name in document["stacks"]:
            catalog.add(SimpleStack(name, document["stacks"][name]))

    if "enums" in document:
        for name in document["enums"]:
            catalog.add(SimpleEnum(name, document["enums"][name]))

    if "primitives" in document:
        for name in document["primitives"]:
            catalog.add(Primitive(name, document["primitives"][name]))

    if "external" in document:
        for name in document["external"]:
            catalog.add(Primitive(name, document["external"][name]))

    if "arrays" in document:
        for name in document["arrays"]:
            catalog.add(SimpleArray(name, document["arrays"][name]))

    if "tags" in document:
        for tag in document["tags"]:
            catalog.tag(tag)

    if "cmp" in document:
        if "extraArgs" in document["cmp"]:
            catalog.noteExtraCmpArgs(document["cmp"]["extraArgs"])
        if "bespokeImplementation" in document["cmp"]:
            for bespoke in document["cmp"]["bespokeImplementation"]:
                catalog.noteBespokeCmpImplementation(bespoke)

    # For continuation YAML, add generated structs/unions to catalog
    if "continuations" in document:
        from generate.kontinuations import KontinuationGenerator
        generator = KontinuationGenerator(document)
        generator.populate_catalog(catalog)

    catalog.build()

    # Generate output based on type
    generate_output(args, catalog, document, typeName, description, includes, limited_includes, parserInfo)


def generate_output(args, catalog, document, typeName, description, includes, limited_includes, parserInfo):
    """Generate the appropriate output based on args.type"""
    
    if args.type == "h":
        generate_header(args, catalog, document, typeName, includes, limited_includes, parserInfo)
    elif args.type == "objtypes_h":
        generate_objtypes_header(args, catalog, document, typeName)
    elif args.type == "c":
        generate_implementation(args, catalog, document, typeName)
    elif args.type == 'debug_h':
        generate_debug_header(args, catalog, document, typeName, includes, limited_includes)
    elif args.type == 'debug_c':
        generate_debug_implementation(args, catalog, document, typeName, limited_includes)
    elif args.type == 'md':
        generate_documentation(args, catalog, typeName, description)
    elif args.type == 'visitor':
        if args.suffix == "":
            print(f"Error: visitor type requires a suffix argument", file=sys.stderr)
            sys.exit(1)
        printGpl(args.yaml, document)
        print("")
        print(catalog.generateVisitor(args.suffix))
    elif args.type == 'kont_impl_inc':
        # For continuation scaffolding, generate .inc (catalog already populated)
        from generate.kontinuations import KontinuationGenerator
        generator = KontinuationGenerator(document)
        generator.generate_kont_impl_inc(sys.stdout, catalog, includes)


def generate_header(args, catalog, document, typeName, includes, limited_includes, parserInfo):
    """Generate main header file"""
    print(f"#ifndef cekf_{typeName}_h")
    print(f"#define cekf_{typeName}_h")
    printGpl(args.yaml, document)
    print("")
    print('#include "hash.h"')
    print('#include "memory.h"')
    print('#include "common.h"')
    print('#include "types.h"')
    if parserInfo:
        print('#include "parser_info.h"')
    for include in includes:
        print(f'#include "{include}"')
    for include in limited_includes:
        print(f'#include "{include}"')
    
    printSection("typedefs")
    catalog.printTypedefs()
    printSection("constructor declarations")
    catalog.printNewDeclarations()
    printSection("init declarations")
    catalog.printInitDeclarations()
    printSection("copy declarations")
    catalog.printCopyDeclarations()
    printSection("mark declarations")
    catalog.printMarkDeclarations()
    printSection("free declarations")
    catalog.printFreeDeclarations()
    printSection("protect declarations")
    catalog.printProtectDeclarations()
    printSection("push/pop declarations")
    catalog.printPushDeclarations()
    catalog.printPopDeclarations()
    catalog.printPopnDeclarations()
    catalog.printMoveDeclarations()
    catalog.printPushnDeclarations()
    catalog.printCopyTopDeclarations()
    catalog.printCopyExceptTopDeclarations()
    catalog.printCopyEntriesDeclarations()
    catalog.printClearDeclarations()
    catalog.printPeekDeclarations()
    catalog.printPeeknDeclarations()
    catalog.printPokeDeclarations()
    catalog.printExtendDeclarations()
    catalog.printSizeDeclarations()
    printSection("hash getter and setter declarations")
    catalog.printGetDeclarations()
    catalog.printSetDeclarations()
    catalog.printIteratorDeclarations()
    printSection("defines")
    catalog.printDefines()
    printSection("discriminated union helper constructor declarations")
    catalog.printHelperNewDeclarations()
    printSection("access declarations")
    catalog.printAccessDeclarations()
    printSection("count declarations")
    catalog.printCountDeclarations()
    printSection("name declarations")
    catalog.printNameFunctionDeclarations()
    printSection("discriminated union getter declarations")
    catalog.printGetterDeclarations()
    print("")
    print("#endif")


def generate_objtypes_header(args, catalog, document, typeName):
    """Generate object types header file"""
    print(f"#ifndef cekf_{typeName}_objtypes_h")
    print(f"#define cekf_{typeName}_objtypes_h")
    printGpl(args.yaml, document)
    printSection("define objtypes")
    catalog.printObjTypeDefine()
    printSection("define cases")
    catalog.printObjCasesDefine()
    printSection("declare generic type functions")
    print(f'void mark{typeName.capitalize()}Obj(struct Header *h);')
    print(f'void free{typeName.capitalize()}Obj(struct Header *h);')
    print(f'char *typename{typeName.capitalize()}Obj(int type);')
    print("")
    print("#endif")


def generate_implementation(args, catalog, document, typeName):
    """Generate main implementation file"""
    printGpl(args.yaml, document)
    print("")
    print(f'#include "{typeName}.h"')
    print("#include <stdio.h>")
    print("#include <strings.h>")
    print('#include "common.h"')
    print(f'#ifdef DEBUG_{typeName.upper()}')
    print('#include "debugging_on.h"')
    print('#else')
    print('#include "debugging_off.h"')
    print('#endif')
    
    printSection("constructor functions")
    catalog.printNewFunctions()
    printSection("init functions")
    catalog.printInitFunctions()
    printSection("copy functions")
    catalog.printCopyFunctions()
    printSection("push/pop functions")
    catalog.printPushFunctions()
    catalog.printPopFunctions()
    catalog.printPopnFunctions()
    catalog.printMoveFunctions()
    catalog.printPushnFunctions()
    catalog.printCopyTopFunctions()
    catalog.printCopyExceptTopFunctions()
    catalog.printCopyEntriesFunctions()
    catalog.printPeekFunctions()
    catalog.printPeeknFunctions()
    catalog.printPokeFunctions()
    catalog.printExtendFunctions()
    printSection("hash getter and setter functions")
    catalog.printGetFunctions()
    catalog.printSetFunctions()
    catalog.printIteratorFunctions()
    printSection("count functions")
    catalog.printCountFunctions()
    printSection("mark functions")
    catalog.printMarkFunctions()
    printSection("generic mark function")
    catalog.printMarkObjFunction()
    printSection("free functions")
    catalog.printFreeFunctions()
    printSection("generic free function")
    catalog.printFreeObjFunction()
    printSection("type identifier function")
    catalog.printTypeObjFunction()
    printSection("type name function")
    catalog.printNameFunctionBodies()
    printSection("protect functions")
    catalog.printProtectFunctions()


def generate_debug_header(args, catalog, document, typeName, includes, limited_includes):
    """Generate debug header file"""
    print(f"#ifndef cekf_{typeName}_debug_h")
    print(f"#define cekf_{typeName}_debug_h")
    printGpl(args.yaml, document)
    print("")
    print(f'#include "{typeName}_helper.h"')
    for include in includes:
        print(f'#include "{include[0:-2]}_debug.h"')
    for include in limited_includes:
        print(f'#include "{include}"')
    printSection("print declarations")
    catalog.printPrintDeclarations()
    printSection("compare declarations")
    catalog.printCompareDeclarations()
    print("")
    print("#endif")


def generate_debug_implementation(args, catalog, document, typeName, limited_includes):
    """Generate debug implementation file"""
    printGpl(args.yaml, document)
    print("")
    print('#include <stdio.h>')
    print("")
    print(f'#include "{typeName}_debug.h"')
    for include in limited_includes:
        print(f'#include "{include}"')
    printSection("helper functions")
    print('static void pad(int depth) { eprintf("%*s", depth * PAD_WIDTH, ""); }')
    printSection("print functions")
    catalog.printPrintFunctions()
    printSection("compare functions")
    catalog.printCompareFunctions()


def generate_documentation(args, catalog, typeName, description):
    """Generate Mermaid documentation"""
    print(f"# {typeName}")
    print("")
    print(description)
    print("")
    print("```mermaid")
    print("flowchart LR")
    catalog.printMermaid()
    print("```")
    print("")
    print(f"> Generated from {args.yaml} by tools/generate.py")


if __name__ == "__main__":
    main()
