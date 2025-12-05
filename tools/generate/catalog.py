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
Catalog class - central registry for all generated entities.

The Catalog manages the collection of structures, unions, enums, arrays, etc.
that are defined in a YAML schema file, and orchestrates their code generation.
"""

from .comment_gen import CommentGen


class Catalog:
    """Central registry managing all entities in a schema."""
    
    def __init__(self, typeName):
        self.typeName = typeName
        self.contents = {}
        self.parserInfo = False

    def add(self, value):
        name = value.getName()
        if name in self.contents:
            raise Exception("attempt to overwtite " + name + " in catalog")
        self.contents[name] = value

    def tag(self, t):
        if t in self.contents:
            self.contents[t].tag()

    def noteExtraCmpArgs(self, args):
        for key in self.contents:
            self.contents[key].noteExtraCmpArgs(args)

    def noteParserInfo(self):
        self.parserInfo = True

    def noteBespokeCmpImplementation(self, name):
        if name in self.contents:
            self.contents[name].noteBespokeCmpImplementation()
        else:
            raise Exception("bespoke cmp implementation declared for nonexistant entry " + name)

    def get(self, key):
        key = key.strip()
        if key in self.contents:
            return self.contents[key]
        else:
            raise Exception("key '" + key + "' not found in catalog")

    def build(self):
        values = []
        for entity in list(self.contents.values()):
            entity.build(self)

    def printHelperNewDeclarations(self):
        for entity in self.contents.values():
            entity.printHelperNewDeclarations(self)

    def printTypedefs(self):
        for entity in self.contents.values():
            if entity.isEnum():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isArray():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isUnion():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isStruct():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isHash():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isVector():
                entity.printTypedef(self)

    def printInitDeclarations(self):
        for entity in self.contents.values():
            entity.printInitDeclaration(self)

    def printInitFunctions(self):
        for entity in self.contents.values():
            entity.printInitFunction(self)

    def printMarkDeclarations(self):
        for entity in self.contents.values():
            entity.printMarkDeclaration(self)

    def printCountDeclarations(self):
        for entity in self.contents.values():
            entity.printCountDeclaration(self)

    def printCountFunctions(self):
        for entity in self.contents.values():
            entity.printCountFunction(self)

    def printAccessDeclarations(self):
        for entity in self.contents.values():
            entity.printAccessDeclarations(self)

    def printPushDeclarations(self):
        for entity in self.contents.values():
            entity.printPushDeclaration(self)

    def printPushFunctions(self):
        for entity in self.contents.values():
            entity.printPushFunction(self)

    def printPopDeclarations(self):
        for entity in self.contents.values():
            entity.printPopDeclaration(self)

    def printPopnDeclarations(self):
        for entity in self.contents.values():
            entity.printPopnDeclaration(self)

    def printMoveDeclarations(self):
        for entity in self.contents.values():
            entity.printMoveDeclaration(self)

    def printPushnDeclarations(self):
        for entity in self.contents.values():
            entity.printPushnDeclaration(self)

    def printCopyTopDeclarations(self):
        for entity in self.contents.values():
            entity.printCopyTopDeclaration(self)

    def printCopyExceptTopDeclarations(self):
        for entity in self.contents.values():
            entity.printCopyExceptTopDeclaration(self)

    def printCopyEntriesDeclarations(self):
        for entity in self.contents.values():
            entity.printCopyEntriesDeclaration(self)

    def printClearDeclarations(self):
        for entity in self.contents.values():
            entity.printClearDeclaration(self)

    def printPeekDeclarations(self):
        for entity in self.contents.values():
            entity.printPeekDeclaration(self)

    def printPeeknDeclarations(self):
        for entity in self.contents.values():
            entity.printPeeknDeclaration(self)

    def printPokeDeclarations(self):
        for entity in self.contents.values():
            entity.printPokeDeclaration(self)

    def printExtendDeclarations(self):
        for entity in self.contents.values():
            entity.printExtendDeclaration(self)

    def printSizeDeclarations(self):
        for entity in self.contents.values():
            entity.printSizeDeclaration(self)

    def printPopFunctions(self):
        for entity in self.contents.values():
            entity.printPopFunction(self)

    def printPopnFunctions(self):
        for entity in self.contents.values():
            entity.printPopnFunction(self)

    def printMoveFunctions(self):
        for entity in self.contents.values():
            entity.printMoveFunction(self)

    def printPushnFunctions(self):
        for entity in self.contents.values():
            entity.printPushnFunction(self)

    def printCopyTopFunctions(self):
        for entity in self.contents.values():
            entity.printCopyTopFunction(self)

    def printCopyExceptTopFunctions(self):
        for entity in self.contents.values():
            entity.printCopyExceptTopFunction(self)

    def printCopyEntriesFunctions(self):
        for entity in self.contents.values():
            entity.printCopyEntriesFunction(self)

    def printPeekFunctions(self):
        for entity in self.contents.values():
            entity.printPeekFunction(self)

    def printPeeknFunctions(self):
        for entity in self.contents.values():
            entity.printPeeknFunction(self)

    def printPokeFunctions(self):
        for entity in self.contents.values():
            entity.printPokeFunction(self)

    def printExtendFunctions(self):
        for entity in self.contents.values():
            entity.printExtendFunction(self)

    def printSetDeclarations(self):
        for entity in self.contents.values():
            entity.printSetDeclaration(self)

    def printGetDeclarations(self):
        for entity in self.contents.values():
            entity.printGetDeclaration(self)

    def printSetFunctions(self):
        for entity in self.contents.values():
            entity.printSetFunction(self)

    def printGetFunctions(self):
        for entity in self.contents.values():
            entity.printGetFunction(self)

    def printIteratorDeclarations(self):
        for entity in self.contents.values():
            entity.printIteratorDeclaration(self)

    def printIteratorFunctions(self):
        for entity in self.contents.values():
            entity.printIteratorFunction(self)

    def printFreeDeclarations(self):
        for entity in self.contents.values():
            entity.printFreeDeclaration(self)

    def printProtectDeclarations(self):
        for entity in self.contents.values():
            entity.printProtectDeclaration(self)

    def printProtectFunctions(self):
        for entity in self.contents.values():
            entity.printProtectFunction(self)

    def printNewDeclarations(self):
        for entity in self.contents.values():
            entity.printNewDeclaration(self)

    def printCopyDeclarations(self):
        for entity in self.contents.values():
            entity.printCopyDeclaration(self)

    def printNameFunctionDeclarations(self):
        for entity in self.contents.values():
            entity.printNameFunctionDeclaration()

    def printNameFunctionBodies(self):
        for entity in self.contents.values():
            entity.printNameFunctionBody()

    def printPrintFunctions(self):
        for entity in self.contents.values():
            entity.printPrintFunction(self)

    def printCompareFunctions(self):
        for entity in self.contents.values():
            entity.printCompareFunction(self)

    def printPrintDeclarations(self):
        for entity in self.contents.values():
            entity.printPrintDeclaration(self)

    def printCompareDeclarations(self):
        for entity in self.contents.values():
            entity.printCompareDeclaration(self)

    def printDefines(self):
        for entity in self.contents.values():
            entity.printDefines(self)

    def printNewFunctions(self):
        for entity in self.contents.values():
            entity.printNewFunction(self)

    def printCopyFunctions(self):
        for entity in self.contents.values():
            entity.printCopyFunction(self)

    def printMarkFunctions(self):
        for entity in self.contents.values():
            entity.printMarkFunction(self)

    def printFreeFunctions(self):
        for entity in self.contents.values():
            entity.printFreeFunction(self)

    def printMermaid(self):
        for entity in self.contents.values():
            entity.printMermaid(self)

    def comment(self, method):
        return CommentGen.method_comment('Catalog', method)

    def printMarkObjFunction(self):
        c = self.comment('printMarkObjFunction')
        print(f'void mark{self.typeName.capitalize()}Obj(struct Header *h) {{ {c}')
        print(f'    switch(h->type) {{ {c}')
        for entity in self.contents.values():
            entity.printMarkObjCase(self)
        print(f'        default: {c}')
        print(f'            cant_happen("unrecognised type %d in mark{self.typeName.capitalize()}Obj\\n", h->type); {c}')
        print(f'    }} {c}')
        print(f'}} {c}')

    def printFreeObjFunction(self):
        c = self.comment('printFreeObjFunction')
        print(f'void free{self.typeName.capitalize()}Obj(struct Header *h) {{ {c}')
        print(f'    switch(h->type) {{ {c}')
        for entity in self.contents.values():
            entity.printFreeObjCase(self)
        print(f'        default: {c}')
        print(f'            cant_happen("unrecognised type %d in free{self.typeName.capitalize()}Obj\\n", h->type); {c}')
        print(f'    }} {c}')
        print(f'}} {c}')

    def printTypeObjFunction(self):
        c = self.comment('printTypeObjFunction')
        print(f'char *typename{self.typeName.capitalize()}Obj(int type) {{ {c}')
        print(f'    switch(type) {{ {c}')
        for entity in self.contents.values():
            entity.printTypeObjCase(self)
        print(f'        default: {c}')
        print(f'            return "???"; {c} no error, can be used during error reporting')
        print(f'    }} {c}')
        print(f'}} {c}')

    def printObjTypeDefine(self):
        objTypeArray = []
        for entity in self.contents.values():
            objTypeArray += entity.objTypeArray()
        print("#define {typeName}_OBJTYPES() \\\n{a}".format(a=', \\\n'.join(objTypeArray), typeName=self.typeName.upper()))

    def printObjCasesDefine(self):
        print(f"#define {self.typeName.upper()}_OBJTYPE_CASES() \\")
        for entity in self.contents.values():
            objType = entity.objTypeArray()
            if len(objType) == 1:
                print(f'case {objType[0]}:\\')
        print("")
