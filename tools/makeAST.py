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
#

# reads YAML and outputs C code for structs and unions

import yaml
import sys
import argparse
import re
import os
import datetime

class Catalog:
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
        return f'// Catalog.{method}'

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


class Base:
    """
    Base class for top-level entities
    Contains default no-op behavior
    All classes contained directly by the catalog should use this base class
    """
    def __init__(self, name, body):
        self.name = name
        self.tagged = False
        self.bespokeCmpImplementation = False
        self.extraCmpArgs = {}
        self.hasDocs = False
        self.brief = None
        self.description = None
        if "meta" in body:
            meta = body["meta"]
            if "brief" in meta:
                self.hasDocs = True
                self.brief = meta["brief"]
            if "description" in meta:
                self.hasDocs = True
                self.description = meta["description"]

    def printBaseDocumentation(self):
        if self.hasDocs:
            print("/**")
            if self.brief is not None:
                print(f" * @brief: {self.brief}")
            for line in self.formatDescription():
                print(f" * {line}")
            print(" */")

    def formatDescription(self):
        """
        Format the description for documentation.
        Splits the description into lines of a maximum length of 70 characters.
        """
        if self.description is None:
            return []
        words = self.description.split()
        lines = []
        current_line = ""
        for word in words:
            if len(current_line) + len(word) + 1 > 70:
                lines.append(current_line.strip())
                current_line = word + " "
            else:
                current_line += word + " "
        if current_line:
            lines.append(current_line.strip())
        return lines

    def isInline(self, catalog):
        return False

    def noteTypedef(self):
        with open(".typedefs", "a") as f:
            print(f'-T {self.name}', file=f)

    def isSelfInitializing(self):
        return False

    def noteExtraCmpArgs(self, args):
        self.extraCmpArgs = args

    def objTypeArray(self):
        return []

    def tag(self):
        self.tagged = True

    def getName(self):
        return self.name

    def hasParserInfo(self, catalog):
        return False

    def build(self, catalog):
        pass

    def printTypedef(self, catalog):
        pass

    def printHelperNewDeclarations(self, catalog):
        pass

    def printNameFunctionDeclaration(self):
        pass

    def printNameFunctionBody(self):
        pass

    def printFreeDeclaration(self, catalog):
        pass

    def printProtectDeclaration(self, catalog):
        pass

    def printProtectFunction(self, catalog):
        pass

    def printMarkDeclaration(self, catalog):
        pass

    def printMarkFunction(self, catalog):
        pass

    def printFreeFunction(self, catalog):
        pass

    def printInitDeclaration(self, catalog):
        pass

    def printInitFunction(self, catalog):
        pass

    def printNewDeclaration(self, catalog):
        pass

    def printCopyDeclaration(self, catalog):
        pass

    def printNewFunction(self, catalog):
        pass

    def printCopyFunction(self, catalog):
        pass

    def printPrintDeclaration(self, catalog):
        pass

    def printCompareDeclaration(self, catalog):
        pass

    def printPrintFunction(self, catalog):
        pass

    def printCompareFunction(self, catalog):
        pass

    def printMarkObjCase(self, catalog):
        pass

    def printFreeObjCase(self, catalog):
        pass

    def printTypeObjCase(self, catalog):
        pass

    def printDefines(self, catalog):
        pass

    def printAccessDeclarations(self, catalog):
        pass

    def printPushDeclaration(self, catalog):
        pass

    def printPushFunction(self, catalog):
        pass

    def printPopDeclaration(self, catalog):
        pass

    def printPopnDeclaration(self, catalog):
        pass

    def printMoveDeclaration(self, catalog):
        pass

    def printPushnDeclaration(self, catalog):
        pass

    def printCopyTopDeclaration(self, catalog):
        pass

    def printCopyExceptTopDeclaration(self, catalog):
        pass

    def printCopyEntriesDeclaration(self, catalog):
        pass

    def printClearDeclaration(self, catalog):
        pass

    def printPeekDeclaration(self, catalog):
        pass

    def printPeeknDeclaration(self, catalog):
        pass

    def printPokeDeclaration(self, catalog):
        pass

    def printExtendDeclaration(self, catalog):
        pass

    def printSizeDeclaration(self, catalog):
        pass

    def printPopFunction(self, catalog):
        pass

    def printPopnFunction(self, catalog):
        pass

    def printMoveFunction(self, catalog):
        pass

    def printPushnFunction(self, catalog):
        pass

    def printCopyTopFunction(self, catalog):
        pass

    def printCopyExceptTopFunction(self, catalog):
        pass

    def printCopyEntriesFunction(self, catalog):
        pass

    def printPeekFunction(self, catalog):
        pass

    def printPeeknFunction(self, catalog):
        pass

    def printPokeFunction(self, catalog):
        pass

    def printExtendFunction(self, catalog):
        pass

    def printCountFunction(self, catalog):
        pass

    def isEnum(self):
        return False

    def isUnion(self):
        return False

    def isStruct(self):
        return False

    def isArray(self):
        return False

    def isHash(self):
        return False

    def isVector(self):
        return False

    def noteBespokeCmpImplementation(self):
        self.bespokeCmpImplementation = True

    def makeCopyCommand(self, arg, catalog):
        return arg

    def printMarkField(self, isInline, field, depth, prefix=''):
        pass

    def printSetDeclaration(self, catalog):
        pass

    def printGetDeclaration(self, catalog):
        pass

    def printIteratorDeclaration(self, catalog):
        pass

    def printCountDeclaration(self, catalog):
        pass

    def printSetFunction(self, catalog):
        pass

    def printGetFunction(self, catalog):
        pass

    def printIteratorFunction(self, catalog):
        pass

    def hasMarkFn(self):
        return True


class EnumField:
    """
    Serves as the class for simple enumeration fields and as
    the base class for discriminated union enum fields
    """
    def __init__(self, owner, name):
        if name is True:
            raise Exception("EnumField passed a boolean")
        self.owner = owner
        self.name = name

    def getName(self):
        return self.name

    def isSimpleField(self):
        return False

    def isSelfInitializing(self, catalog):
        return False

    def printEnumTypedefLine(self, count):
        field = self.makeTypeName()
        print(f"    {field}, // {count}")

    def comment(self, method):
        return f'// EnumField.{method}'

    def printNameFunctionLine(self):
        c = self.comment('printNameFunctionLine')
        field = self.makeTypeName()
        print(f'        case {field}: return "{field}"; {c}')

    def makeTypeName(self):
        v = self.owner + '_type_' + self.name
        v = v.upper().replace('AST', 'AST_')
        return v

    def printCompareCase(self, depth):
        c = self.comment('printCompareCase')
        typeName = self.makeTypeName()
        pad(depth)
        print(f'case {typeName}: {c}')
        pad(depth + 1)
        print(f"if (a != b) return false; {c}")
        pad(depth + 1)
        print(f'break; {c}')

    def printPrintCase(self, depth):
        c = self.comment('printPrintCase')
        typeName = self.makeTypeName()
        pad(depth)
        print(f'case {typeName}: {c}')
        pad(depth + 1)
        print(f'pad(depth + 1); {c}')
        pad(depth + 1)
        print(f'eprintf("{typeName}"); {c}')
        pad(depth + 1)
        print(f'break; {c}')

class SimpleField:
    """
    Represents a field in a SimpleStruct object
    """
    def __init__(self, owner, name, typeName):
        self.owner = owner
        parts = re.split(r"\s*=\s*", typeName, 1)
        self.name = name
        if len(parts) == 2:
            self.typeName = parts[0]
            self.default = parts[1]
        else:
            self.typeName = typeName
            self.default = None

    def isSimpleField(self):
        return True

    def getName(self):
        return self.name

    def getObj(self, catalog):
        return catalog.get(self.typeName)

    def getObjName(self, catalog):
        return self.getObj(catalog).getName()

    def isInline(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.isInline(catalog)

    def isSelfInitializing(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.isSelfInitializing()

    def getConstructorName(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getConstructorName()

    def printPrintDeclaration(self, catalog):
        obj = catalog.get(self.typeName)
        obj.printPrintDeclaration(catalog)

    def getTypeDeclaration(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getTypeDeclaration(catalog)

    def getSignature(self, catalog):
        return "{type} {name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getArraySignature(self, catalog):
        return "{type} *{name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getVectorSignature(self, catalog):
        return "{type} {name}[0]".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getFieldName(self):
        return self.name

    def hasMarkFn(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.hasMarkFn()

    def getCopyCall(self, arg, catalog):
        obj = catalog.get(self.typeName)
        return obj.makeCopyCommand(arg, catalog)

    def printMarkLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(isInline, self.name, depth)

    def printMarkArrayLine(self, isInline, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(isInline, f"{self.name}[{key}]", depth)

    def printMarkHashLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkHashField(depth)

    def printPrintHashLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintHashField(depth)

    def printCompareLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printCompareField(isInline, self.name, depth)

    def printPrintLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(isInline, self.name, depth)

    def printCopyLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printCopyField(isInline, self.name, depth)

    def printPrintArrayLine(self, isInline, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(isInline, f"{self.name}[{key}]", depth)

    def printCopyArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printCopyField(obj.isInline(catalog), f"{self.name}[{key}]", depth)

    def printCompareArrayLine(self, isInline, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printCompareField(isInline, f"{self.name}[{key}]", depth)

    def comment(self, method):
        return f'// SimpleField.{method}'

    def printStructTypedefLine(self, catalog):
        c = self.comment('printStructTypedefLine')
        decl=self.getSignature(catalog)
        print(f"    {decl}; {c}")

    def printArrayTypedefLine(self, catalog):
        c = self.comment('printArrayTypedefLine')
        decl=self.getArraySignature(catalog)
        print(f"    {decl}; {c}")

    def printVectorTypedefLine(self, catalog):
        c = self.comment('printVectorTypedefLine')
        decl=self.getVectorSignature(catalog)
        print(f"    {decl}; {c}")


class SimpleHash(Base):
    """
    Hash tables
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            if "entries" in data:
                self.entries = SimpleField(self.name, "entries", data["entries"])
            else:
                self.entries = None
        else:
            raise Exception(f"SimpleHash {name} must have 'data' field")

    def isHash(self):
        return True

    def printMermaid(self, catalog):
        myName = self.getName()
        if self.entries is None:
            print(myName)
        else:
            print(f"{myName} --entries--> {self.entries.getName()}")

    def isSelfInitializing(self):
        return True # other constructors will call this automatically

    def getConstructorName(self):
        myName = self.getName()
        return f"new{myName}"

    def getTypeDeclaration(self, catalog):
        myName = self.getName()
        return f"struct {myName} *"

    def comment(self, method):
        return f'// SimpleHash.{method}'

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl=self.getNewSignature()
        print(f"{decl}; {c}")

    def getNewSignature(self):
        myType = self.getTypeDeclaration(catalog)
        myConstructor = self.getConstructorName()
        return f"{myType}{myConstructor}(void)"

    def getSetDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        if self.entries is None:
            return f'void set{myName}({myType} table, HashSymbol *key)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'void set{myName}({myType} table, HashSymbol *key, {valueType} value)'

    def printSetDeclaration(self, catalog):
        c = self.comment('printSetDeclaration')
        decl = self.getSetDeclaration(catalog)
        print(f'{decl}; {c}')

    def getGetDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        if self.entries is None:
            return f'bool get{myName}({myType} table, HashSymbol *key)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'bool get{myName}({myType} table, HashSymbol *key, {valueType}* value)'

    def printGetDeclaration(self, catalog):
        c = self.comment('printGetDeclaration')
        decl = self.getGetDeclaration(catalog)
        print(f'{decl}; {c}')

    def getIteratorDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        if self.entries is None:
            return f'HashSymbol * iterate{myName}({myType} table, Index *i)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'HashSymbol * iterate{myName}({myType} table, Index *i, {valueType}*value)'

    def printIteratorDeclaration(self, catalog):
        c = self.comment('printIteratorDeclaration')
        decl = self.getIteratorDeclaration(catalog)
        print(f'{decl}; {c}')

    def printIteratorFunction(self, catalog):
        c = self.comment('printIteratorFunction')
        decl = self.getIteratorDeclaration(catalog)
        print(f'{decl} {{ {c}')
        if self.entries is None:
            print('    return iterateHashTable((HashTable *)table, i, NULL);')
        else:
            print('    return iterateHashTable((HashTable *)table, i, value);')
        print(f'}} {c}')
        print('')

    def printSetFunction(self, catalog):
        c = self.comment('printSetFunction')
        decl = self.getSetDeclaration(catalog)
        print(f'{decl} {{ {c}')
        if self.entries is None:
            print(f'    hashSet((HashTable *)table, key, NULL); {c}')
        else:
            print(f'    hashSet((HashTable *)table, key, &value); {c}')
        print(f'}} {c}')
        print('')

    def printGetFunction(self, catalog):
        c = self.comment('printGetFunction')
        decl = self.getGetDeclaration(catalog)
        print(f'{decl} {{ {c}')
        if self.entries is None:
            print(f'    return hashGet((HashTable *)table, key, NULL); {c}')
        else:
            print(f'    return hashGet((HashTable *)table, key, value); {c}')
        print(f'}} {c}')
        print('')

    def printCountDeclaration(self, catalog):
        c = self.comment('printCountDeclaration')
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        print(f'static inline Index count{myName}({myType} table) {{ {c}')
        print(f'    return ((HashTable *)table)->count; {c}')
        print(f'}} {c}')
        print('')

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        myName = self.getName()
        self.printBaseDocumentation()
        print(f'typedef struct {myName} {{ {c}')
        print(f'    struct HashTable wrapped; {c}')
        print(f'}} {myName}; {c}')
        print('')

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"void print{myName}({myType} x, int depth)"

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl = self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printPrintFunction(self, catalog):
        decl = self.getPrintSignature(catalog)
        c = self.comment('printPrintFunction')
        print(f"{decl} {{ {c}")
        print(f"    printHashTable(&(x->wrapped), depth); {c}")
        print(f"}} {c}")
        print("")

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        myConstructor = self.getConstructorName()
        a = '.' if isInline else '->'
        print(f'    x{a}{prefix}{field} = {myConstructor}(); {c}')
        print(f'    copyHashTable((HashTable *)x{a}{prefix}{field}, (HashTable *)o{a}{prefix}{field}); {c}')

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        pad(depth)
        print(f'printHashTable(*(HashTable **)ptr, depth + 1); {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        a = '.' if isInline else '->'
        pad(depth)
        print(f'printHashTable((HashTable *)x{a}{prefix}{field}, depth + 1); {c}')

    def printCompareField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        pad(depth)
        print(f"return false; {c}")

    def printNewFunction(self, catalog):
        c = self.comment('printNewFunction')
        decl = self.getNewSignature()
        myName = self.getName()
        if self.entries is None:
            size = '0'
            markFn = 'NULL'
            printFn = 'NULL'
        else:
            size = f'sizeof({self.entries.getTypeDeclaration(catalog)})'
            printFn = f'_print{myName}'
            if self.entries.hasMarkFn(catalog):
                markFn = f'_mark{myName}'
                print(f'static void {markFn}(void *ptr) {{ {c}')
                self.entries.printMarkHashLine(catalog, 1)
                print(f'}} {c}')
                print('')
            else:
                markFn = 'NULL'
            self.entries.printPrintDeclaration(catalog)
            print('')
            print(f'static void {printFn}(void *ptr, int depth) {{ {c}')
            self.entries.printPrintHashLine(catalog, 1)
            print(f'}} {c}')
            print('')
        print(f"""/**
  * @brief Create a new {myName}.
  * This generated function initializes a new {myName} structure,
  * which is a wrapper around a HashTable.
  */
""")
        print(f'{decl} {{ {c}')
        print(f'    return ({myName} *)newHashTable({size}, {markFn}, {printFn}); {c}')
        print(f'}} {c}')
        print('')

    def printMarkField(self, isInline, field, depth, prefix=''):
        c = self.comment('printMarkField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f"markHashTable((HashTable *)x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f"return PROTECT((HashTable *)x{a}{prefix}{field}); {c}")


class SimpleArray(Base):
    """
    Array structures declared in the yaml
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.dimension = data["dimension"] if "dimension" in data else 1
            if self.dimension > 2:
                raise Exception("only 1 or 2 dimensional arrays supported for now")
            if self.dimension == 2:
                self.width = SimpleField(self.name, "width", "int")
                self.height = SimpleField(self.name, "height", "int")
            self.entries = SimpleField(self.name,"entries", data["entries"])
        else:
            raise Exception(f"SimpleArray {name} must have 'data' field")

    def printMermaid(self, catalog):
        myName = self.getName()
        mySpec = '[]' * self.dimension
        print(f'{myName}["{myName}{mySpec}"] --entries--> {self.entries.getObjName(catalog)}')

    def getDefineValue(self):
        return 'x'

    def getDefineArg(self):
        return 'x'

    def tag(self):
        super().tag()
        self.tagField = SimpleField(self.name, "_tag", "string")

    def getTypeDeclaration(self, catalog):
        a = '' if self.isInline(catalog) else '*'
        name = self.getName()
        return f"struct {name} {a}"

    def printCompareField(self, isInline, field, depth, prefix=''):
        myName=self.getName()
        extraCmpArgs = self.getExtraCmpAargs(catalog)
        a = '.' if isInline else '->'
        c = self.comment('printCompareField')
        pad(depth)
        print(f"if (!eq{myName}(a{a}{prefix}{field}, b{a}{prefix}{field}{extraCmpArgs})) return false; {c}")

    def comment(self, method):
        return f'// SimpleArray.{method}'

    def printCopyField(self, isInline, field, depth, prefix=''):
        myName=self.getName()
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f'x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        pad(depth)
        myName=self.getName()
        print(f'print{myName}(*({myName} **)ptr, depth + 1); {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        myName=self.getName()
        a = '.' if isInline else '->'
        pad(depth)
        print(f'print{myName}(x{a}{prefix}{field}, depth + 1); {c}')

    def printAccessDeclarations(self, catalog):
        entryType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printAccessDeclarations')
        if self.dimension == 2:
            print(f"static inline {entryType} get{myName}Index({myType} obj, Index x, Index y) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x >= obj->width || y >= obj->height) {{ {c}")
            print(f'        cant_happen("{myName} 2d matrix bounds exceeded"); {c}')
            print(f"    }}")
            print(f"#endif")
            print(f"    return obj->entries[x + y * obj->width]; {c}")
            print(f"}} {c}")
            print("")
            print(f"static inline void set{myName}Index({myType} obj, Index x, Index y, {entryType} val) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x >= obj->width || y >= obj->height) {{ {c}")
            print(f'        cant_happen("{myName} 2d matrix bounds exceeded"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    obj->entries[x + y * obj->width] = val; {c}")
            print(f"}} {c}")
            print("")

    def printIndexFields(self):
        c = self.comment('printIndexFields')
        print(f"    Index size; {c}")
        print(f"    Index capacity; {c}")

    def printExtraStackEntries(self):
        pass

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        if self.tagged:
            print(f"    char *_tag; {c}")
        if self.dimension == 2: # 2D arrays are fixed size
            print(f"    Index width; {c}")
            print(f"    Index height; {c}")
        else:                   # 1D arrays can grow
            self.printIndexFields()
        self.entries.printArrayTypedefLine(catalog)
        self.printExtraStackEntries()
        print(f"}} {name}; {c}\n")

    def printMarkDeclaration(self, catalog):
        c = self.comment('printMarkDeclaration')
        decl=self.getMarkSignature(catalog)
        print(f"{decl}; {c}")

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def printFreeDeclaration(self, catalog):
        c = self.comment('printFreeDeclaration')
        decl=self.getFreeSignature(catalog)
        print(f"{decl}; {c}")

    def printFreeFunction(self, catalog):
        myName = self.getName()
        decl = decl=self.getFreeSignature(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printFreeFunction')
        print(f"{decl} {{ {c}")
        if self.dimension == 1:
            print(f"    FREE_ARRAY({entryType}, x->entries, x->capacity); {c}")
        else:
            print(f"    FREE_ARRAY({entryType}, x->entries, x->width * x->height); {c}")
        print(f"    FREE(x, {myName}); {c}")
        print(f"}} {c}")
        print("")

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getObjType(self):
        return ('objtype_' + self.getName()).upper()

    def objTypeArray(self):
        return [ self.getObjType() ]

    def getNewArgs(self, catalog):
        if self.tagged:
            return [self.tagField] + self._getNewArgs()
        else:
            return self._getNewArgs()

    def _getNewArgs(self):
        if self.dimension == 1:
            return []
        return [self.width, self.height]

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        args = []
        for field in self.getNewArgs(catalog):
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        return "{myType} new{myName}({args})".format(myType=myType, myName=self.getName(), args=', '.join(args))

    def getCopySignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"{myType} copy{myName}({myType} o)"

    def printNullEntries(self):
        c = self.comment('printNullEntries')
        print(f"    x->entries = NULL; {c}")

    def printZeroCapacities(self):
        c = self.comment('printZeroCapacities')
        print(f"    x->size = 0; {c}")
        print(f"    x->capacity = 0; {c}")

    def printInitEntries(self, catalog):
        c = self.comment('printInitEntries')
        print(f"    x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, 8); {c}")
        print(f"    x->capacity = 8; {c}")

    def printNewFunction(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        decl = self.getNewSignature(catalog)
        c = self.comment('printNewFunction')
        print(f"{decl} {{ {c}")
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("new {myName} %p", x); {c}')
        if self.tagged:
            print(f"    x->_tag = _tag; {c}")
        self.printNullEntries()
        if self.dimension == 1:
            self.printZeroCapacities()
            print(f"    int save = PROTECT(x); {c}")
            self.printInitEntries(catalog)
        else:
            print(f"    x->width = 0; {c}")
            print(f"    x->height = 0; {c}")
            print(f"    int save = PROTECT(x); {c}")
            print(f"    if (width * height > 0) {{ {c}")
            print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, width * height); {c}")
            print(f"        bzero(x->entries, sizeof({self.entries.getTypeDeclaration(catalog)}) * width * height); {c}")
            print(f"    }} {c}")
            print(f"    x->width = width; {c}")
            print(f"    x->height = height; {c}")
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl=self.getNewSignature(catalog)
        print(f"{decl}; {c}")

    def printCopyDeclaration(self, catalog):
        c = self.comment('printCopyDeclaration')
        decl=self.getCopySignature(catalog)
        print(f"{decl}; {c}")

    def printExtendDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printExtendDeclaration')
            print(f"void extend{name}({myType} {a}obj, Index size); {c}")

    def printSizeDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printSizeDeclaration')
            print(f"static inline Index size{name}({myType} {a}obj) {{ return obj->size; }} {c}")

    def printPushDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPushDeclaration')
            print(f"Index push{name}({myType} {a}obj, {entryType} entry); {c}")

    def printPopDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopDeclaration')
            print(f"{entryType} pop{name}({myType} {a}obj); {c}")

    def printPopnDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopnDeclaration')
            print("#ifdef SAFETY_CHECKS")
            print(f"void popn{name}({myType} {a}obj, int n); {c}")
            print("#else")
            print(f"static inline void popn{name}({myType} {a}obj, int n) {{ obj->size -= n; }}; {c}")
            print("#endif")

    def printMoveDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printMoveDeclaration')
            print(f"void move{name}({myType} {a}obj, int b, int n); {c}")

    def printPushnDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPushnDeclaration')
            print(f"void pushn{name}({myType} {a}obj, int n, {entryType} entry); {c}")

    def printCopyTopDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyTopDeclaration')
            print(f"void copyTop{name}({myType} {a}dest, {myType} {a}src, int n); {c}")

    def printCopyExceptTopDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyExceptTopDeclaration')
            print(f"void copyExceptTop{name}({myType} {a}dest, {myType} {a}src, int n); {c}")

    def printCopyEntriesDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyEntriesDeclaration')
            print(f"void copy{name}Entries({myType} {a}dest, {myType} {a}src); {c}")

    def printClearDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printClearDeclaration')
            print(f"static inline void clear{name}({myType} {a}obj) {{ obj->size = 0; }}; {c}")

    def printPeekDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeekDeclaration')
            print(f"{entryType} peek{name}({myType} {a}obj); {c}")

    def printPeeknDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeeknDeclaration')
            print(f"{entryType} peekn{name}({myType} {a}obj, int offset); {c}")

    def printPokeDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPokeDeclaration')
            print(f"void poke{name}({myType} {a}obj, int offset, {entryType} val); {c}")

    def printExtendFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            c = self.comment('printExtendFunction')
            a = '*' if self.isInline(catalog) else ''
            print(f"/**")
            print(f" * Ensures that `x` has at least a capacity of `size`.")
            print(f" */")
            print(f"void extend{name}({myType} {a}x, Index size) {{ {c}")
            print(f'    DEBUG("extend{name}(%p, %u)", x, size);')
            print(f"    if (size > 0) {{ {c}")
            print(f"        size = size < 8 ? 8 : size; {c}")
            print(f"        if (x->capacity == 0) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"            if (x->entries != NULL) {{ {c}")
            print(f'                cant_happen("{name} non-null entries with zero capacity"); {c}')
            print(f"            }} {c}")
            print(f"#endif")
            print(f"            x->entries = NEW_ARRAY({entryType}, size); {c}")
            print(f"            x->capacity = size; {c}")
            print(f"        }} else {{ {c}")
            print(f"            while (size > x->capacity) {{ {c}")
            print(f"                x->entries = GROW_ARRAY({entryType}, x->entries, x->capacity, x->capacity *2); {c}")
            print(f"                x->capacity *= 2; {c}")
            print(f"            }} {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}\n")

    def printPushFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            c = self.comment('printPushFunction')
            a = '*' if self.isInline(catalog) else ''
            print(f"/**")
            print(f" * Pushes `entry` on to `x`, extending `x` if required.")
            print(f" * Returns the stack pointer after the push.")
            print(f" */")
            print(f"Index push{name}({myType} {a}x, {entryType} entry) {{ {c}")
            print(f'    DEBUG("push{name}(%p)", x);')
            print(f"    extend{name}(x, x->size + 1); {c}")
            print(f"    x->entries[x->size++] = entry; {c}")
            print(f"    return x->size - 1; {c}")
            print(f"}} {c}\n")

    def printPopFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopFunction')
            print(f"/**")
            print(f" * Pops the top entry from `x` and returns it.")
            print(f" */")
            print(f"{entryType} pop{name}({myType} {a}x) {{ {c}")
            print(f'    DEBUG("pop{name}(%p)", x);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x->size == 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return x->entries[--(x->size)]; {c}")
            print(f"}} {c}\n")

    def printPopnFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopnFunction')
            print(f"/**")
            print(f" * Discards the top `n` entries from `x`.")
            print(f" */")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"void popn{name}({myType} {a}x, int n) {{ {c}")
            print(f'    DEBUG("popn{name}(%p, %d)", x, n);')
            print(f"    if (((int) x->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, x->size); {c}')
            print(f"    }} {c}")
            print(f"    x->size -= n; {c}")
            print(f"}} {c}")
            print(f"#endif")
            print("")

    def printMoveFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printMoveFunction')
            print(f"/**")
            print(f" * Safe move `n` entries from `sp - n` to `b`,")
            print(f" * sets sp to `b + n`.")
            print(f" */")
            print(f"void move{name}({myType} {a}x, int b, int n) {{ {c}")
            print(f'    DEBUG("move{name}(%p, %d, %d)", x, b, n);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (((int) x->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, x->size); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(x, b + n); {c}")
            print(f"        MOVE_ARRAY({entryType}, &x->entries[b], &x->entries[x->size - n], n); {c}")
            print(f"    }} {c}")
            print(f"    x->size = (Index) (b + n); {c}")
            print(f"}} {c}\n")

    def printPushnFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPushnFunction')
            print(f"/**")
            print(f" * Pushes `n` copies of `entry` on to `x`.")
            print(f" */")
            print(f"void pushn{name}({myType} {a}x, int n, {entryType} entry) {{ {c}")
            print(f'    DEBUG("pushn{name}(%p, %d)", x, n);')
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(x, x->size + n); {c}")
            print(f"        while (n-- > 0) {{ {c}")
            print(f"            x->entries[x->size++] = entry; {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}\n")

    def printCopyTopFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyTopFunction')
            print(f"/**")
            print(f" * Copies the top `n` entries from `src` to the base of `dest`,")
            print(f" * sets `dest` size (sp) to `n`.")
            print(f" */")
            print(f"void copyTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
            print(f'    DEBUG("copyTop{name}(%p, %p, %d)", dest, src, n);')
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(dest, n); {c}")
            print(f"        COPY_ARRAY({entryType}, dest->entries, &src->entries[src->size - n], n); {c}")
            print(f"    }} {c}")
            print(f"    dest->size = n; {c}")
            print(f"}} {c}\n")

    def printCopyExceptTopFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyExceptTopFunction')
            print(f"/**")
            print(f" * Copies all but top `n` entries from `src` to the base of `dest`,")
            print(f" * sets `dest` sp to `src->sp - n`.")
            print(f" */")
            print(f"void copyExceptTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
            print(f'    DEBUG("copyExceptTop{name}(%p, %p, %d)", dest, src, n);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (((int) src->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, src->size); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    if (((Index) n) < src->size) {{ {c}")
            print(f"        extend{name}(dest, src->size - n); {c}")
            print(f"        COPY_ARRAY({entryType}, dest->entries, src->entries, src->size - n); {c}")
            print(f"    }} {c}")
            print(f"    dest->size = src->size - n; {c}")
            print(f"}} {c}\n")

    def printCopyEntriesFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyEntriesFunction')
            print(f"/**")
            print(f" * Copies all entries from `src` to `dest`,")
            print(f" * sets `dest` sp to `src->sp`.")
            print(f" */")
            print(f"void copy{name}Entries({myType} {a}dest, {myType} {a}src) {{ {c}")
            print(f'    DEBUG("copyEntries{name}(%p, %p)", dest, src);')
            print(f"    extend{name}(dest, src->size); {c}")
            print(f"    COPY_ARRAY({entryType}, dest->entries, src->entries, src->size); {c}")
            print(f"    dest->size = src->size; {c}")
            print(f"}} {c}\n")

    def printPeekFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeekFunction')
            print(f"/**")
            print(f" * Returns the value at the top of `x`.")
            print(f" */")
            print(f"{entryType} peek{name}({myType} {a}x) {{ {c}")
            print(f'    DEBUG("peek{name}(%p)", x);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x->size == 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return x->entries[x->size - 1]; {c}")
            print(f"}} {c}\n")

    def printPeeknFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeeknFunction')
            print(f"/**")
            print(f" * If `n` is negative, returns the value at `sp - |n|`,")
            print(f" * otherwise returns the value at `n`.")
            print(f" */")
            print(f"{entryType} peekn{name}({myType} {a}x, int offset) {{ {c}")
            print(f'    DEBUG("peekn{name}(%p, %d)", x, offset);')
            print(f"    if (offset < 0) offset = ((int) x->size) + offset; {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (offset >= (int) x->size) {{ {c}")
            print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->size); {c}')
            print(f"    }} {c}")
            print(f"    if (offset < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return x->entries[offset]; {c}")
            print(f"}} {c}\n")

    def printPokeFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPokeFunction')
            print(f"/**")
            print(f" * If `n` is negative, replaces the value at `sp - |n|`,")
            print(f" * otherwise replaces the value at `n`.")
            print(f" */")
            print(f"void poke{name}({myType} {a}x, int offset, {entryType} val) {{ {c}")
            print(f'    DEBUG("poke{name}(%p, %d)", x, offset);')
            print(f"    if (offset < 0) offset = ((int) x->size) + offset; {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (offset >= (int) x->size) {{ {c}")
            print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->size); {c}')
            print(f"    }} {c}")
            print(f"    if (offset < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    x->entries[offset] = val; {c}")
            print(f"}} {c}\n")

    def printMarkFunction(self, catalog):
        decl = self.getMarkSignature(catalog)
        c = self.comment('printMarkFunction')
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (x == NULL) return; {c}")
            print(f"    if (MARKED(x)) return; {c}")
            print(f"    MARK(x); {c}")
        self.printMarkFunctionBody(catalog)
        print(f"}} {c}")
        print("")

    def printMarkFunctionBody(self, catalog):
        if self.dimension == 1:
            self.printMark1dFunctionBody(catalog)
        else:
            self.printMark2dFunctionBody(catalog)

    def printMark1dFunctionBody(self, catalog):
        c = self.comment('print1dFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    for (Index i = 0; i < x{a}size; i++) {{ {c}")
        self.entries.printMarkArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")

    def printMark2dFunctionBody(self, catalog):
        c = self.comment('print2dFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    Index size = x{a}width * x{a}height; {c}")
        print(f"    for (Index i = 0; i < size; i++) {{ {c}")
        self.entries.printMarkArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl=self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printCompareDeclaration(self, catalog):
        c = self.comment('printCompareDeclaration')
        decl=self.getCompareSignature(catalog)
        print(f"{decl}; {c}")

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def getCtype(self, astType, catalog):
        return f"{astType} *"

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printCountDeclaration')
        a = '.' if self.isInline(catalog) else '->'
        print(f'static inline Index count{myName}({myType} x) {{ {c}')
        if self.dimension == 1:
            print(f'    return x{a}size; {c}')
        else:
            print(f'    return x{a}width * x{a}height; {c}')
        print(f'}} {c}')
        print('')

    def getExtraCmpFargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            ctype = self.getCtype(self.extraCmpArgs[name], catalog)
            extra += [f"{ctype}{name}"]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def getExtraCmpAargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            extra += [name]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return f"bool eq{myName}({myType} a, {myType} b{extraCmpArgs})"

    def printCompareFunction(self, catalog):
        c = self.comment('printCompareFunction')
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for")
            print("// {decl}".format(decl=self.getCompareSignature(catalog)))
            print("")
            return
        myName = self.getName()
        decl = self.getCompareSignature(catalog)
        a = '.' if self.isInline(catalog) else '->'
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (a == b) return true; {c}")
            print(f"    if (a == NULL || b == NULL) return false; {c}")
        if self.dimension == 1:
            print(f"    if (a{a}size != b{a}size) return false; {c}")
            print(f"    for (Index i = 0; i < a{a}size; i++) {{ {c}")
            self.entries.printCompareArrayLine(self.isInline(catalog), catalog, "i", 2)
            print(f"    }} {c}")
        else:
            print(f"    if (a{a}width != b{a}width || a{a}height != b{a}height) return false; {c}")
            print(f"    for (Index i = 0; i < (a{a}width * a{a}height); i++) {{ {c}")
            self.entries.printCompareArrayLine(self.isInline(catalog), catalog, "i", 2)
            print(f"    }} {c}")
        print(f"    return true; {c}")
        print(f"}} {c}\n")

    def printCopyFunction(self, catalog):
        c = self.comment('printCopyFunction')
        decl = self.getCopySignature(catalog)
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"{decl} {{ {c}")
        print(f"    if (o == NULL) return NULL; {c}")
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    int save = PROTECT(x); {c}")
        self.printCopyFunctionBody(catalog)
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}\n")

    def printCopyFunctionBody(self, catalog):
        if self.dimension == 1:
            self.print1dCopyFunctionBody(catalog)
        else:
            self.print2dCopyFunctionBody(catalog)

    def print1dCopyFunctionBody(self, catalog):
        c = self.comment('print1dCopyFunctionBody')
        entryType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        print(f"    if (o->entries != NULL) {{ {c}")
        print(f"        extend{myName}(x, o->size); {c}")
        if self.entries.isInline(catalog):
            print(f"        COPY_ARRAY({entryType}, x->entries, o->entries, o->size); {c}")
            print(f"        x->size = o->size; {c}")
        else:
            print(f"        x->size = 0; {c}")
            print(f"        for (Index i = 0; i < o->size; i++) {{ {c}")
            self.entries.printCopyArrayLine(catalog, "i", 3)
            print(f"            x->size++; {c}")
            print(f"        }} {c}")
        print(f"    }} else {{ {c}")
        print(f"        x->size = 0; {c}")
        print(f"    }} {c}")

    def print2dCopyFunctionBody(self, catalog):
        c = self.comment('print2dCopyFunctionBody')
        print(f"    if (o->entries != NULL) {{ {c}")
        print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->width * x->height); {c}")
        print(f"        x->width = 0; {c}")
        print(f"        x->height = 0; {c}")
        print(f"        for (Index i = 0; i < (o->width * o->height); i++) {{ {c}")
        self.entries.printCopyArrayLine(catalog, "i", 3)
        print(f"        }} {c}")
        print(f"        x->height = o->height; {c}")
        print(f"        x->width = o->width; {c}")
        print(f"    }} {c}")

    def printPrintFunction(self, catalog):
        myName = self.getName()
        decl = self.getPrintSignature(catalog)
        a = '.' if self.isInline(catalog) else '->'
        c = self.comment('printPrintFunction')
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        if not self.isInline(catalog):
            print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        if self.tagged:
            print(f'    eprintf("<<%s>>", x{a}_tag); {c}')
        if self.dimension == 1:
            print(f'    eprintf("{myName}(%d)[\\n", x{a}size); {c}')
        else:
            print(f'    eprintf("{myName}(%d * %d)[\\n", x{a}width, x{a}height); {c}')
        self.printPrintFunctionBody(catalog)
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}")
        print("")

    def printPrintFunctionBody(self, catalog):
        if self.dimension == 1:
            self.print1dPrintFunctionBody(catalog)
        else:
            self.print2dPrintFunctionBody(catalog)

    def print1dPrintFunctionBody(self, catalog):
        c = self.comment('print1dPrintFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    for (Index i = 0; i < x{a}size; i++) {{ {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f'        eprintf("\\n"); {c}')
        print(f"    }} {c}")

    def print2dPrintFunctionBody(self, catalog):
        c = self.comment('print2dPrintFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    for (Index i = 0; i < x{a}height; i++) {{ {c}")
        print(f"        pad(depth); {c}")
        print(f'        eprintf("[\\n"); {c}')
        print(f"        for (Index j = 0; j < x{a}width; j++) {{ {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, f"i * x{a}width + j", 3)
        print(f'            eprintf("\\n"); {c}')
        print(f"        }} {c}")
        print(f"        pad(depth); {c}")
        print(f'        eprintf("]\\n"); {c}')
        print(f"    }} {c}")

    def printFreeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printFreeObjCase')
        name = self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'free{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

    def printMarkObjCase(self, catalog):
        c = self.comment('printMarkObjCase')
        objType = self.getObjType()
        name = self.getName()
        pad(2)
        print(f'case {objType}: {c}')
        pad(3)
        print(f'mark{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

    def printTypeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        objType = self.getObjType()
        name = self.getName()
        c = self.comment('printTypeObjCase')
        pad(2)
        print(f'case {objType}: {c}')
        pad(3)
        print(f'return "{name}"; {c}')

    def printMarkHashField(self, depth):
        c = self.comment('printMarkHashField')
        myName = self.getName()
        pad(depth)
        print(f'mark{myName}(*({myName} **)ptr); {c}')

    def printMarkField(self, isInline, field, depth, prefix=''):
        c = self.comment('printMarkField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f"mark{myName}(x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f"return PROTECT(x{a}{prefix}{field}); {c}")

    def getIterator1DDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        myContainedType = self.entries.getTypeDeclaration(catalog)
        return f'bool iterate{myName}({myType} table, Index *i, {myContainedType} *res, bool *more)'

    def getIterator2DDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        myContainedType = self.entries.getTypeDeclaration(catalog)
        return f'bool iterate{myName}({myType} table, Index *x, Index *y, {myContainedType} *res, bool *more_x, bool *more_y)'

    def printIteratorDeclaration(self, catalog):
        if self.dimension == 2:
            self.printIterator2DDeclaration(catalog)
        else:
            self.printIterator1DDeclaration(catalog)

    def printIterator1DDeclaration(self, catalog):
        c = self.comment('printIterator1DDeclaration')
        decl = self.getIterator1DDeclaration(catalog)
        print(f'{decl}; {c}')

    def printIterator2DDeclaration(self, catalog):
        c = self.comment('printIterator2DDeclaration')
        decl = self.getIterator2DDeclaration(catalog)
        print(f'{decl}; {c}')

    def printIteratorFunction(self, catalog):
        if self.dimension == 2:
            self.printIterator2DFunction(catalog)
        else:
            self.printIterator1DFunction(catalog)

    def printIterator1DFunction(self, catalog):
        c = self.comment('printIterator1DFunction')
        decl = self.getIterator1DDeclaration(catalog)
        a = '.' if self.isInline(catalog) else '->'
        print(f'{decl} {{ {c}')
        print(f'    if (*i >= table{a}size) {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = (*i + 1 < table{a}size); {c}')
        print(f'        }} {c}')
        print(f'        if (res != NULL) {{ {c}')
        print(f'            *res = table{a}entries[*i]; {c}')
        print(f'        }} {c}')
        print(f'        *i = *i + 1; {c}')
        print(f'        return true; {c}')
        print(f'    }} {c}')
        print(f'}} {c}')
        print('')

    def printIterator2DFunction(self, catalog):
        c = self.comment('printIterator2DFunction')
        decl = self.getIterator2DDeclaration(catalog)
        a = '.' if self.isInline(catalog) else '->'
        print(f'{decl} {{ {c}')
        print(f'    if (*x >= table{a}width) {{ {c}')
        print(f'        if (more_x != NULL) {{ {c}')
        print(f'            *more_x = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else if (*y >= table{a}height) {{ {c}')
        print(f'        if (more_y != NULL) {{ {c}')
        print(f'            *more_y = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else {{ {c}')
        print(f'        if (more_x != NULL) {{ {c}')
        print(f'            *more_x = (*x + 1 < table{a}width); {c}')
        print(f'        }} {c}')
        print(f'        if (more_y != NULL) {{ {c}')
        print(f'            *more_y = (*y + 1 < table{a}height); {c}')
        print(f'        }} {c}')
        print(f'        if (res != NULL) {{ {c}')
        print(f'            *res = table{a}entries[*x * table{a}width + *y]; {c}')
        print(f'        }} {c}')
        print(f'        if (*x + 1 == table->width) {{ {c}')
        print(f'            *x = 0; {c}')
        print(f'            *y = *y + 1; {c}')
        print(f'        }} else {{ {c}')
        print(f'            *x = *x + 1; {c}')
        print(f'        }} {c}')
        print(f'        return true; {c}')
        print(f'    }} {c}')
        print(f'}} {c} {c}')
        print('')

    def isArray(self):
        return True

class SimpleStack(SimpleArray):
    """
    Stacks don't expose a size, instead they have a frame pointer and a stack pointer
    where the stack pointer is an offset from the frame pointer rather than an absolute
    value. They have an additional pushable and popable array of frame pointers and
    stack pointers.
    """
    def __init__(self, name, data):
        super().__init__(name, data)
        if self.dimension != 1:
            raise Exception("stacks must have dimension 1")

    def comment(self, method):
        return f'// SimpleStack.{method}'

    def printIndexFields(self):
        c = self.comment('printIndexFields')
        print(f"    Index frame; {c}")
        print(f"    Index offset; {c}")
        print(f"    Index entries_capacity; {c}")
        print(f"    Index frames_capacity; {c}")
        print(f"    Index frames_index; {c}")

    def printExtraStackEntries(self):
        c = self.comment('printExtraStackEntries')
        print(f"    StackFrame *frames; {c}")

    def printNullEntries(self):
        c = self.comment('printNullEntries')
        print(f"    x->entries = NULL; {c}")
        print(f"    x->frames = NULL; {c}")

    def printZeroCapacities(self):
        c = self.comment('printZeroCapacities')
        print(f"    x->frame = 0; {c}")
        print(f"    x->offset = 0; {c}")
        print(f"    x->entries_capacity = 0; {c}")
        print(f"    x->frames_capacity = 0; {c}")
        print(f"    x->frames_index = 0; {c}")

    def printFreeFunction(self, catalog):
        myName = self.getName()
        decl = decl=self.getFreeSignature(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printFreeFunction')
        print(f"{decl} {{ {c}")
        print(f"    FREE_ARRAY({entryType}, x->entries, x->entries_capacity); {c}")
        print(f"    FREE_ARRAY(StackFrame, x->frames, x->frames_capacity); {c}")
        print(f"    FREE(x, {myName}); {c}")
        print(f"}} {c}")
        print("")

    def printInitEntries(self, catalog):
        c = self.comment('printInitEntries')
        print(f"    x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, 8); {c}")
        print(f"    x->entries_capacity = 8; {c}")
        print(f"    x->frames = NEW_ARRAY(StackFrame, 8); {c}")
        print(f"    x->frames_capacity = 8; {c}")

    def printExtendDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printExtendDeclaration')
        print(f"void extend{name}Entries({myType} {a}obj, Index size); {c}")
        print(f"void extend{name}Frames({myType} {a}obj, Index size); {c}")

    def printSizeDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printSizeDeclaration')
        print(f"static inline Index totalSize{name}({myType} {a}obj) {{ return obj->frame + obj->offset; }} {c}")
        print(f"static inline Index frameSize{name}({myType} {a}obj) {{ return obj->frames_index; }} {c}")
        print(f"static inline Index offset{name}({myType} {a}obj) {{ return obj->offset; }} {c}")

    def printPushDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPushDeclaration')
        print(f"Index push{name}Entry({myType} {a}x, {entryType} entry); {c}")
        print(f"Index let{name}Frame({myType} {a}x); {c}")
        print(f"void push{name}Frame({myType} {a}x); {c}")

    def printCopyEntriesDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printCopyEntriesDeclaration')
        print(f"void copyCurrent{name}Entries({myType} dest, {myType} src); {c}")
        print(f"void copyAll{name}Entries({myType} dest, {myType} src); {c}")
        print(f"void copy{name}Continuation({myType} dest, {myType} src); {c}")

    def printPopDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopDeclaration')
        print(f"{entryType} pop{name}Entry({myType} {a}x); {c}")
        print(f"void pop{name}Frame({myType} {a}x); {c}")

    def printPopnDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopnDeclaration')
        print("#ifdef SAFETY_CHECKS")
        print(f"void popn{name}({myType} {a}obj, int n); {c}")
        print("#else")
        print(f"static inline void popn{name}({myType} {a}obj, int n) {{ obj->offset -= n; }}; {c}")
        print("#endif")

    def printClearDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printClearDeclaration')
            print(f"static inline void clear{name}Entries({myType} {a}x) {{ x->offset = 0; }}; {c}")
            print(f"static inline void clear{name}Frames({myType} {a}x) {{ x->frames_index = x->offset = x->frame = 0; }}; {c}")

    def printExtendFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            c = self.comment('printExtendFunction')
            a = '*' if self.isInline(catalog) else ''
            print(f"/**")
            print(f" * Ensures that `x` has at least an absolute entries_capacity of `size`.")
            print(f" */")
            print(f"void extend{name}Entries({myType} {a}x, Index size) {{ {c}")
            print(f'    DEBUG("extend{name}Entries(%p, %u)", x, size);')
            print(f"    if (size > 0) {{ {c}")
            print(f"        size = size < 8 ? 8 : size; {c}")
            print(f"        if (x->entries_capacity == 0) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"            if (x->entries != NULL) {{ {c}")
            print(f'                cant_happen("{name} non-null entries with zero capacity"); {c}')
            print(f"            }} {c}")
            print(f"#endif")
            print(f"            x->entries = NEW_ARRAY({entryType}, size); {c}")
            print(f"            x->entries_capacity = size; {c}")
            print(f"        }} else {{ {c}")
            print(f"            while (size > x->entries_capacity) {{ {c}")
            print(f"                x->entries = GROW_ARRAY({entryType}, x->entries, x->entries_capacity, x->entries_capacity *2); {c}")
            print(f"                x->entries_capacity *= 2; {c}")
            print(f"            }} {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}")
            print(f"")
            print(f"/**")
            print(f" * Ensures that `x` has at least a frames_capacity of `size`.")
            print(f" */")
            print(f"void extend{name}Frames({myType} {a}x, Index size) {{ {c}")
            print(f'    DEBUG("extend{name}Frames(%p, %u)", x, size);')
            print(f"    if (size > 0) {{ {c}")
            print(f"        size = size < 8 ? 8 : size; {c}")
            print(f"        if (x->frames_capacity == 0) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"            if (x->frames != NULL) {{ {c}")
            print(f'                cant_happen("{name} non-null frames with zero capacity"); {c}')
            print(f"            }} {c}")
            print(f"#endif")
            print(f"            x->frames = NEW_ARRAY(StackFrame, size); {c}")
            print(f"            x->frames_capacity = size; {c}")
            print(f"        }} else {{ {c}")
            print(f"            while (size > x->frames_capacity) {{ {c}")
            print(f"                x->frames = GROW_ARRAY(StackFrame, x->frames, x->frames_capacity, x->frames_capacity *2); {c}")
            print(f"                x->frames_capacity *= 2; {c}")
            print(f"            }} {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}")
            print(f"")

    def printPushFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printPushFunction')
        a = '*' if self.isInline(catalog) else ''
        print(f"/**")
        print(f" * Pushes `entry` on to `x`, extending `x` if required.")
        print(f" * Returns the stack pointer after the push.")
        print(f" */")
        print(f"Index push{name}Entry({myType} {a}x, {entryType} entry) {{ {c}")
        print(f'    DEBUG("push{name}Entry(%p)", x);')
        print(f"    extend{name}Entries(x, x->frame + x->offset + 1); {c}")
        print(f"    x->entries[x->frame + x->offset] = entry; {c}")
        print(f"    x->offset++; {c}")
        print(f"    return x->offset - 1; {c}")
        print(f"}} {c}")
        print(f"")
        print(f"/**")
        print(f" * Duplicates the top frame.")
        print(f" */")
        print(f"Index let{name}Frame({myType} {a}x) {{ {c}")
        print(f'    DEBUG("let{name}Frame(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x == NULL) {{ {c}")
        print(f'        cant_happen("{name} null stack"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    extend{name}Frames(x, x->frames_index + 1); {c}")
        print(f"    x->frames[x->frames_index++] = (StackFrame) {{.frame = x->frame, .offset = x->offset }}; {c}")
        print(f"    extend{name}Entries(x, x->frame + x->offset * 2); {c}")
        print(f"    COPY_ARRAY({entryType}, &x->entries[x->frame + x->offset], &x->entries[x->frame], x->offset); {c}")
        print(f"    x->frame += x->offset; {c}")
        print(f"    return x->offset; {c}")
        print(f"}} {c}")
        print(f"")
        print(f"/**")
        print(f" * Creates new empty top frame.")
        print(f" */")
        print(f"void push{name}Frame({myType} {a}x) {{ {c}")
        print(f'    DEBUG("push{name}Frame(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x == NULL) {{ {c}")
        print(f'        cant_happen("{name} null stack"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    extend{name}Frames(x, x->frames_index + 1); {c}")
        print(f"    x->frames[x->frames_index++] = (StackFrame) {{.frame = x->frame, .offset = x->offset }}; {c}")
        print(f"    x->frame += x->offset; {c}")
        print(f"    x->offset = 0; {c}")
        print(f"}} {c}")
        print(f"")

    def printPopFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopFunction')
        print(f"/**")
        print(f" * Pops the top entry from `x` and returns it.")
        print(f" */")
        print(f"{entryType} pop{name}Entry({myType} {a}x) {{ {c}")
        print(f'    DEBUG("pop{name}Entry(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x->offset == 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    return x->entries[--(x->offset) + x->frame]; {c}")
        print(f"}} {c}")
        print(f"")
        print(f"/**")
        print(f" * Pops the top frame from `x`.")
        print(f" */")
        print(f"void pop{name}Frame({myType} {a}x) {{ {c}")
        print(f'    DEBUG("pop{name}Frame(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x->frames_index == 0) {{ {c}")
        print(f'        cant_happen("{name} stack frame underflow"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    x->frames_index--; {c}")
        print(f"    x->frame = x->frames[x->frames_index].frame; {c}")
        print(f"    x->offset = x->frames[x->frames_index].offset; {c}")
        print(f"}} {c}")
        print(f"")

    def printPopnFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopnFunction')
        print(f"/**")
        print(f" * Discards the top `n` entries from `x`.")
        print(f" */")
        print(f"#ifdef SAFETY_CHECKS")
        print(f"void popn{name}({myType} {a}x, int n) {{ {c}")
        print(f'    DEBUG("popn{name}(%p, %d)", x, n);')
        print(f"    if (((int) x->offset) - n < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d/%u", n, x->offset); {c}')
        print(f"    }} {c}")
        print(f"    x->offset -= n; {c}")
        print(f"}} {c}")
        print(f"#endif")
        print("")

    def printMoveFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printMoveFunction')
        print(f"/**")
        print(f" * Safe move `n` entries from `sp - n` to `b`,")
        print(f" * sets sp to `b + n`.")
        print(f" */")
        print(f"void move{name}({myType} {a}x, int b, int n) {{ {c}")
        print(f'    DEBUG("move{name}(%p, %d, %d)", x, b, n);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (((int) x->offset) - n < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d/%u", n, x->offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    if (n > 0) {{ {c}")
        print(f"        extend{name}Entries(x, x->frame + b + n); {c}")
        print(f"        MOVE_ARRAY({entryType}, &x->entries[x->frame + b], &x->entries[x->frame + x->offset - n], n); {c}")
        print(f"    }} {c}")
        print(f"    x->offset = (Index) (b + n); {c}")
        print(f"}} {c}\n")

    def printPushnFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPushnFunction')
        print(f"/**")
        print(f" * Pushes `n` copies of `entry` on to `x`.")
        print(f" */")
        print(f"void pushn{name}({myType} {a}x, int n, {entryType} entry) {{ {c}")
        print(f'    DEBUG("pushn{name}(%p, %d)", x, n);')
        print(f"    if (n > 0) {{ {c}")
        print(f"        extend{name}Entries(x, x->frame + x->offset + n); {c}")
        print(f"        while (n-- > 0) {{ {c}")
        print(f"            x->entries[x->frame + x->offset++] = entry; {c}")
        print(f"        }} {c}")
        print(f"    }} {c}")
        print(f"}} {c}\n")

    def printCopyTopFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printCopyTopFunction')
        print(f"/**")
        print(f" * Copies the top `n` entries from `src` to the base of `dest`,")
        print(f" * sets `dest` offset (sp) to `n`.")
        print(f" */")
        print(f"void copyTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
        print(f'    DEBUG("copyTop{name}(%p, %p, %d)", dest, src, n);')
        print(f"    if (n > 0) {{ {c}")
        print(f"        extend{name}Entries(dest, dest->frame + n); {c}")
        print(f"        COPY_ARRAY({entryType}, &dest->entries[dest->frame], &src->entries[src->frame + src->offset - n], n); {c}")
        print(f"    }} {c}")
        print(f"    dest->offset = n; {c}")
        print(f"}} {c}\n")

    def printCopyExceptTopFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printCopyExceptTopFunction')
        print(f"/**")
        print(f" * Copies all but top `n` entries from `src` to the base of `dest`,")
        print(f" * sets `dest` sp to `src->sp - n`.")
        print(f" */")
        print(f"void copyExceptTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
        print(f'    DEBUG("copyExceptTop{name}(%p, %p, %d)", dest, src, n);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (((int) src->offset) - n < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d/%u", n, src->offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    if (((Index) n) < src->offset) {{ {c}")
        print(f"        extend{name}Entries(dest, dest->frame + src->offset - n); {c}")
        print(f"        COPY_ARRAY({entryType}, &dest->entries[dest->frame], &src->entries[src->frame], src->offset - n); {c}")
        print(f"    }} {c}")
        print(f"    dest->offset = src->offset - n; {c}")
        print(f"}} {c}\n")

    def printCopyEntriesFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printCopyEntriesFunction')
        print(f'/**')
        print(f" * Copies the curent frame's entries from `src` to `dest`,")
        print(f' * sets `dest` offset to `src->offset`.')
        print(f' */')
        print(f'void copyCurrent{name}Entries({myType} {a}dest, {myType} {a}src) {{ {c}')
        print(f'    DEBUG("copyCurrent{name}Entries(%p, %p)", dest, src);')
        print(f'    extend{name}Entries(dest, dest->frame + src->offset); {c}')
        print(f'    COPY_ARRAY({entryType}, &dest->entries[dest->frame], &src->entries[src->frame], src->offset); {c}')
        print(f'    dest->offset = src->offset; {c}')
        print(f'}} {c}')
        print(f'')
        print(f'/**')
        print(f' * Copies all entries from `src` to `dest`,')
        print(f' */')
        print(f'void copyAll{name}Entries({myType} {a}dest, {myType} {a}src) {{ {c}')
        print(f'    DEBUG("copyAll{name}Entries(%p, %p)", dest, src);')
        print(f'    extend{name}Entries(dest, src->frame + src->offset); {c}')
        print(f'    extend{name}Frames(dest, src->frames_index); {c}')
        print(f'    COPY_ARRAY({entryType}, dest->entries, src->entries, src->frame + src->offset); {c}')
        print(f'    COPY_ARRAY(StackFrame, dest->frames, src->frames, src->frames_index); {c}')
        print(f'    dest->frames_index = src->frames_index; {c}')
        print(f'    dest->frame = src->frame; {c}')
        print(f'    dest->offset = src->offset; {c}')
        print(f'}} {c}')
        print(f'')
        print(f'/**')
        print(f' * Copies all entries from `src` to `dest`, except the current frame,')
        print(f' */')
        print(f'void copy{name}Continuation({myType} {a}dest, {myType} {a}src) {{ {c}')
        print(f'    DEBUG("copy{name}Continuation(%p, %p)", dest, src);')
        print(f'    if (src->frames_index == 0) {{ {c}')
        print(f'        dest->frames_index = dest->frame = dest->offset = 0; {c}')
        print(f'        return; {c}')
        print(f'    }} {c}')
        print(f'    StackFrame sf = src->frames[src->frames_index - 1]; {c}')
        print(f'    Index newSize = sf.frame + sf.offset; {c}')
        print(f'    extend{name}Entries(dest, newSize); {c}')
        print(f'    extend{name}Frames(dest, src->frames_index); {c}')
        print(f'    COPY_ARRAY({entryType}, dest->entries, src->entries, newSize); {c}')
        print(f'    COPY_ARRAY(StackFrame, dest->frames, src->frames, src->frames_index - 1); {c}')
        print(f'    dest->frames_index = src->frames_index - 1; {c}')
        print(f'    dest->frame = sf.frame; {c}')
        print(f'    dest->offset = sf.offset; {c}')
        print(f'}} {c}')
        print(f'')

    def printPeekFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPeekFunction')
        print(f"/**")
        print(f" * Returns the value at the top of `x`.")
        print(f" */")
        print(f"{entryType} peek{name}({myType} {a}x) {{ {c}")
        print(f'    DEBUG("peek{name}(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x->offset == 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    return x->entries[x->frame + x->offset - 1]; {c}")
        print(f"}} {c}\n")

    def printPeeknFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPeeknFunction')
        print(f"/**")
        print(f" * If `n` is negative, returns the value at `sp - |n|`,")
        print(f" * otherwise returns the value at `n`.")
        print(f" */")
        print(f"{entryType} peekn{name}({myType} {a}x, int offset) {{ {c}")
        print(f'    DEBUG("peekn{name}(%p, %d)", x, offset);')
        print(f"    if (offset < 0) offset = ((int) x->offset) + offset; {c}")
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (offset >= (int) x->offset) {{ {c}")
        print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->offset); {c}')
        print(f"    }} {c}")
        print(f"    if (offset < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    return x->entries[x->frame + offset]; {c}")
        print(f"}} {c}\n")

    def printPokeFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPokeFunction')
        print(f"/**")
        print(f" * If `n` is negative, replaces the value at `sp - |n|`,")
        print(f" * otherwise replaces the value at `n`.")
        print(f" */")
        print(f"void poke{name}({myType} {a}x, int offset, {entryType} val) {{ {c}")
        print(f'    DEBUG("poke{name}(%p, %d)", x, offset);')
        print(f"    if (offset < 0) offset = ((int) x->offset) + offset; {c}")
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (offset >= (int) x->offset) {{ {c}")
        print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->offset); {c}')
        print(f"    }} {c}")
        print(f"    if (offset < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    x->entries[x->frame + offset] = val; {c}")
        print(f"}} {c}\n")

    def printMark1dFunctionBody(self, catalog):
        c = self.comment('print1dFunctionBody')
        print(f'    DEBUG("markStack(%p, %d + %d)", x, x->frame, x->offset); {c}')
        print(f"    for (Index i = 0; i < x->frame + x->offset; i++) {{ {c}")
        self.entries.printMarkArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printCountDeclaration')
        print(f'static inline Index count{myName}Frame({myType} x) {{ {c}')
        print(f'    return x->offset; {c}')
        print(f'}} {c}')
        print('')
        print(f'static inline Index count{myName}Entries({myType} x) {{ {c}')
        print(f'    return x->frame + x->offset; {c}')
        print(f'}} {c}')
        print('')
        print(f'static inline Index count{myName}Frames({myType} x) {{ {c}')
        print(f'    return x->frames_index; {c}')
        print(f'}} {c}')
        print('')

    def printCompareFunction(self, catalog):
        c = self.comment('printCompareFunction')
        decl = self.getCompareSignature(catalog)
        if self.bespokeCmpImplementation:
            print(f"// Bespoke implementation required for {decl}")
            print("")
            return
        myName = self.getName()
        print(f"{decl} {{ {c}")
        print(f"    if (a == b) return true; {c}")
        print(f"    if (a == NULL || b == NULL) return false; {c}")
        print(f"    if (a->frame != b->frame) return false; {c}")
        print(f"    if (a->offset != b->offset) return false; {c}")
        print(f"    if (a->frames_index != b->frames_index) return false; {c}")
        print(f"    for (Index i = 0; i < a->frames_index; i++) {{ {c}")
        print(f"        if (a->frames[i].frame != b->frames[i].frame) return false; {c}")
        print(f"        if (a->frames[i].offset != b->frames[i].offset) return false; {c}")
        print(f"    }} {c}")
        print(f"    for (Index i = 0; i < a->frame + a->offset; i++) {{ {c}")
        self.entries.printCompareArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")
        print(f"    return true; {c}")
        print(f"}} {c}")
        print("")

    def print1dCopyFunctionBody(self, catalog):
        c = self.comment('print1dCopyFunctionBody')
        entryType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        print(f"    if (o->entries != NULL) {{ {c}")
        print(f"        extend{myName}Entries(x, o->frame + o->offset); {c}")
        if self.entries.isInline(catalog):
            print(f"        COPY_ARRAY({entryType}, x->entries, o->entries, o->frame + o->offset); {c}")
        else:
            print(f"        for (Index i = 0; i < o->frame + o->offset; i++) {{ {c}")
            self.entries.printCopyArrayLine(catalog, "i", 3)
            print(f"        }} {c}")
        print(f"        x->frame = o->frame; {c}")
        print(f"        x->offset = o->offset; {c}")
        print(f"    }} {c}")
        print(f"    if (o->frames != NULL) {{ {c}")
        print(f"        extend{myName}Frames(x, o->frames_index); {c}")
        print(f"        COPY_ARRAY(StackFrame, x->frames, o->frames, o->frames_index); {c}")
        print(f"        x->frames_index = o->frames_index; {c}")
        print(f"    }} {c}")

    def printPrintFunction(self, catalog):
        myName = self.getName()
        decl = self.getPrintSignature(catalog)
        c = self.comment('printPrintFunction')
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        if self.tagged:
            print(f'    eprintf("<<%s>>", x->_tag); {c}')
        print(f'    eprintf("{myName}(%d)[\\n", x->frames_index); {c}')
        print(f"    for (Index i = 0; i < x->frames_index; i++) {{ {c}")
        print(f"        for (Index j = 0; j < x->frames[i].offset; j++) {{ {c}")
        print(f"            Index k = x->frames[i].frame + j; {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, "k", 3)
        print(f'            eprintf("\\n"); {c}')
        print(f"        }} {c}")
        print(f"        pad(depth + 1); {c}")
        print(f'        eprintf("---\\n"); {c}')
        print(f"    }} {c}")
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}")
        print("")

    def printIterator1DFunction(self, catalog):
        c = self.comment('printIterator1DFunction')
        decl = self.getIterator1DDeclaration(catalog)
        print(f'{decl} {{ {c}')
        print(f'    if (*i >= table->offset) {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = (*i + 1 < table->offset); {c}')
        print(f'        }} {c}')
        print(f'        if (res != NULL) {{ {c}')
        print(f'            *res = table->entries[table->frame + *i]; {c}')
        print(f'        }} {c}')
        print(f'        *i = *i + 1; {c}')
        print(f'        return true; {c}')
        print(f'    }} {c}')
        print(f'}} {c}')
        print('')

class InlineArray(SimpleArray):
    """
    Call-by-value inline Array structures declared in the yaml
    """
    def __init__(self, name, data):
        super().__init__(name, data)

    def isInline(self, catalog):
        return True

    def printNewFunction(self, catalog):
        pass

    def printNewDeclaration(self, catalog):
        pass

    def printFreeFunction(self, catalog):
        pass

    def printFreeDeclaration(self, catalog):
        pass

    def objTypeArray(self):
        return []

    def comment(self, method):
        return f'// InlineArray.{method}'

    def printCopyDeclaration(self, catalog):
        c = self.comment('printCopyDeclaration')
        typeName = self.getTypeDeclaration(catalog)
        myName = self.getName()
        print(f'static inline {typeName} copy{myName}({typeName} o) {{ return o; }}; {c}')

    def printCopyFunction(self, catalog):
        pass

    def printInitDeclaration(self, catalog):
        if self.dimension != 1:
            return
        typeName = self.getTypeDeclaration(catalog)
        myName = self.getName()
        c = self.comment('printInitDeclaration')
        print(f'void init{myName}({typeName} *x, Index size); {c}')

    def printInitFunction(self, catalog):
        if self.dimension != 1:
            return
        typeName = self.getTypeDeclaration(catalog)
        myName = self.getName()
        c = self.comment('printInitFunction')
        print(f'void init{myName}({typeName} *x, Index size) {{ {c}')
        print(f"    x->size = 0; {c}")
        print(f"    x->capacity = 0; {c}")
        print(f"    x->entries = NULL; {c}")
        print(f"    if (size > 0) {{ {c}")
        print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, size); {c}")
        print(f"        x->capacity = size; {c}")
        print(f'    }} {c}')
        print(f'}} {c}')
        print("")

    def printMarkObjCase(self, catalog):
        pass

class SimpleVector(Base):
    """
    Simple vectors declared directly in the yaml.
    Vectors are fixed-size arrays with a simpler
    and more efficient flat implementation
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.entries = SimpleField(self.name, "entries", data["entries"])
        else:
            raise ValueError(f"SimpleVector {name} must have 'data' field with 'entries'")

    def isVector(self):
        return True

    def printTypedef(self, catalog):
        self.noteTypedef()
        c = self.comment('printTypedef')
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        print(f"    Index size; {c}")
        self.entries.printVectorTypedefLine(catalog)
        print(f"}} {name}; {c}")
        print("")

    def printMermaid(self, catalog):
        myName = self.getName()
        print(f'{myName}["{myName}[]"] --entries--> {self.entries.getObjName(catalog)}')

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"{myType} new{myName}(int size)"

    def comment(self, method):
        return f'// SimpleVector.{method}'

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl=self.getNewSignature(catalog)
        print(f"{decl}; {c}")

    def printNewFunction(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        fieldType = self.entries.getTypeDeclaration(catalog)
        decl = self.getNewSignature(catalog)
        c = self.comment('printNewFunction')
        print(f"{decl} {{ {c}")
        print(f"    {myType} x = NEW_VECTOR(size, {myName}, {fieldType}, {myObjType}); {c}")
        print(f'    DEBUG("new {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName}) + size * sizeof({fieldType})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    x->size = size; {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printCopyField(self, isInline, field, depth, prefix=''):
        myName=self.getName()
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f'x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def getTypeDeclaration(self, catalog):
        return "struct {name} *".format(name=self.getName())

    def getDefineValue(self):
        return 'x'

    def getDefineArg(self):
        return 'x'

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        myName=self.getName()
        a = '.' if isInline else '->'
        pad(depth)
        print(f'print{myName}(x{a}{prefix}{field}, depth+1); {c}')

    def printCompareField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        myName=self.getName()
        extraCmpArgs = self.getExtraCmpAargs(catalog)
        a = '.' if isInline else '->'
        pad(depth)
        print(f"if (!eq{myName}(a{a}{prefix}{field}, b{a}{prefix}{field}{extraCmpArgs})) return false; {c}")

    def getExtraCmpFargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            ctype = self.getCtype(self.extraCmpArgs[name], catalog)
            extra += [f"{ctype}{name}"]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def getExtraCmpAargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            extra += [name]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def objTypeArray(self):
        return [ self.getObjType() ]

    def getObjType(self):
        return ('objtype_' + self.getName()).upper()

    def printCopyDeclaration(self, catalog):
        decl = self.getCopySignature(catalog)
        c = self.comment('printCopyDectaration')
        print(f"{decl}; {c}")

    def printCopyFunction(self, catalog):
        c = self.comment('printCopyFunction')
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        fieldType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        decl = self.getCopySignature(catalog)
        print(f"{decl} {{ {c}")
        print(f"    if (o == NULL) return NULL; {c}")
        print(f"    {myType} x = NEW_VECTOR(o->size, {myName}, {fieldType}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    int save = PROTECT(x); {c}")
        if self.entries.isInline(catalog):
            print(f"    COPY_ARRAY({fieldType}, x->entries, o->entries, o->size); {c}")
        else:
            print(f"    for (Index i = 0; i < o->size; ++i) {{ {c}")
            self.entries.printCopyArrayLine(catalog, "i", 3)
            print(f"    }} {c}")
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def getCopySignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"{myType} copy{myName}({myType} o)"

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl=self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printCompareDeclaration(self, catalog):
        c = self.comment('printCompareDeclaration')
        decl=self.getCompareSignature(catalog)
        print(f"{decl}; {c}")

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return f"bool eq{myName}({myType} a, {myType} b{extraCmpArgs})"

    def printMarkFunction(self, catalog):
        decl = self.getMarkSignature(catalog)
        c = self.comment('printMarkFunction')
        print(f"{decl} {{ {c}")
        print(f"    if (x == NULL) return; {c}")
        print(f"    if (MARKED(x)) return; {c}")
        print(f"    MARK(x); {c}")
        print(f"    for (Index i = 0; i < x->size; i++) {{ {c}")
        self.entries.printMarkArrayLine(False, catalog, "i", 2)
        print(f"    }} {c}")
        print(f"}} {c}")
        print("")

    def printCompareFunction(self, catalog):
        decl = self.getCompareSignature(catalog)
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for")
            print(f"// {decl}")
            print("")
            return
        myName = self.getName()
        c = self.comment('printCompareFunction')
        print(f"{decl} {{ {c}")
        print(f"    if (a == b) return true; {c}")
        print(f"    if (a == NULL || b == NULL) return false; {c}")
        print(f"    if (a->size != b->size) return false; {c}")
        print(f"    for (Index i = 0; i < a->size; i++) {{ {c}")
        self.entries.printCompareArrayLine(False, catalog, "i", 2)
        print(f"    }} {c}")
        print(f"    return true; {c}")
        print(f"}} {c}")
        print("")

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printCountDeclaration')
        print(f'static inline Index count{myName}({myType} x) {{ {c}')
        print(f'    return x->size; {c}')
        print(f'}} {c}')
        print('')

    def printPrintFunction(self, catalog):
        myName = self.getName()
        decl = self.getPrintSignature(catalog)
        c = self.comment('printPrintFunction')
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        print(f'    eprintf("{myName}(%d)[\\n", x->size); {c}')
        print(f"    for (Index i = 0; i < x->size; i++) {{ {c}")
        self.entries.printPrintArrayLine(False, catalog, "i", 2)
        print(f'        eprintf("\\n"); {c}')
        print(f"    }} {c}")
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}")
        print("")

    def printMarkDeclaration(self, catalog):
        c = self.comment('printMarkDeclaration')
        decl=self.getMarkSignature(catalog)
        print(f"{decl}; {c}")

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def printFreeDeclaration(self, catalog):
        c = self.comment('printFreeDeclaration')
        decl=self.getFreeSignature(catalog)
        print(f"{decl}; {c}")

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def printFreeFunction(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        fieldType = self.entries.getTypeDeclaration(catalog)
        decl = self.getFreeSignature(catalog)
        c = self.comment('printFreeFunction')
        print(f"{decl} {{ {c}")
        print(f"    FREE_VECTOR(x, {myName}, {fieldType}, x->size); {c}")
        print(f"}} {c}")
        print("")

    def printMarkObjCase(self, catalog):
        c = self.comment('printMarkObjCase')
        objType = self.getObjType()
        name = self.getName()
        pad(2)
        print(f'case {objType}: {c}')
        pad(3)
        print(f'mark{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

    def printTypeObjCase(self, catalog):
        objType = self.getObjType()
        name = self.getName()
        c = self.comment('printTypeObjCase')
        pad(2)
        print(f'case {objType}: {c}')
        pad(3)
        print(f'return "{name}"; {c}')

    def printMarkHashField(self, depth):
        c = self.comment('printMarkHashField')
        myName = self.getName()
        pad(depth)
        print(f'mark{myName}(*({myName} **)ptr); {c}')

    def printMarkField(self, isInline, field, depth, prefix=''):
        c = self.comment('printMarkField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f"mark{myName}(x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f"return PROTECT(x{a}{prefix}{field}); {c}")

    def printFreeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printFreeObjCase')
        name=self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'free{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')


class SimpleStruct(Base):
    """
    Simple structs declared directly in the yaml
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.fields = [self.makeField(x, data[x]) for x in data.keys()]
        else:
            raise ValueError(f"SimpleStruct {name} must have 'data' field")

    def hasParserInfo(self, catalog):
        return catalog.parserInfo

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        if catalog.parserInfo:
            print(f"    ParserInfo _yy_parser_info; {c}")
        for field in self.fields:
            field.printStructTypedefLine(catalog)
        print(f"}} {name}; {c}\n")

    def isStruct(self):
        return True

    def printMermaid(self, catalog):
        for field in self.fields:
            print(f"{self.getName()} --{field.getName()}--> {field.getObjName(catalog)}")

    def makeField(self, fieldName, fieldType):
        return SimpleField(self.name, fieldName, fieldType)

    def getTypeDeclaration(self, catalog):
        name=self.getName()
        if self.isInline(catalog):
            return f"struct {name} "
        else:
            return f"struct {name} *"

    def getObjType(self):
        return ('objtype_' + self.getName()).upper()

    def isSinglySelfReferential(self, catalog):
        count = 0
        for field in self.fields:
            if field.isSimpleField() and field.getObjName(catalog) == self.getName():
                count += 1
        return count == 1

    def getSelfReferentialField(self, catalog):
        for field in self.fields:
            if field.isSimpleField() and field.getObjName(catalog) == self.getName():
                return field.getName()
        raise Exception(f'cannot find self-referential field name for {self.getName()}')

    def objTypeArray(self):
        return [ self.getObjType() ]

    def getCountSignature(self):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f'Index count{myName}({myType} x)'

    def comment(self, method):
        return f'// SimpleStruct.{method}'

    def printCountDeclaration(self, catalog):
        c = self.comment('printCountDeclaration')
        if self.isSinglySelfReferential(catalog):
            print(f'{self.getCountSignature()}; {c}')

    def printCountFunction(self, catalog):
        if self.isSinglySelfReferential(catalog):
            c = self.comment('printCountFunction')
            print(f'{self.getCountSignature()} {{ {c}')
            selfRefField = self.getSelfReferentialField(catalog)
            print(f'    Index count = 0; {c}')
            print(f'    while (x != NULL) {{ {c}')
            print(f'        x = x->{selfRefField}; {c}')
            print(f'        count++; {c}')
            print(f'    }} {c}')
            print(f'    return count; {c}')
            print(f'}} {c}')
            print('')

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def getCtype(self, astType, catalog):
        return f"{astType} *"

    def getExtraCmpFargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            ctype = self.getCtype(self.extraCmpArgs[name], catalog)
            extra += [f"{ctype}{name}"]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def getExtraCmpAargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            extra += [name]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return f"bool eq{myName}({myType} a, {myType} b{extraCmpArgs})"

    def getNewArgs(self, catalog):
        return [x for x in self.fields if x.default is None and not x.isSelfInitializing(catalog)]

    def getDefaultArgs(self, catalog):
        return [x for x in self.fields if x.default is not None or x.isSelfInitializing(catalog)]

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName=self.getName()
        args = []
        for field in self.getNewArgs(catalog):
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        if catalog.parserInfo:
            args = ['ParserInfo _PI'] + args

        return f"{myType} new{myName}({', '.join(args)})"

    def getCopySignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"{myType} copy{myName}({myType} o)"

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl = self.getNewSignature(catalog)
        print(f"{decl}; {c}")

    def printCopyDeclaration(self, catalog):
        c = self.comment('printCopyDeclaration')
        decl=self.getCopySignature(catalog)
        print(f"{decl}; {c}")

    def printFreeDeclaration(self, catalog):
        c = self.comment('printFreeDeclaration')
        decl=self.getFreeSignature(catalog)
        print(f"{decl}; {c}")

    def printMarkDeclaration(self, catalog):
        c = self.comment('printMarkDeclaration')
        decl=self.getMarkSignature(catalog)
        print(f"{decl}; {c}")

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl=self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printCompareDeclaration(self, catalog):
        c = self.comment('printCompareDeclaration')
        decl=self.getCompareSignature(catalog)
        print(f"{decl}; {c}")

    def printNewFunction(self, catalog):
        c = self.comment('printNewFunction')
        decl = self.getNewSignature(catalog)
        print(f"{decl} {{ {c}")
        hasInternalConstructors = False
        for field in self.getDefaultArgs(catalog):
            if field.isSelfInitializing(catalog) and field.default is None:
                hasInternalConstructors = True
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        if hasInternalConstructors:
            print(f"    Header _h = x->header; {c}")
            print(f"    bzero(x, sizeof(struct {myName})); {c}")
            print(f"    x->header = _h; {c}")
            print(f"    int save = PROTECT(x); {c}")
        print(f'    DEBUG("new {myName} %p", x); {c}')
        if catalog.parserInfo:
            print(f"    x->_yy_parser_info = _PI; {c}")
        for field in self.getNewArgs(catalog):
            f = field.getFieldName()
            print(f"    x->{f} = {f}; {c}")
        for field in self.getDefaultArgs(catalog):
            f = field.getFieldName()
            if field.isSelfInitializing(catalog) and field.default is None:
                d = f'{field.getConstructorName(catalog)}()'
            else:
                d = field.default
            print(f"    x->{f} = {d}; {c}")
        if hasInternalConstructors:
            print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printMarkFunctionBody(self, catalog):
        for field in self.fields:
            field.printMarkLine(self.isInline(catalog), catalog, 1)

    def printCompareFunctionBody(self, catalog):
        for field in self.fields:
            field.printCompareLine(self.isInline(catalog), catalog, 1)

    def printCopyFunctionBody(self, catalog):
        for field in self.fields:
            field.printCopyLine(self.isInline(catalog), catalog, 1)

    def printPrintFunctionBody(self, catalog):
        c = self.comment('printPrintFunctionBody')
        for field in self.fields:
            field.printPrintLine(self.isInline(catalog), catalog, 1)
            print(f'    eprintf("\\n"); {c}')

    def printMarkHashField(self, depth):
        c = self.comment('printMarkHashField')
        myName = self.getName()
        pad(depth)
        print(f'mark{myName}(*({myName} **)ptr); {c}')

    def printMarkField(self, isInline, field, depth, prefix=''):
        c = self.comment('printMarkField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f"mark{myName}(x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f"return PROTECT(x{a}{prefix}{field}); {c}")

    def printCompareField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        myName=self.getName()
        extraArgs = self.getExtraCmpAargs({})
        a = '.' if isInline else '->'
        pad(depth)
        print(f"if (!eq{myName}(a{a}{prefix}{field}, b{a}{prefix}{field}{extraArgs})) return false; {c}")

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(*({myName} **)ptr, depth + 1); {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        myName=self.getName()
        a = '.' if isInline else '->'
        pad(depth)
        print(f'print{myName}(x{a}{prefix}{field}, depth + 1); {c}')

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f'x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def printMarkFunction(self, catalog):
        c = self.comment('printMarkFunction')
        decl = self.getMarkSignature(catalog)
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (x == NULL) return; {c}")
            print(f"    if (MARKED(x)) return; {c}")
            print(f"    MARK(x); {c}")
        self.printMarkFunctionBody(catalog)
        print(f"}} {c}\n")

    def printFreeFunction(self, catalog):
        c = self.comment('printFreeFunction')
        decl = self.getFreeSignature(catalog)
        print(f"{decl} {{ {c}")
        print(f"    FREE(x, {self.getName()}); {c}")
        print(f"}} {c}\n")

    def printMarkObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printMarkObjCase')
        name=self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'mark{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

    def printFreeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printFreeObjCase')
        name=self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'free{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

    def printTypeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printTypeObjCase')
        name=self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'return "{name}"; {c}')

    def printCompareFunction(self, catalog):
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for")
            print("// {decl}".format(decl=self.getCompareSignature(catalog)))
            print("")
            return
        myName = self.getName()
        c = self.comment('printCompareFunction')
        decl=self.getCompareSignature(catalog)
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (a == b) return true; {c}")
            print(f"    if (a == NULL || b == NULL) return false; {c}")
        self.printCompareFunctionBody(catalog)
        print(f"    return true; {c}")
        print(f"}} {c}\n")

    def printCopyFunction(self, catalog):
        c = self.comment('printCopyFunction')
        decl = self.getCopySignature(catalog)
        print(f"{decl} {{ {c}")
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"    if (o == NULL) return NULL; {c}")
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    int save = PROTECT(x); {c}")
        if catalog.parserInfo:
            print(f"    x->_yy_parser_info = o->_yy_parser_info; {c}")
        self.printCopyFunctionBody(catalog)
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printPrintFunction(self, catalog):
        c = self.comment('printPrintFunction')
        myName = self.getName()
        decl=self.getPrintSignature(catalog)
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        if not self.isInline(catalog):
            print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        print(f'    eprintf("{myName}[\\n"); {c}')
        self.printPrintFunctionBody(catalog)
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}\n")

    def getDefineValue(self):
        return 'x'

    def getDefineArg(self):
        return 'x'


class DiscriminatedUnionField(EnumField):
    """
    Contains all the information from a field in a discriminated union.
    Shared between DiscriminatedUnionEnum, Union and DiscriminatedUnion objects
    """
    def __init__(self, owner, name, typeName):
        if name is True:
            raise Exception("DiscriminatedUnionField passed a boolean")
        super().__init__(owner, name)
        self.typeName = typeName
        self.default = None

    def getObjName(self, catalog):
        return self.typeName

    def comment(self, method):
        return f'// DiscriminatedUnionField.{method}'

    def printHelperNewDeclaration(self, catalog, isInline):
        ucfirst = self.getName()[0].upper() + self.getName()[1:]
        c = self.comment('printHelperNewDeclaration')
        arg = self.getDefineArg(catalog)
        macroArg = arg
        typeName = self.makeTypeName()
        argMacro = self.getDefineMacro(catalog, self.getName())
        obj = catalog.get(self.typeName)
        owner = catalog.get(self.owner)
        argType = ''
        if arg != '':
            argType = obj.getTypeDeclaration(catalog) + ' '
        parserInfoFarg = ''
        parserInfoAarg = ''
        if owner.hasParserInfo(catalog):
            parserInfoFarg = 'struct ParserInfo I'
            parserInfoAarg = 'I, '
            if argType != '':
                argType = ', ' + argType
        else:
            if arg == '':
                arg = 'void'
        if isInline:
            consPrefix = self.owner[0].lower() + self.owner[1:]
            print(f'static inline {self.owner} {consPrefix}_{ucfirst}({parserInfoFarg}{argType}{arg}) {{ {c}')
            print(f'    return ({self.owner}) {{ .type = {parserInfoAarg}{typeName}, .val = {argMacro}({macroArg}) }}; {c}')
        else:
            print(f'static inline {self.owner} *new{self.owner}_{ucfirst}({parserInfoFarg}{argType}{arg}) {{ {c}')
            print(f'    return new{self.owner}({parserInfoAarg}{typeName}, {argMacro}({macroArg})); {c}')
        print(f'}} {c}')
        print('')

    def printStructTypedefLine(self, catalog):
        c = self.comment('printStructTypedefLine')
        obj = catalog.get(self.typeName)
        otype=obj.getTypeDeclaration(catalog)
        name=self.name
        print(f"    {otype} {name}; {c}")

    def getSignature(self, catalog):
        obj = catalog.get(self.typeName)
        return "{type} {name}".format(type=obj.getTypeDeclaration(catalog), name=self.name)

    def getCopyCall(self, arg, catalog):
        return f'DiscriminatedUnionField_getCopyCall({arg})'

    def getFieldName(self):
        return 'DiscriminatedUnionField_getFieldName'

    def getDefineMacro(self, catalog, user):
        v = self.owner + '_val_' + user
        v = v.upper().replace('AST', 'AST_')
        return v

    def getDefineArg(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getDefineArg()

    def getDefineType(self, catalog):
        return catalog.get(self.owner).getUnion().getTypeDeclaration(catalog)

    def getDefineField(self, catalog):
        return self.name

    def getDefineValue(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getDefineValue()

    def printDefine(self, catalog, user, value):
        keys = {
            "macro": self.getDefineMacro(catalog, user),
            "arg": self.getDefineArg(catalog),
            "type": self.getDefineType(catalog),
            "field": self.getDefineField(catalog),
            "value": value
        }
        print("#define {macro}({arg}) (({type}){{.{field} = ({value})}})".format_map(keys))

    def printDefines(self, catalog):
        self.printDefine(catalog, self.name, self.getDefineValue(catalog))

    def printMarkCase(self, isInline, catalog):
        c = self.comment('printMarkCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printMarkField(isInline, self.name, 3, 'val.')
        print(f"            break; {c}")

    def printProtectCase(self, isInline, catalog):
        c = self.comment('printProtectCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printProtectField(isInline, self.name, 3, 'val.')

    def printCompareCase(self, isInline, catalog):
        c = self.comment('printCompareCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printCompareField(isInline, self.name, 3, 'val.')
        print(f"            break; {c}")

    def printPrintCase(self, catalog, isInline):
        c = self.comment('printPrintCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        print(f'            pad(depth + 1); {c}')
        print(f'            eprintf("{typeName}\\n"); {c}')
        obj = catalog.get(self.typeName)
        obj.printPrintField(isInline, self.name, 3, 'val.')
        print(f"            break; {c}")

    def printCopyCase(self, catalog, isInline):
        c = self.comment('printCopyCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printCopyField(isInline, self.name, 3, 'val.')
        print(f"            break; {c}")


class DiscriminatedUnion(SimpleStruct):
    """
    Contains the data from a union specification in the yaml.
    Prints as the struct { type, val }
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        self.union = DiscriminatedUnionUnion(self.name, self.fields, body)
        self.enum = DiscriminatedUnionEnum(self.name, self.fields, body)

    def build(self, catalog):
        catalog.add(self.union)
        catalog.add(self.enum)

    def makeField(self, fieldName, fieldData):
        return DiscriminatedUnionField(self.name, fieldName, fieldData)

    def comment(self, method):
        return f'// DiscriminatedUnion.{method}'

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        name=self.getName()
        self.noteTypedef()
        enum=self.enum.getTypeDeclaration(catalog)
        efield=self.enum.getFieldName()
        union=self.union.getTypeDeclaration(catalog)
        ufield=self.union.getFieldName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        if catalog.parserInfo:
            print(f"    ParserInfo _yy_parser_info; {c}")
        print(f"    {enum} {efield}; {c}")
        print(f"    {union} {ufield}; {c}")
        print(f"}} {name}; {c}\n")

    def printHelperNewDeclarations(self, catalog):
        for field in self.fields:
            field.printHelperNewDeclaration(catalog, self.isInline(catalog))

    def getNewArgs(self, catalog):
        return [self.enum, self.union]

    def getUnion(self):
        return self.union

    def printDefines(self, catalog):
        for field in self.fields:
            field.printDefines(catalog)

    def printMarkFunctionBody(self, catalog):
        c = self.comment('printMarkFunctionBody')
        myName=self.getName()
        a = '.' if self.isInline(catalog) else '->'
        print(f"    switch(x{a}type) {{ {c}")
        for field in self.fields:
            field.printMarkCase(self.isInline(catalog), catalog)
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d in mark{myName}", x{a}type); {c}')
        print(f"    }} {c}")

    def printCompareFunctionBody(self, catalog):
        c = self.comment('printCompareFunctionBody')
        myName=self.getName()
        a = '.' if self.isInline(catalog) else '->'
        print(f"    if (a{a}type != b{a}type) return false; {c}")
        print(f"    switch(a{a}type) {{ {c}")
        for field in self.fields:
            field.printCompareCase(self.isInline(catalog), catalog)
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d in eq{myName}", a{a}type); {c}')
        print(f"    }} {c}")

    def printCopyFunctionBody(self, catalog):
        c = self.comment('printCopyFunctionBody')
        myName=self.getName()
        a = '.' if self.isInline(catalog) else '->'
        print(f"    switch(o{a}type) {{ {c}")
        for field in self.fields:
            field.printCopyCase(catalog, self.isInline(catalog))
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d in copy{myName}", o{a}type); {c}')
        print(f"    }} {c}")
        print(f'    x{a}type = o{a}type; {c}')

    def printPrintFunctionBody(self, catalog):
        c = self.comment('printPrintFunctionBody')
        myName=self.getName()
        a = '.' if self.isInline(catalog) else '->'
        print(f"    switch(x{a}type) {{ {c}")
        for field in self.fields:
            field.printPrintCase(catalog, self.isInline(catalog))
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d in print{myName}", x{a}type); {c}')
        print(f"    }} {c}")
        print(f'    eprintf("\\n"); {c}')


class InlineDiscriminatedUnion(DiscriminatedUnion):
    """
    Inline (call by value) structs live on the stack not the heap, they are
    passed by value not by reference, and they are not directly memory-maneged
    (though their components may be, so they still need mark functions,
    just not new and free functions.
    """

    def __init__(self, name, data):
        super().__init__(name, data)

    def isInline(self, catalog):
        return True

    def printNewFunction(self, catalog):
        pass

    def printNewDeclaration(self, catalog):
        pass

    def printFreeFunction(self, catalog):
        pass

    def printFreeDeclaration(self, catalog):
        pass

    def getProtectDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        return f'int protect{myName}({myType} x)'

    def printProtectDeclaration(self, catalog):
        decl = self.getProtectDeclaration(catalog)
        c = self.comment('printProtectDeclaration')
        print(f'{decl}; {c}')

    def printProtectFunction(self, catalog):
        decl = self.getProtectDeclaration(catalog)
        a = '.' if self.isInline(catalog) else '->'
        c = self.comment('printProtectFunction')
        print(f'{decl} {{ {c}')
        print(f'    switch(x{a}type) {{ {c}')
        for field in self.fields:
            field.printProtectCase(self.isInline(catalog), catalog)
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d", x{a}type); {c}')
        print(f'    }} {c}')
        print(f'}} {c}')
        print('')

    def objTypeArray(self):
        return []

    def comment(self, method):
        return f'// InlineDiscriminatedUnion.{method}'

    def printCopyDeclaration(self, catalog):
        pass

    def printCopyFunction(self, catalog):
        pass

class DiscriminatedUnionUnion(Base):
    """
    Built and added to the catalog by DiscriminatedUnion.build()
    contains DiscriminatedUnionField objects
    """
    def __init__(self, name, fields, body):
        super().__init__(name, body)
        self.fields = fields

    def comment(self, method):
        return f'// DiscriminatedUnionUnion.{method}'

    def getName(self):
        return self.name + "Val"

    def printMermaid(self, catalog):
        print(self.getName())

    def getTypeDeclaration(self, catalog):
        return "union {name} ".format(name=self.getName())

    def getFieldName(self):
        return 'val'

    def isUnion(self):
        return True

    def getSignature(self, catalog):
        return "{type} val".format(type=self.getTypeDeclaration(catalog))

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        name=self.getName()
        self.noteTypedef()
        self.printBaseDocumentation()
        print(f"typedef union {name} {{ {c}")
        for field in self.fields:
            field.printStructTypedefLine(catalog)
        print(f"}} {name}; {c}\n")


class SimpleEnum(Base):
    """
    Contains enums declared directly by the yaml
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.fields = [EnumField(name, x) for x in data]
        else:
            raise ValueError(f"SimpleEnum {name} must have 'data' field")

    def getTypeDeclaration(self, catalog):
        return "enum {name} ".format(name=self.getName())

    def printMermaid(self, catalog):
        print(f'{self.getName()}["enum {self.getName()}"]')

    def comment(self, method):
        return f'// SimpleEnum.{method}'

    def getDefineValue(self):
        return 'x'

    def getDefineArg(self):
        return 'x'

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef enum {name} {{ {c}")
        count = 0
        for  field in self.fields:
            field.printEnumTypedefLine(count)
            count += 1
        print(f"}} {name}; {c}\n")

    def isEnum(self):
        return True

    def printCompareField(self, isInline, field, depth, prefix=''):
        pad(depth)
        c = self.comment('printCompareField')
        a = '.' if isInline else '->'
        print(f"switch (a{a}{prefix}{field}) {{ {c}")
        for field in self.fields:
            field.printCompareCase(depth + 1)
        pad(depth)
        print(f'}} {c}')

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        myName = self.getName()
        pad(depth)
        print(f'{MyName} *_{myName} = *({myName} **)ptr; {c}')
        pad(depth)
        print(f'switch (_{myName}->type) {{ {c}')
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print(f'}} {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        pad(depth)
        c = self.comment('printPrintField')
        a = '.' if isInline else '->'
        print(f'switch (x{a}{prefix}{field}) {{ {c}')
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print(f'}} {c}')

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f'x{a}{field} = o{a}{field}; {c}')

    def getNameFunctionDeclaration(self):
        name = self.getName()
        camel = name[0].lower() + name[1:]
        return f"char * {camel}Name(enum {name} type)"

    def printNameFunctionDeclaration(self):
        c = self.comment('printNameFunctionDeclaration')
        decl = self.getNameFunctionDeclaration()
        print(f"{decl}; {c}")

    def printNameFunctionBody(self):
        decl = self.getNameFunctionDeclaration()
        c = self.comment('printNameFunctionDeclaration')
        print(f"{decl} {{ {c}")
        print(f"    switch(type) {{ {c}")
        for  field in self.fields:
            field.printNameFunctionLine()
        print(f"        default: {{ {c}")
        print(f"            static char buf[64]; {c}")
        print(f'            sprintf(buf, "%d", type); {c}')
        print(f"            return buf; {c}")
        print(f"        }} {c}")
        print(f"    }} {c}")
        print(f"}} {c}")
        print("")


class DiscriminatedUnionEnum(Base):
    """
    Built and added to the catalog by DiscriminatedUnion.build()
    contains DiscriminatedUnionField objects
    """
    def __init__(self, name, fields, body):
        super().__init__(name, body)
        self.fields = fields

    def comment(self, method):
        return f'// DiscriminatedUnionEnum.{method}'

    def getName(self):
        return self.name + "Type"

    def printMermaid(self, catalog):
        print(self.getName())

    def getFieldName(self):
        return 'type'

    def getNameFunctionDeclaration(self):
        name = self.getName()
        camel = name[0].lower() + name[1:]
        return f"char * {camel}Name(enum {name} type)"

    def printNameFunctionDeclaration(self):
        c = self.comment('printNameFunctionDeclaration')
        decl = self.getNameFunctionDeclaration()
        print(f"{decl}; {c}")

    def printNameFunctionBody(self):
        decl = self.getNameFunctionDeclaration()
        c = self.comment('printNameFunctionBody')
        print(f"{decl} {{ {c}")
        print(f"    switch(type) {{ {c}")
        for  field in self.fields:
            field.printNameFunctionLine()
        print(f"        default: {{ {c}")
        print(f"            static char buf[64]; {c}")
        print(f'            sprintf(buf, "%d", type); {c}')
        print(f"            return buf; {c}")
        print(f"        }} {c}")
        print(f"    }} {c}")
        print(f"}} {c}")
        print("")

    def getTypeDeclaration(self, catalog):
        return "enum {name} ".format(name=self.getName())

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name=self.getName()
        self.printBaseDocumentation()
        print(f"typedef enum {name} {{ {c}")
        count = 0
        for  field in self.fields:
            field.printEnumTypedefLine(count)
            count += 1
        print(f"}} {name}; {c}\n")

    def getSignature(self, catalog):
        return "{type} type".format(type=self.getTypeDeclaration(catalog))

    def isEnum(self):
        return True


class Primitive(Base):
    """
    Primitive types declared by the yaml and added to the catalog
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.cname = data['cname']
            if 'markFn' in data:
                self.markFn = data['markFn']
            else:
                self.markFn = None
            if 'printf' in data:
                self.printFn = 'printf'
                self.printf = data['printf']
            else:
                self.printFn = data['printFn']
            self.valued = data['valued']
            if 'compareFn' in data:
                self.compareFn = data['compareFn']
            else:
                self.compareFn = None
            if 'copyFn' in data:
                self.copyFn = data['copyFn']
            else:
                self.copyFn = None
        else:
            raise ValueError(f"Primitive {name} must have 'data' field")

    def printMermaid(self, catalog):
        pass

    def isInline(self, catalog):
        return True

    def comment(self, method):
        return f'// Primitive.{method}'

    def printMarkCase(self, isInline, catalog):
        c = self.comment('printMarkCase')
        if self.markFn is not None:
            typeName = self.makeTypeName()
            print(f"        case {typeName}: {c}")
            self.printMarkField(isInline, self.name, 3, 'val.')
            print("            break; {c}")

    def hasMarkFn(self):
        return self.markFn is not None

    def printMarkHashField(self, depth):
        c = self.comment('printMarkHashField')
        if self.markFn is not None:
            pad(depth)
            print(f'{self.markFn}(*({self.cname}*)ptr); {c}')

    # Primitive
    def printMarkField(self, isInline, field, depth, prefix=''):
        if self.markFn is not None:
            c = self.comment('printMarkField')
            markFn=self.markFn
            pad(depth)
            a = '.' if isInline else '->'
            print(f"{markFn}(x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        pad(depth)
        if self.markFn is None:
            print(f"return PROTECT(NULL); {c}")
        else:
            a = '.' if isInline else '->'
            print(f"return PROTECT(x{a}{prefix}{field}); {c}")

    def getTypeDeclaration(self, catalog):
        return self.cname

    def printCompareField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        pad(depth)
        a = '.' if isInline else '->'
        if self.compareFn is None:
            print(f"if (a{a}{prefix}{field} != b{a}{prefix}{field}) return false; {c}")
        else:
            print(f"if ({self.compareFn}(a{a}{prefix}{field}, b{a}{prefix}{field})) return false; {c}")

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        pad(depth)
        print(f'eprintf("%*s", depth * PAD_WIDTH, ""); {c}')
        pad(depth)
        if self.printFn == 'printf':
            print(f'eprintf("{self.cname} {self.printf}", *({self.cname} *)ptr); {c}')
        else:
            print(f'{self.printFn}(*({self.cname} *)ptr, depth + 1); {c}')


    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        a = '.' if isInline else '->'
        if self.printFn == 'printf':
            pad(depth)
            print(f'pad(depth + 1); {c}')
            pad(depth)
            print(f'eprintf("{self.cname} {self.printf}", x{a}{prefix}{field}); {c}')
        else:
            pad(depth)
            print(f'{self.printFn}(x{a}{prefix}{field}, depth + 1); {c}')

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        if self.copyFn is None:
            print(f"x{a}{prefix}{field} = o{a}{prefix}{field}; {c}")
        else:
            print(f"x{a}{prefix}{field} = {self.copyFn}(o{a}{prefix}{field}); {c}")

    def getDefineValue(self):
        return 'x' if self.valued else 'NULL'

    def getDefineArg(self):
        return 'x' if self.valued else ''


##################################################################

def debug(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def pad(depth):
    print("    " * depth, end='')

def printGpl(file, document):
    now = datetime.datetime.now()
    print('/*')
    with open('docs/gpl') as gpl:
        line = gpl.readline()
        while line:
            print(' * ', end='')
            print(line, end='')
            line = gpl.readline()
    print(" *")
    print(f" * {document['config']['description']}")
    print(f" * Generated from {file} by tools/makeAST.py")
    print(" */")

class Loader(yaml.SafeLoader):

    def __init__(self, stream):
        self._root = os.path.split(stream.name)[0]
        super(Loader, self).__init__(stream)

    def include(self, node):
        filename = os.path.join(self._root, self.construct_scalar(node))
        with open(filename, 'r') as f:
            return yaml.load(f, Loader)

Loader.add_constructor('!include', Loader.include)

##################################################################

parser = argparse.ArgumentParser()
parser.add_argument("yaml", help="input yaml file")
parser.add_argument("type",
                    type=str,
                    choices=["h", "c", "objtypes_h", "debug_h", "debug_c", "md"],
                    help="the type of output to produce")
args = parser.parse_args()

stream = open(args.yaml, 'r')

document = yaml.load(stream, Loader)

typeName = document['config']['name']
description = document['config']['description']
if 'includes' in document['config']:
    includes = document['config']['includes']
else:
    includes = []
if 'limited_includes' in document['config']:
    limited_includes = document['config']['limited_includes']
else:
    limited_includes = []

parserInfo = document['config']['parserInfo'] if 'parserInfo' in document['config'] else False

catalog = Catalog(typeName)

if parserInfo:
    catalog.noteParserInfo()

if "hashes" in document:
    for name in document["hashes"]:
        catalog.add(SimpleHash(name, document["hashes"][name]))

if "structs" in document:
    for name in document["structs"]:
        catalog.add(SimpleStruct(name, document["structs"][name]))

# vectors are variable sized so could never be inlined
if "vectors" in document:
    for name in document["vectors"]:
        catalog.add(SimpleVector(name, document["vectors"][name]))

# Inline components are different in a few consistent ways:
# 1. They are not directly memory managed.
#    So they have no Header, and no new or free functions.
#    They do however have mark functions to mark their components.
# 2. They are passed by value rather than by reference.
# 3. They are shallow rather than deep copied.
# 4. They may have their own protect functions.
if "inline" in document:
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

catalog.build()

def printSection(name):
    print("")
    print("/*")
    print(f" * {name}")
    print(" */")
    print("")

if args.type == "h":

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
    print("")
    print("#endif")

elif args.type == "objtypes_h":

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

elif args.type == "c":

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

elif args.type == 'debug_h':

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

elif args.type == 'debug_c':

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

elif args.type == 'md':

    print(f"# {typeName}")
    print("")
    print(description)
    print("")

    print("```mermaid")
    print("flowchart TD")
    catalog.printMermaid()
    print("```")
    print("")
    print(f"> Generated from {args.yaml} by tools/makeAST.py")
