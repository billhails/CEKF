#
# CEKF - VM supporting amb
# Copyright (C) 2022-2023  Bill Hails
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

class Catalog:
    def __init__(self, typeName):
        self.typeName = typeName
        self.contents = {}

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

    def noteBespokeCmpImplementation(self, name):
        if name in self.contents:
            self.contents[name].noteBespokeCmpImplementation()
        else:
            raise Exception("bespoke cmp implementation declared for nonexistant entry " + name)

    def get(self, key):
        if key in self.contents:
            return self.contents[key]
        else:
            raise Exception("key " + key + " not found in catalog")

    def build(self):
        values = []
        for entity in list(self.contents.values()):
            entity.build(self)

    def printTypedefs(self):
        for entity in self.contents.values():
            if entity.isEnum():
                entity.printTypedef(self)
        print("\n")
        for entity in self.contents.values():
            if entity.isUnion():
                entity.printTypedef(self)
        print("\n")
        for entity in self.contents.values():
            if entity.isStruct():
                entity.printTypedef(self)
        print("\n")
        for entity in self.contents.values():
            if entity.isArray():
                entity.printTypedef(self)
        print("\n")
        for entity in self.contents.values():
            if entity.isHash():
                entity.printTypedef(self)

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

    def printNewDeclarations(self):
        for entity in self.contents.values():
            entity.printNewDeclaration(self)

    def printCopyDeclarations(self):
        for entity in self.contents.values():
            entity.printCopyDeclaration(self)

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

    def printMarkObjFunction(self):
        comment = '// Catalog.printMarkObjFunction'
        print(f'void mark{self.typeName.capitalize()}Obj(struct Header *h) {{')
        print(f'    switch(h->type) {{ {comment}')
        for entity in self.contents.values():
            entity.printMarkObjCase(self)
        print(f'        default: {comment}')
        print(f'            cant_happen("unrecognised type %d in mark{self.typeName.capitalize()}Obj\\n", h->type); {comment}')
        print(f'    }} {comment}')
        print(f'}} {comment}')

    def printFreeObjFunction(self):
        comment = '// Catalog.printFreeObjFunction'
        print(f'void free{self.typeName.capitalize()}Obj(struct Header *h) {{ {comment}')
        print(f'    switch(h->type) {{ {comment}')
        for entity in self.contents.values():
            entity.printFreeObjCase(self)
        print(f'        default: {comment}')
        print(f'            cant_happen("unrecognised type %d in free{self.typeName.capitalize()}Obj\\n", h->type); {comment}')
        print(f'    }} {comment}')
        print(f'}} {comment}')

    def printTypeObjFunction(self):
        comment = '// Catalog.printTypeObjFunction'
        print(f'char *typename{self.typeName.capitalize()}Obj(int type) {{ {comment}')
        print(f'    switch(type) {{ {comment}')
        for entity in self.contents.values():
            entity.printTypeObjCase(self)
        print(f'        default: {comment}')
        print(f'            return "???"; {comment} no error, can be used during error reporting')
        print(f'    }} {comment}')
        print(f'}} {comment}')

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
    def __init__(self, name):
        self.name = name
        self.tagged = False
        self.bespokeCmpImplementation = False
        self.extraCmpArgs = {}

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

    def build(self, catalog):
        pass

    def printTypedef(self, catalog):
        pass

    def printFreeDeclaration(self, catalog):
        pass

    def printMarkDeclaration(self, catalog):
        pass

    def printMarkFunction(self, catalog):
        pass

    def printFreeFunction(self, catalog):
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

    def noteBespokeCmpImplementation(self):
        self.bespokeCmpImplementation = True

    def makeCopyCommand(self, arg, catalog):
        return arg

    def printMarkField(self, field, depth, prefix=''):
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

    def isSimpleField(self):
        return False;

    def isSelfInitializing(self, catalog):
        return False

    def printEnumTypedefLine(self, count):
        field = self.makeTypeName()
        print(f"    {field}, // {count}");

    def makeTypeName(self):
        v = self.owner + '_type_' + self.name
        v = v.upper().replace('AST', 'AST_')
        return v

    def printCompareCase(self, depth):
        comment = '// EnumField.printCompareCase'
        typeName = self.makeTypeName()
        pad(depth)
        print(f'case {typeName}: {comment}')
        pad(depth + 1)
        print(f"if (a != b) return false; {comment}")
        pad(depth + 1)
        print(f'break; {comment}')

    def printPrintCase(self, depth):
        comment = '// EnumField.printPrintCase'
        typeName = self.makeTypeName()
        pad(depth)
        print(f'case {typeName}: {comment}')
        pad(depth + 1)
        print(f'pad(depth + 1); {comment}')
        pad(depth + 1)
        print(f'eprintf("{typeName}"); {comment}')
        pad(depth + 1)
        print(f'break; {comment}')

class SimpleField:
    """
    Represents a field in a SimpleStruct object
    """
    def __init__(self, owner, name, typeName):
        self.owner = owner
        parts = re.split("\s*=\s*", typeName, 1)
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
        return obj.getTypeDeclaration()

    def getSignature(self, catalog):
        return "{type} {name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getArraySignature(self, catalog):
        return "{type} *{name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getFieldName(self):
        return self.name

    def hasMarkFn(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.hasMarkFn()

    def getCopyCall(self, arg, catalog):
        obj = catalog.get(self.typeName)
        return obj.makeCopyCommand(arg, catalog)

    def printMarkLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(self.name, depth)

    def printMarkArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(f"{self.name}[{key}]", depth)

    def printMarkHashLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkHashField(depth)

    def printPrintHashLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintHashField(depth)

    def printCompareLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printCompareField(self.name, depth)

    def printPrintLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(self.name, depth)

    def printCopyLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printCopyField(self.name, depth)

    def printPrintArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(f"{self.name}[{key}]", depth)

    def printCopyArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printCopyField(f"{self.name}[{key}]", depth)

    def printCompareArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printCompareField(f"{self.name}[{key}]", depth)

    def printStructTypedefLine(self, catalog):
        print("    {decl}; // SimpleField.printStructTypedefLine".format(decl=self.getSignature(catalog)))

    def printArrayTypedefLine(self, catalog):
        print("    {decl}; // SimpleField.printArrayTypedefLine".format(decl=self.getArraySignature(catalog)))


class SimpleHash(Base):
    """
    Hash tables
    """
    def __init__(self, name, data):
        super().__init__(name)
        if "entries" in data:
            self.entries = SimpleField(self.name, "entries", data["entries"])
        else:
            self.entries = None
    
    def isHash(self):
        return True

    def isSelfInitializing(self):
        return True # other constructors will call this automatically

    def getConstructorName(self):
        myName = self.getName()
        return f"new{myName}"

    def getTypeDeclaration(self):
        myName = self.getName()
        return f"struct {myName} *"

    def printNewDeclaration(self, catalog):
        decl=self.getNewSignature()
        print(f"{decl}; // SimpleHash.printNewDeclaration")

    def getNewSignature(self):
        myType = self.getTypeDeclaration()
        myConstructor = self.getConstructorName()
        return f"{myType}{myConstructor}(void)"

    def getSetDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration()
        if self.entries is None:
            return f'void set{myName}({myType} table, HashSymbol *key)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'void set{myName}({myType} table, HashSymbol *key, {valueType} value)'

    def printSetDeclaration(self, catalog):
        decl = self.getSetDeclaration(catalog)
        print(f'{decl}; // SimpleHash.printSetDeclaration')

    def getGetDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration()
        if self.entries is None:
            return f'bool get{myName}({myType} table, HashSymbol *key)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'bool get{myName}({myType} table, HashSymbol *key, {valueType}* value)'
        
    def printGetDeclaration(self, catalog):
        decl = self.getGetDeclaration(catalog)
        print(f'{decl}; // SimpleHash.printGetDeclaration')

    def getIteratorDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration()
        if self.entries is None:
            return f'HashSymbol * iterate{myName}({myType} table, int *i)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'HashSymbol * iterate{myName}({myType} table, int *i, {valueType}*value)'

    def printIteratorDeclaration(self, catalog):
        decl = self.getIteratorDeclaration(catalog)
        print(f'{decl}; // SimpleHash.printIteratorDeclaration')

    def printIteratorFunction(self, catalog):
        decl = self.getIteratorDeclaration(catalog)
        print(f'{decl} {{ // SimpleHash.printIteratorFunction')
        if self.entries is None:
            print('    return iterateHashTable((HashTable *)table, i, NULL);')
        else:
            print('    return iterateHashTable((HashTable *)table, i, value);')
        print('} // SimpleHash.printIteratorFunction')
        print('')
        
    def printSetFunction(self, catalog):
        decl = self.getSetDeclaration(catalog)
        print(f'{decl} {{ // SimpleHash.printSetFunction')
        if self.entries is None:
            print('    hashSet((HashTable *)table, key, NULL); // SimpleHash.printSetFunction')
        else:
            print('    hashSet((HashTable *)table, key, &value); // SimpleHash.printSetFunction')
        print('} // SimpleHash.printSetFunction')
        print('')

    def printGetFunction(self, catalog):
        decl = self.getGetDeclaration(catalog)
        print(f'{decl} {{ // SimpleHash.printGetFunction')
        if self.entries is None:
            print('    return hashGet((HashTable *)table, key, NULL); // SimpleHash.printGetFunction')
        else:
            print('    return hashGet((HashTable *)table, key, value); // SimpleHash.printGetFunction')
        print('} // SimpleHash.printGetFunction')
        print('')

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration()
        print(f'static inline int count{myName}({myType} table) {{ // SimpleHash.printCountDeclaration')
        print('    return ((HashTable *)table)->count; // SimpleHash.printCountDeclaration')
        print('} // SimpleHash.printCountDeclaration')
        print('')
        
    def printTypedef(self, catalog):
        self.noteTypedef()
        myName = self.getName()
        print(f'typedef struct {myName} {{ // SimpleHash.printTypedef')
        print('    struct HashTable wrapped; // SimpleHash.printTypedef')
        print(f'}} {myName}; // SimpleHash.printTypedef')
        print('')

    def printCopyField(self, field, depth, prefix=''):
        myConstructor = self.getConstructorName()
        print(f'    x->{prefix}{field} = {myConstructor}(); // SimpleHash.printCopyField')
        print(f'    copyHashTable((HashTable *)x->{prefix}{field}, (HashTable *)o->{prefix}{field}); // SimpleHash.printCopyField')

    def printPrintHashField(self, depth):
        pad(depth)
        print(f'printHashTable(*(HashTable **)ptr, depth + 1); // SimpleHash.printPrintHashField')

    def printPrintField(self, field, depth, prefix=''):
        pad(depth)
        print(f'printHashTable((HashTable *)x->{prefix}{field}, depth + 1); // SimpleHash.printPrintField')

    def printCompareField(self, field, depth, prefix=''):
        pad(depth)
        print("return false; // SimpleHash.printCompareField")

    def printNewFunction(self, catalog):
        decl = self.getNewSignature()
        myName = self.getName()
        if self.entries is None:
            size = '0'
            markFn = 'NULL'
            printFn = 'NULL'
        else:
            size = f'sizeof({self.entries.getTypeDeclaration(catalog)})'
            printFn = f'print{myName}'
            if self.entries.hasMarkFn(catalog):
                markFn = f'mark{myName}'
                print(f'static void mark{myName}(void *ptr) {{ // SimpleHash.printNewFunction')
                self.entries.printMarkHashLine(catalog, 1)
                print('} // SimpleHash.printNewFunction')
                print('')
            else:
                markFn = 'NULL'
            self.entries.printPrintDeclaration(catalog)
            print('')
            print(f'static void print{myName}(void *ptr, int depth) {{ // SimpleHash.printNewFunction')
            self.entries.printPrintHashLine(catalog, 1);
            print('}')
            print('')
        print(f'{decl} {{ // SimpleHash.printNewFunction')
        print(f'    return ({myName} *)newHashTable({size}, {markFn}, {printFn});// SimpleHash.printNewFunction')
        print('}')
        print('')

    def printMarkField(self, field, depth, prefix=''):
        pad(depth)
        print("markHashTable((HashTable *)x->{prefix}{field}); // SimpleHash.printMarkField".format(field=field, prefix=prefix))

class SimpleArray(Base):
    """
    Array structures declared in the yaml
    """
    def __init__(self, name, data):
        super().__init__(name)
        self.dimension = data["dimension"] or 1
        if self.dimension > 2:
            raise Exception("only 1 or 2 dimensional arrays supported for now")
        if self.dimension == 2:
            self.width = SimpleField(self.name, "width", "int")
            self.height = SimpleField(self.name, "height", "int")
        self.entries = SimpleField(self.name,"entries", data["entries"])

    def tag(self):
        super().tag()
        self.tagField = SimpleField(self.name, "_tag", "string")

    def getTypeDeclaration(self):
        return "struct {name} *".format(name=self.getName())

    def printCompareField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f"if (!eq{myName}(a->{prefix}{field}, b->{prefix}{field})) return false; // SimpleArray.printCompareField")

    def printCopyField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f'x->{prefix}{field} = copy{myName}(o->{prefix}{field}); // SimpleArray.printCopyField')

    def printPrintHashField(self, depth):
        pad(depth)
        myName=self.getName()
        print(f'print{myName}(*({myName} **)ptr, depth + 1); // SimpleArray.printPrintHashField')

    def printPrintField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(x->{prefix}{field}, depth+1); // SimpleArray.printPrintField')

    def printAccessDeclarations(self, catalog):
        if self.dimension == 2:
            print(f"static inline {self.entries.getTypeDeclaration(catalog)} get{self.getName()}Index({self.getTypeDeclaration()} obj, int x, int y) {{ // SimpleArray.printAccessDeclarations")
            print("#ifdef SAFETY_CHECKS // SimpleArray.printAccessDeclarations");
            print("    if (x >= obj->width || y >= obj->height || x < 0 || y < 0) { // SimpleArray.printAccessDeclarations");
            print('        cant_happen("2d matrix bounds exceeded"); // SimpleArray.printAccessDeclarations')
            print("    }")
            print("#endif // SimpleArray.printAccessDeclarations");
            print("    return obj->entries[x + y * obj->width]; // SimpleArray.printAccessDeclarations")
            print("} // SimpleArray.printAccessDeclarations")
            print("")
            print(f"static inline void set{self.getName()}Index({self.getTypeDeclaration()} obj, int x, int y, {self.entries.getTypeDeclaration(catalog)} val) {{ // SimpleArray.printAccessDeclarations")
            print("#ifdef SAFETY_CHECKS // SimpleArray.printAccessDeclarations");
            print("    if (x >= obj->width || y >= obj->height || x < 0 || y < 0) { // SimpleArray.printAccessDeclarations");
            print('        cant_happen("2d matrix bounds exceeded"); // SimpleArray.printAccessDeclarations')
            print("    } // SimpleArray.printAccessDeclarations")
            print("#endif // SimpleArray.printAccessDeclarations");
            print("    obj->entries[x + y * obj->width] = val; // SimpleArray.printAccessDeclarations")
            print("} // SimpleArray.printAccessDeclarations")

    def printTypedef(self, catalog):
        self.noteTypedef()
        print("typedef struct {name} {{ // SimpleArray.printTypedef".format(name=self.getName()))
        print("    Header header; // SimpleArray.printTypedef")
        if self.tagged:
            print("    char *_tag; // SimpleArray.printTypedef")
        if self.dimension == 2: # 2D arrays are fixed size
            print("    int width; // SimpleArray.printTypedef")
            print("    int height; // SimpleArray.printTypedef")
        else:                   # 1D arrays can grow
            print("    int size; // SimpleArray.printTypedef")
            print("    int capacity; // SimpleArray.printTypedef")
        self.entries.printArrayTypedefLine(catalog)
        print("}} {name}; // SimpleArray.printTypedef\n".format(name=self.getName()))

    def printMarkDeclaration(self, catalog):
        print("{decl}; // SimpleArray.printMarkDeclaration".format(decl=self.getMarkSignature(catalog)))

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def printFreeDeclaration(self, catalog):
        print("{decl}; // simpleArray.printFreeDeclaration".format(decl=self.getFreeSignature(catalog)))

    def printFreeFunction(self, catalog):
        print("{decl} {{ // SimpleArray.printFreeFunction".format(decl=self.getFreeSignature(catalog)))
        if self.dimension == 1:
            print(f"    FREE_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->entries, x->capacity); // SimpleArray.printFreeFunction")
        else:
            print(f"    FREE_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->entries, x->width * x->height); // SimpleArray.printFreeFunction")
        print(f"    FREE(x, {self.getName()}); // SimpleArray.printFreeFunction")
        print("} // SimpleArray.printFreeFunction\n")

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration()
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
        myType = self.getTypeDeclaration()
        args = []
        for field in self.getNewArgs(catalog):
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        return "{myType} new{myName}({args})".format(myType=myType, myName=self.getName(), args=', '.join(args))

    def getCopySignature(self):
        myType = self.getTypeDeclaration()
        myName = self.getName()
        return f"{myType} copy{myName}({myType} o)"

    def printNewFunction(self, catalog):
        print("{decl} {{ // SimpleArray.printNewFunction".format(decl=self.getNewSignature(catalog)))
        myType = self.getTypeDeclaration()
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"    {myType} x = NEW({myName}, {myObjType}); // SimpleArray.printNewFunction")
        print(f'    DEBUG("new {myName} %p", x); // SimpleArray.printNewFunction')
        print("    x->entries = NULL; // SimpleArray.printNewFunction")
        if self.tagged:
            print("    x->_tag = _tag; // SimpleArray.printNewFunction")
        if self.dimension == 1:
            print("    x->size = 0; // SimpleArray.printNewFunction")
            print("    x->capacity = 0; // SimpleArray.printNewFunction")
            print("    int save = PROTECT(x); // SimpleArray.printNewFunction")
            print(f"    x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, 4); // SimpleArray.printNewFunction")
            print("    x->capacity = 4; // SimpleArray.printNewFunction")
        else:
            print("    x->width = 0; // SimpleArray.printNewFunction")
            print("    x->height = 0; // SimpleArray.printNewFunction")
            print("    int save = PROTECT(x); // SimpleArray.printNewFunction")
            print("    if (width * height > 0) { // SimpleArray.printNewFunction")
            print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, width * height); // SimpleArray.printNewFunction")
            print(f"        bzero(x->entries, sizeof({self.entries.getTypeDeclaration(catalog)}) * width * height); // SimpleArray.printNewFunction")
            print("    } // SimpleArray.printNewFunction")
            print("    x->width = width; // SimpleArray.printNewFunction")
            print("    x->height = height; // SimpleArray.printNewFunction")
        print("    UNPROTECT(save); // SimpleArray.printNewFunction");
        print("    return x; // SimpleArray.printNewFunction")
        print("} // SimpleArray.printNewFunction\n")

    def printNewDeclaration(self, catalog):
        print("{decl}; // SimpleArray.printNewDeclaration".format(decl=self.getNewSignature(catalog)))

    def printCopyDeclaration(self, catalog):
        print("{decl}; // SimpleArray.printCopyDeclaration".format(decl=self.getCopySignature()))

    def printPushDeclaration(self, catalog):
        if self.dimension == 1:
            print(f"void push{self.getName()}({self.getTypeDeclaration()} obj, {self.entries.getTypeDeclaration(catalog)} entry); // simpleArray.printPushDeclaration")


    def printPushFunction(self, catalog):
        if self.dimension == 1:
            print(f"void push{self.getName()}({self.getTypeDeclaration()} x, {self.entries.getTypeDeclaration(catalog)} entry) {{ // SimpleArray.printPushFunction")
            print("    if (x->size == x->capacity) { // SimpleArray.printPushFunction")
            print(f"        x->entries = GROW_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->entries, x->capacity, x->capacity *2); // SimpleArray.printPushFunction")
            print("        x->capacity *= 2; // SimpleArray.printPushFunction")
            print("    } // SimpleArray.printPushFunction")
            print("    x->entries[x->size++] = entry; // SimpleArray.printPushFunction")
            print("} // SimpleArray.printPushFunction\n")

    def printMarkFunction(self, catalog):
        print("{decl} {{ // SimpleArray.printMarkFunction".format(decl=self.getMarkSignature(catalog)))
        print("    if (x == NULL) return; // SimpleArray.printMarkFunction")
        print("    if (MARKED(x)) return; // SimpleArray.printMarkFunction")
        print("    MARK(x); // SimpleArray.printMarkFunction")
        self.printMarkFunctionBody(catalog)
        print("} // SimpleArray.printMarkFunction\n")

    def printMarkFunctionBody(self, catalog):
        if self.dimension == 1:
            self.printMark1dFunctionBody(catalog)
        else:
            self.printMark2dFunctionBody(catalog)

    def printMark1dFunctionBody(self, catalog):
        print("    for (int i = 0; i < x->size; i++) { // SimpleArray.print1dFunctionBody")
        self.entries.printMarkArrayLine(catalog, "i", 2)
        print("    } // SimpleArray.print1dFunctionBody")

    def printMark2dFunctionBody(self, catalog):
        print("    int size = x->width * x->height; // SimpleArray.print2dFunctionBody")
        print("    for (int i = 0; i < size; i++) { // SimpleArray.print2dFunctionBody")
        self.entries.printMarkArrayLine(catalog, "i", 2)
        print("    } // SimpleArray.print2dFunctionBody")

    def printPrintDeclaration(self, catalog):
        print("{decl}; // SimpleArray.printPrintDeclaration".format(decl=self.getPrintSignature(catalog)))

    def printCompareDeclaration(self, catalog):
        print("{decl}; // SimpleArray.printCompareDeclaration".format(decl=self.getCompareSignature(catalog)))

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def getCtype(self, astType, catalog):
        return f"{astType} *"

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration()
        print(f'static inline int count{myName}({myType} x) {{ // SimpleArray.printCountDeclaration')
        if self.dimension == 1:
            print('    return x->size; // SimpleArray.printCountDeclaration')
        else:
            print('    return x->width * x->height; // SimpleArray.printCountDeclaration')
        print('} // SimpleArray.printCountDeclaration')
        print('')
        
    def getExtraCmpFargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            ctype = self.getCtype(self.extraCmpArgs[name], catalog)
            extra += [f"{ctype}{name}"]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration()
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return f"bool eq{myName}({myType} a, {myType} b{extraCmpArgs})"

    def printCompareFunction(self, catalog):
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for");
            print("// {decl}".format(decl=self.getCompareSignature(catalog)))
            print("")
            return
        myName = self.getName()
        print("{decl} {{ // SimpleArray.printCompareFunction".format(decl=self.getCompareSignature(catalog)))
        print("    if (a == b) return true; // SimpleArray.printCompareFunction")
        print("    if (a == NULL || b == NULL) return false; // SimpleArray.printCompareFunction")
        if self.dimension == 1:
            print("    if (a->size != b->size) return false; // SimpleArray.printCompareFunction")
            print("    for (int i = 0; i < a->size; i++) { // SimpleArray.printCompareFunction")
            self.entries.printCompareArrayLine(catalog, "i", 2)
            print("    } // SimpleArray.printCompareFunction")
        else:
            print("    if (a->width != b->width || a->height != b->height) return false; // SimpleArray.printCompareFunction")
            print("    for (int i = 0; i < (a->width * a->height); i++) { // SimpleArray.printCompareFunction")
            self.entries.printCompareArrayLine(catalog, "i", 2)
            print("    } // SimpleArray.printCompareFunction")
        print("    return true; // SimpleArray.printCompareFunction")
        print("} // SimpleArray.printCompareFunction\n")

    def printCopyFunction(self, catalog):
        print("{decl} {{ // SimpleArray.printCopyFunction".format(decl=self.getCopySignature()))
        myType = self.getTypeDeclaration()
        myObjType = self.getObjType()
        myName = self.getName()
        print("    if (o == NULL) return NULL; // SimpleArray.printCopyFunction")
        print(f"    {myType} x = NEW({myName}, {myObjType}); // SimpleArray.printCopyFunction")
        print(f'    DEBUG("copy {myName} %pn", x); // SimpleArray.printCopyFunction')
        print("    Header _h = x->header; // SimpleArray.printCopyFunction")
        print(f"    bzero(x, sizeof(struct {myName})); // SimpleArray.printCopyFunction")
        print("    x->header = _h; // SimpleArray.printCopyFunction")
        print("    int save = PROTECT(x); // SimpleArray.printCopyFunction")
        self.printCopyFunctionBody(catalog)
        print("    UNPROTECT(save); // SimpleArray.printCopyFunction")
        print("    return x; // SimpleArray.printCopyFunction")
        print("} // SimpleArray.printCopyFunction\n")

    def printCopyFunctionBody(self, catalog):
        if self.dimension == 1:
            self.print1dCopyFunctionBody(catalog)
        else:
            self.print2dCopyFunctionBody(catalog)

    def print1dCopyFunctionBody(self, catalog):
        print("    if (o->entries != NULL) { // SimpleArray.print1dCopyFunctionBody")
        print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->capacity); // SimpleArray.print1dCopyFunctionBody")
        print("        x->size = 0; // SimpleArray.print1dCopyFunctionBody")
        print("        x->capacity = o->capacity; // SimpleArray.print1dCopyFunctionBody")
        print("        for (int i = 0; i < o->size; i++) { // SimpleArray.print1dCopyFunctionBody")
        self.entries.printCopyArrayLine(catalog, "i", 3)
        print("            x->size++; // SimpleArray.print1dCopyFunctionBody")
        print("        } // SimpleArray.print1dCopyFunctionBody")
        print("    } // SimpleArray.print1dCopyFunctionBody")

    def print2dCopyFunctionBody(self, catalog):
        print("    if (o->entries != NULL) { // SimpleArray.print2dCopyFunctionBody")
        print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->width * x->height); // SimpleArray.print2dCopyFunctionBody")
        print("        x->width = 0; // SimpleArray.print2dCopyFunctionBody")
        print("        x->height = 0; // SimpleArray.print2dCopyFunctionBody")
        print("        for (int i = 0; i < (o->width * o->height); i++) { // SimpleArray.print2dCopyFunctionBody")
        self.entries.printCopyArrayLine(catalog, "i", 3)
        print("        } // SimpleArray.print2dCopyFunctionBody")
        print("        x->height = o->height; // SimpleArray.print2dCopyFunctionBody")
        print("        x->width = o->width; // SimpleArray.print2dCopyFunctionBody")
        print("    } // SimpleArray.print2dCopyFunctionBody")

    def printPrintFunction(self, catalog):
        myName = self.getName()
        print("{decl} {{ // SimpleArray.printPrintFunction".format(decl=self.getPrintSignature(catalog)))
        print("    pad(depth); // SimpleArray.printPrintFunction")
        print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} // SimpleArray.printPrintFunction')
        if self.tagged:
            print('    eprintf("<<%s>>", x->_tag); // SimpleArray.printPrintFunction')
        if self.dimension == 1:
            print(f'    eprintf("{myName}(%d)[\\n", x->size); // SimpleArray.printPrintFunction')
        else:
            print(f'    eprintf("{myName}(%d * %d)[\\n", x->width, x->height); // SimpleArray.printPrintFunction')
        self.printPrintFunctionBody(catalog)
        print("    pad(depth); // SimpleArray.printPrintFunction")
        print('    eprintf("]"); // SimpleArray.printPrintFunction')
        print("} // SimpleArray.printPrintFunction\n")

    def printPrintFunctionBody(self, catalog):
        if self.dimension == 1:
            self.print1dPrintFunctionBody(catalog)
        else:
            self.print2dPrintFunctionBody(catalog)

    def print1dPrintFunctionBody(self, catalog):
        print("    for (int i = 0; i < x->size; i++) { // SimpleArray.print1dPrintFunctionBody")
        self.entries.printPrintArrayLine(catalog, "i", 2)
        print('        eprintf("\\n"); // SimpleArray.print1dPrintFunctionBody')
        print("    } // SimpleArray.print1dPrintFunctionBody")

    def print2dPrintFunctionBody(self, catalog):
        print("    for (int i = 0; i < x->height; i++) { // SimpleArray.print2dPrintFunctionBody")
        print("        pad(depth); // SimpleArray.print2dPrintFunctionBody")
        print('        eprintf("[\\n"); // SimpleArray.print2dPrintFunctionBody')
        print("        for (int j = 0; j < x->width; j++) { // SimpleArray.print2dPrintFunctionBody")
        self.entries.printPrintArrayLine(catalog, "i * x->width + j", 3)
        print('            eprintf("\\n"); // SimpleArray.print2dPrintFunctionBody')
        print("        } // SimpleArray.print2dPrintFunctionBody")
        print("        pad(depth); // SimpleArray.print2dPrintFunctionBody")
        print('        eprintf("]\\n"); // SimpleArray.print2dPrintFunctionBody')
        print("    } // SimpleArray.print2dPrintFunctionBody")

    def printFreeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}: // SimpleArray.printFreeObjCase')
        pad(3)
        print('free{name}(({name} *)h); // SimpleArray.printFreeObjCase'.format(name=self.getName()))
        pad(3)
        print('break; // SimpleArray.printFreeObjCase')

    def printMarkObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}: // SimpleArray.printMarkObjCase')
        pad(3)
        print('mark{name}(({name} *)h); // SimpleArray.printMarkObjCase'.format(name=self.getName()))
        pad(3)
        print('break; // SimpleArray.printMarkObjCase')

    def printTypeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}: // SimpleArray.printTypeObjCase')
        pad(3)
        print('return "{name}"; // SimpleArray.printTypeObjCase'.format(name=self.getName()))

    def printMarkHashField(self, depth):
        myName = self.getName()
        pad(depth)
        print(f'mark{myName}(*({myName} **)ptr); // SimpleArray.printMarkHashField')

    def printMarkField(self, field, depth, prefix=''):
        pad(depth)
        print("mark{myName}(x->{prefix}{field}); // SimpleArray..printMarkField".format(field=field, myName=self.getName(), prefix=prefix))

    def isArray(self):
        return True


class SimpleStruct(Base):
    """
    Simple structs declared directly in the yaml
    """
    def __init__(self, name, data):
        super().__init__(name)
        self.fields = [self.makeField(x, data[x]) for x in data.keys()]

    def printTypedef(self, catalog):
        self.noteTypedef()
        print("typedef struct {name} {{ // SimpleStruct.printTypedef".format(name=self.getName()))
        print("    Header header; // SimpleStruct.printTypedef")
        for field in self.fields:
            field.printStructTypedefLine(catalog)
        print("}} {name}; // SimpleStruct.printTypedef\n".format(name=self.getName()))

    def isStruct(self):
        return True

    def makeField(self, fieldName, fieldType):
        return SimpleField(self.name, fieldName, fieldType)

    def getTypeDeclaration(self):
        return "struct {name} *".format(name=self.getName())

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
        myType = self.getTypeDeclaration()
        myName = self.getName()
        return f'int count{myName}({myType} x)'

    def printCountDeclaration(self, catalog):
        if self.isSinglySelfReferential(catalog):
            print(f'{self.getCountSignature()}; // SimpleStruct.printCountDeclaration')

    def printCountFunction(self, catalog):
        if self.isSinglySelfReferential(catalog):
            print(f'{self.getCountSignature()} {{ // SimpleStruct.printCountFunction')
            selfRefField = self.getSelfReferentialField(catalog)
            print('    int count = 0; // SimpleStruct.printCountFunction')
            print('    while (x != NULL) { // SimpleStruct.printCountFunction')
            print(f'        x = x->{selfRefField}; // SimpleStruct.printCountFunction')
            print('        count++;; // SimpleStruct.printCountFunction')
            print('    } // SimpleStruct.printCountFunction')
            print('    return count; // SimpleStruct.printCountFunction')
            print('} // SimpleStruct.printCountFunction')
            print('')

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration()
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
        myType = self.getTypeDeclaration()
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return f"bool eq{myName}({myType} a, {myType} b{extraCmpArgs})"

    def getNewArgs(self, catalog):
        return [x for x in self.fields if x.default is None and not x.isSelfInitializing(catalog)]

    def getDefaultArgs(self, catalog):
        return [x for x in self.fields if x.default is not None or x.isSelfInitializing(catalog)]

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration()
        args = []
        for field in self.getNewArgs(catalog):
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        return "{myType} new{myName}({args})".format(myType=myType, myName=self.getName(), args=', '.join(args))

    def getCopySignature(self):
        myType = self.getTypeDeclaration()
        myName = self.getName()
        return f"{myType} copy{myName}({myType} o)"

    def printNewDeclaration(self, catalog):
        print("{decl}; // SimpleStruct.printNewDeclaration".format(decl=self.getNewSignature(catalog)))

    def printCopyDeclaration(self, catalog):
        print("{decl}; // SimpleStruct.printCopyDeclaration".format(decl=self.getCopySignature()))

    def printFreeDeclaration(self, catalog):
        print("{decl}; // SimpleStruct.printFreeDeclaration".format(decl=self.getFreeSignature(catalog)))

    def printMarkDeclaration(self, catalog):
        print("{decl}; // SimpleStruct.printMarkDeclaration".format(decl=self.getMarkSignature(catalog)))

    def printPrintDeclaration(self, catalog):
        print("{decl}; // SimpleStruct.printPrintDeclaration".format(decl=self.getPrintSignature(catalog)))

    def printCompareDeclaration(self, catalog):
        print("{decl}; // SimpleStruct.printCompareDeclaration".format(decl=self.getCompareSignature(catalog)))

    def printNewFunction(self, catalog):
        print("{decl} {{ // SimpleStruct.printNewFunction".format(decl=self.getNewSignature(catalog)))
        hasInternalConstructors = False
        for field in self.getDefaultArgs(catalog):
            if field.isSelfInitializing(catalog) and field.default is None:
                hasInternalConstructors = True
        myType = self.getTypeDeclaration()
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"    {myType} x = NEW({myName}, {myObjType}); // SimpleStruct.printNewFunction")
        if hasInternalConstructors:
            print("    int save = PROTECT(x); // SimpleStruct.printNewFunction")
        print(f'    DEBUG("new {myName} %pn", x); // SimpleStruct.printNewFunction')
        for field in self.getNewArgs(catalog):
            print("    x->{f} = {f}; // SimpleStruct.printNewFunction".format(f=field.getFieldName()))
        for field in self.getDefaultArgs(catalog):
            f = field.getFieldName()
            if field.isSelfInitializing(catalog) and field.default is None:
                d = f'{field.getConstructorName(catalog)}()'
            else:
                d = field.default
            print(f"    bzero(&(x->{f}), sizeof(x->{f})); // SimpleStruct.printNewFunction")
            print(f"    x->{f} = {d}; // SimpleStruct.printNewFunction")
        if hasInternalConstructors:
            print("    UNPROTECT(save); // SimpleStruct.printNewFunction")
        print("    return x; // SimpleStruct.printNewFunction")
        print("} // SimpleStruct.printNewFunction")
        print("")

    def printMarkFunctionBody(self, catalog):
        for field in self.fields:
            field.printMarkLine(catalog, 1)

    def printCompareFunctionBody(self, catalog):
        for field in self.fields:
            field.printCompareLine(catalog, 1)

    def printCopyFunctionBody(self, catalog):
        for field in self.fields:
            field.printCopyLine(catalog, 1)

    def printPrintFunctionBody(self, catalog):
        for field in self.fields:
            field.printPrintLine(catalog, 1)
            print('    eprintf("\\n"); // SimpleStruct.printPrintFunctionBody')

    def printMarkHashField(self, depth):
        myName = self.getName()
        pad(depth)
        print(f'mark{myName}(*({myName} **)ptr); // SimpleStruct.printMarkHashField')

    def printMarkField(self, field, depth, prefix=''):
        pad(depth)
        print("mark{myName}(x->{prefix}{field}); // SimpleStruct.printMarkField".format(field=field, myName=self.getName(), prefix=prefix))

    def printCompareField(self, field, depth, prefix=''):
        myName=self.getName()
        extraArgs = self.getExtraCmpAargs({})
        pad(depth)
        print(f"if (!eq{myName}(a->{prefix}{field}, b->{prefix}{field}{extraArgs})) return false; // SimpleStruct.printCompareField")

    def printPrintHashField(self, depth):
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(*({myName} **)ptr, depth + 1); // SimpleStruct.printPrintHashField')

    def printPrintField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(x->{prefix}{field}, depth + 1); // SimpleStruct.printPrintField')

    def printCopyField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f'x->{prefix}{field} = copy{myName}(o->{prefix}{field}); // SimpleStruct.printCopyField')

    def printMarkFunction(self, catalog):
        print("{decl} {{ // SimpleStruct.printMarkFunction".format(decl=self.getMarkSignature(catalog)))
        print("    if (x == NULL) return; // SimpleStruct.printMarkFunction")
        print("    if (MARKED(x)) return; // SimpleStruct.printMarkFunction")
        print("    MARK(x); // SimpleStruct.printMarkFunction")
        self.printMarkFunctionBody(catalog)
        print("} // SimpleStruct.printMarkFunction\n")

    def printFreeFunction(self, catalog):
        print("{decl} {{ // SimpleStruct.printFreeFunction".format(decl=self.getFreeSignature(catalog)))
        print(f"    FREE(x, {self.getName()}); // SimpleStruct.printFreeFunction")
        print("} // SimpleStruct.printFreeFunction\n")

    def printMarkObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}: // SimpleStruct.printMarkObjectCase')
        pad(3)
        print('mark{name}(({name} *)h); // SimpleStruct.printMarkObjectCase'.format(name=self.getName()))
        pad(3)
        print('break; // SimpleStruct.printMarkObjectCase')

    def printFreeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}: // SimpleStruct.preintFreeObjectCase')
        pad(3)
        print('free{name}(({name} *)h); // SimpleStruct.preintFreeObjectCase'.format(name=self.getName()))
        pad(3)
        print('break; // SimpleStruct.preintFreeObjectCase')

    def printTypeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}: // SimpleStruct.printTypeObjCase')
        pad(3)
        print('return "{name}"; // SimpleStruct.printTypeObjCase'.format(name=self.getName()))

    def printCompareFunction(self, catalog):
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for");
            print("// {decl}".format(decl=self.getCompareSignature(catalog)))
            print("")
            return
        myName = self.getName()
        print("{decl} {{ // SimpleStruct.printCompareFunction".format(decl=self.getCompareSignature(catalog)))
        print("    if (a == b) return true; // SimpleStruct.printCompareFunction")
        print("    if (a == NULL || b == NULL) return false; // SimpleStruct.printCompareFunction")
        self.printCompareFunctionBody(catalog)
        print("    return true; // SimpleStruct.printCompareFunction")
        print("} // SimpleStruct.printCompareFunction\n")

    def printCopyFunction(self, catalog):
        print("{decl} {{ // SimpleStruct.printCopyFunction".format(decl=self.getCopySignature()))
        myType = self.getTypeDeclaration()
        myObjType = self.getObjType()
        myName = self.getName()
        print("    if (o == NULL) return NULL; // SimpleStruct.printCopyFunction")
        print(f"    {myType} x = NEW({myName}, {myObjType}); // SimpleStruct.printCopyFunction")
        print(f'    DEBUG("copy {myName} %pn", x); // SimpleStruct.printCopyFunction')
        print("    Header _h = x->header; // SimpleStruct.printCopyFunction")
        print(f"    bzero(x, sizeof(struct {myName})); // SimpleStruct.printCopyFunction")
        print("    x->header = _h; // SimpleStruct.printCopyFunction")
        print("    int save = PROTECT(x); // SimpleStruct.printCopyFunction")
        self.printCopyFunctionBody(catalog)
        print("    UNPROTECT(save); // SimpleStruct.printCopyFunction")
        print("    return x; // SimpleStruct.printCopyFunction")
        print("} // SimpleStruct.printCopyFunction\n")

    def printPrintFunction(self, catalog):
        myName = self.getName()
        print("{decl} {{ // SimpleStruct.printPrintFunction".format(decl=self.getPrintSignature(catalog)))
        print("    pad(depth); // SimpleStruct.printPrintFunction")
        print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} // SimpleStruct.printPrintFunction')
        print(f'    eprintf("{myName}[\\n"); // SimpleStruct.printPrintFunction')
        self.printPrintFunctionBody(catalog)
        print("    pad(depth); // SimpleStruct.printPrintFunction")
        print('    eprintf("]"); // SimpleStruct.printPrintFunction')
        print("} // SimpleStruct.printPrintFunction\n")

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

    def printStructTypedefLine(self, catalog):
        obj = catalog.get(self.typeName)
        print("    {type} {name}; // DiscriminatedUnionField.printStructTypedefLine".format(type=obj.getTypeDeclaration(), name=self.name))

    def getSignature(self, catalog):
        obj = catalog.get(self.typeName)
        return "{type} {name}".format(type=obj.getTypeDeclaration(), name=self.name)

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
        return catalog.get(self.owner).getUnion().getTypeDeclaration()

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

    def printMarkCase(self, catalog):
        typeName = self.makeTypeName()
        print(f"        case {typeName}: // DiscriminatedUnionField.printMarkCase")
        obj = catalog.get(self.typeName)
        obj.printMarkField(self.name, 3, 'val.')
        print("            break; // DiscriminatedUnionField.printMarkCase")

    def printCompareCase(self, catalog):
        typeName = self.makeTypeName()
        print(f"        case {typeName}: // DiscriminatedUnionField.printCompareCase")
        obj = catalog.get(self.typeName)
        obj.printCompareField(self.name, 3, 'val.')
        print("            break; // DiscriminatedUnionField.printCompareCase")

    def printPrintCase(self, catalog):
        typeName = self.makeTypeName()
        print(f"        case {typeName}: // DiscriminatedUnionField.printPrintCase")
        print(f'            pad(depth + 1); // DiscriminatedUnionField.printPrintCase')
        print(f'            eprintf("{typeName}\\n"); // DiscriminatedUnionField.printPrintCase')
        obj = catalog.get(self.typeName)
        obj.printPrintField(self.name, 3, 'val.')
        print("            break; // DiscriminatedUnionField.printPrintCase")

    def printCopyCase(self, catalog):
        typeName = self.makeTypeName()
        print(f"        case {typeName}: // DiscriminatedUnionField.printCopyCase")
        obj = catalog.get(self.typeName)
        obj.printCopyField(self.name, 3, 'val.')
        print("            break; // DiscriminatedUnionField.printCopyCase")


class DiscriminatedUnion(SimpleStruct):
    """
    Contains the data from a union specification in the yaml.
    Prints as the struct { type, val }
    """
    def __init__(self, name, data):
        super().__init__(name, data)
        self.union = DiscriminatedUnionUnion(self.name, self.fields)
        self.enum = DiscriminatedUnionEnum(self.name, self.fields)

    def build(self, catalog):
        catalog.add(self.union)
        catalog.add(self.enum)

    def makeField(self, fieldName, fieldData):
        return DiscriminatedUnionField(self.name, fieldName, fieldData)

    def printTypedef(self, catalog):
        self.noteTypedef()
        print("typedef struct {name} {{ // DiscriminatedUnion.printTypedef".format(name=self.getName()))
        print("    Header header; // DiscriminatedUnion.printTypedef")
        print("    {enum} {field}; // DiscriminatedUnion.printTypedef".format(enum=self.enum.getTypeDeclaration(), field=self.enum.getFieldName()))
        print("    {union} {field}; // DiscriminatedUnion.printTypedef".format(union=self.union.getTypeDeclaration(), field=self.union.getFieldName()))
        print("}} {name}; // DiscriminatedUnion.printTypedef\n".format(name=self.getName()))

    def getNewArgs(self, catalog):
        return [self.enum, self.union]

    def getUnion(self):
        return self.union

    def printDefines(self, catalog):
        for field in self.fields:
            field.printDefines(catalog)

    def printMarkFunctionBody(self, catalog):
        print("    switch(x->type) { // DiscriminatedUnion.printMarkFunctionBody")
        for field in self.fields:
            field.printMarkCase(catalog)
        print("        default: // DiscriminatedUnion.printMarkFunctionBody")
        print('            cant_happen("unrecognised type %d in mark{myName}", x->type); // DiscriminatedUnion.printMarkFunctionBody'.format(myName=self.getName()))
        print("    } // DiscriminatedUnion.printMarkFunctionBody")

    def printCompareFunctionBody(self, catalog):
        print("    if (a->type != b->type) return false; // DiscriminatedUnion.printCompareFunctionBody")
        print("    switch(a->type) { // DiscriminatedUnion.printCompareFunctionBody")
        for field in self.fields:
            field.printCompareCase(catalog)
        print("        default: // DiscriminatedUnion.printCompareFunctionBody")
        print('            cant_happen("unrecognised type %d in eq{myName}", a->type); // DiscriminatedUnion.printCompareFunctionBody'.format(myName=self.getName()))
        print("    }")

    def printCopyFunctionBody(self, catalog):
        print("    switch(o->type) { // DiscriminatedUnion.printCopyFunctionBody")
        for field in self.fields:
            field.printCopyCase(catalog)
        print("        default: // DiscriminatedUnion.printCopyFunctionBody")
        print('            cant_happen("unrecognised type %d in copy{myName}", o->type); // DiscriminatedUnion.printCopyFunctionBody'.format(myName=self.getName()))
        print("    } // DiscriminatedUnion.printCopyFunctionBody")
        print('    x->type = o->type; // DiscriminatedUnion.printCopyFunctionBody')

    def printPrintFunctionBody(self, catalog):
        print("    switch(x->type) { // DiscriminatedUnion.printPrintFunctionBody")
        for field in self.fields:
            field.printPrintCase(catalog)
        print("        default: // DiscriminatedUnion.printPrintFunctionBody")
        print('            cant_happen("unrecognised type %d in print{myName}", x->type); // DiscriminatedUnion.printPrintFunctionBody'.format(myName=self.getName()))
        print("    } // DiscriminatedUnion.printPrintFunctionBody")
        print('    eprintf("\\n"); // DiscriminatedUnion.printPrintFunctionBody')


class DiscriminatedUnionUnion(Base):
    """
    Built and added to the catalog by DiscriminatedUnion.build()
    contains DiscriminatedUnionField objects
    """
    def __init__(self, name, fields):
        super().__init__(name)
        self.fields = fields

    def getName(self):
        return self.name + "Val"

    def getTypeDeclaration(self):
        return "union {name} ".format(name=self.getName())

    def getFieldName(self):
        return 'val'

    def isUnion(self):
        return True

    def getSignature(self, catalog):
        return "{type} val".format(type=self.getTypeDeclaration())

    def printTypedef(self, catalog):
        self.noteTypedef()
        print("typedef union {name} {{ // DiscriminatedUnionUnion.printTypedef".format(name=self.getName()))
        for field in self.fields:
            field.printStructTypedefLine(catalog)
        print("}} {name}; // DiscriminatedUnionUnion.printTypedef\n".format(name=self.getName()))


class SimpleEnum(Base):
    """
    Contains enums declared directly by the yaml
    """
    def __init__(self, name, data):
        super().__init__(name)
        self.fields = [EnumField(name, x) for x in data]

    def getTypeDeclaration(self):
        return "enum {name} ".format(name=self.getName())

    def printTypedef(self, catalog):
        self.noteTypedef()
        print("typedef enum {name} {{ // SimpleEnum.printTypedef".format(name=self.getName()))
        count = 0
        for  field in self.fields:
            field.printEnumTypedefLine(count)
            count += 1
        print("}} {name}; // SimpleEnum.printTypedef\n".format(name=self.getName()))

    def isEnum(self):
        return True

    def printCompareField(self, field, depth, prefix=''):
        pad(depth)
        print("switch (a->type) { // SimpleEnum.printCompareField")
        for field in self.fields:
            field.printCompareCase(depth + 1)
        pad(depth)
        print('} // SimpleEnum.printCompareField')

    def printPrintHashField(self, depth):
        myName = self.getName()
        pad(depth)
        print(f'{MyName} *_{myName} = *({myName} **)ptr; // SimpleEnum.printPrintHashField')
        pad(depth)
        print(f'switch (_{myName}->type) {{ // SimpleEnum.printPrintHashField')
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print('} // SimpleEnum.printPrintHashField')

    def printPrintField(self, field, depth, prefix=''):
        pad(depth)
        print('switch (x->type) { // SimpleEnum.printPrintField')
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print('} // SimpleEnum.printPrintField')

    def printCopyField(self, field, depth, prefix=''):
        pad(depth)
        print(f'x->{field} = o->{field}; // SimpleEnum.printCopyField')



class DiscriminatedUnionEnum(Base):
    """
    Built and added to the catalog by DiscriminatedUnion.build()
    contains DiscriminatedUnionField objects
    """
    def __init__(self, name, fields):
        super().__init__(name)
        self.fields = fields

    def getName(self):
        return self.name + "Type"

    def getFieldName(self):
        return 'type'

    def getTypeDeclaration(self):
        return "enum {name} ".format(name=self.getName())

    def printTypedef(self, catalog):
        self.noteTypedef()
        print("typedef enum {name} {{ // DiscriminatedUnionEnum.printTypedef".format(name=self.getName()))
        count = 0
        for  field in self.fields:
            field.printEnumTypedefLine(count)
            count += 1
        print("}} {name}; // DiscriminatedUnionEnum.printTypedef\n".format(name=self.getName()))

    def getSignature(self, catalog):
        return "{type} type".format(type=self.getTypeDeclaration())

    def isEnum(self):
        return True


class Primitive(Base):
    """
    Primitive types declared by the yaml and added to the catalog
    """
    def __init__(self, name, data):
        super().__init__(name)
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

    def printMarkCase(self, catalog):
        if self.markFn is not None:
            typeName = self.makeTypeName()
            print(f"        case {typeName}: // Primitive.printMarkCase")
            self.printMarkField(self.name, 3, 'val.')
            print("            break; // Primitive.printMarkCase")

    def hasMarkFn(self):
        return self.markFn is not None

    def printMarkHashField(self, depth):
        if self.markFn is not None:
            pad(depth)
            print(f'{self.markFn}(*({self.cname}*)ptr); // Primitive.printMarkHashField')

    def printMarkField(self, field, depth, prefix=''):
        if self.markFn is not None:
            pad(depth)
            print("{markFn}(x->{prefix}{field}); // Primitive.printMarkField".format(field=field, markFn=self.markFn, prefix=prefix))

    def getTypeDeclaration(self):
        return self.cname

    def printCompareField(self, field, depth, prefix=''):
        pad(depth)
        if self.compareFn is None:
            print(f"if (a->{prefix}{field} != b->{prefix}{field}) return false; // Primitive.printCompareField")
        else:
            print(f"if (!{self.compareFn}(a->{prefix}{field}, b->{prefix}{field})) return false; // Primitive.printCompareField")

    def printPrintHashField(self, depth):
        pad(depth)
        print('eprintf("%*s", depth * PAD_WIDTH, "");')
        pad(depth)
        if self.printFn == 'printf':
            print(f'eprintf("{self.cname} {self.printf}", *({self.cname} *)ptr); // Primitive.printPrintHashField')
        else:
            print(f'{self.printFn}(*({self.cname} *)ptr, depth + 1); // Primitive.printPrintHashField')


    def printPrintField(self, field, depth, prefix=''):
        if self.printFn == 'printf':
            pad(depth)
            print('pad(depth + 1); // Primitive.printPrintField')
            pad(depth)
            print(f'eprintf("{self.cname} {self.printf}", x->{prefix}{field}); // Primitive.printPrintField')
        else:
            pad(depth)
            print(f'{self.printFn}(x->{prefix}{field}, depth + 1); // Primitive.printPrintField')

    def printCopyField(self, field, depth, prefix=''):
        pad(depth)
        if self.copyFn is None:
            print(f"x->{prefix}{field} = o->{prefix}{field}; // Primitive.printCopyField")
        else:
            print(f"x->{prefix}{field} = {self.copyFn}(o->{prefix}{field}); // Primitive.printCopyField")

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
    print(f"""/*
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
 *""")
    if 'description' in document:
        print(f" * {document['description']}\n *")

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
                    choices=["h", "c", "objtypes_h", "debug_h", "debug_c"],
                    help="the type of output to produce")
args = parser.parse_args()

stream = open(args.yaml, 'r')

document = yaml.load(stream, Loader)

typeName = document['config']['name']
if 'includes' in document['config']:
    includes = document['config']['includes']
else:
    includes = []
if 'limited_includes' in document['config']:
    limited_includes = document['config']['limited_includes']
else:
    limited_includes = []

catalog = Catalog(typeName)

if "hashes" in document:
    for name in document["hashes"]:
        catalog.add(SimpleHash(name, document["hashes"][name]))

if "structs" in document:
    for name in document["structs"]:
        catalog.add(SimpleStruct(name, document["structs"][name]))

if "unions" in document:
    for name in document["unions"]:
        catalog.add(DiscriminatedUnion(name, document["unions"][name]))

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
        catalog.tag(tag);

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
    for include in includes:
        print(f'#include "{include}"')
    for include in limited_includes:
        print(f'#include "{include}"')
    printSection("typedefs")
    catalog.printTypedefs()
    printSection("constructor declarations")
    catalog.printNewDeclarations()
    printSection("copy declarations")
    catalog.printCopyDeclarations()
    printSection("mark declarations")
    catalog.printMarkDeclarations()
    printSection("free declarations")
    catalog.printFreeDeclarations()
    printSection("push declarations")
    catalog.printPushDeclarations()
    printSection("hash getter and setter declarations")
    catalog.printGetDeclarations()
    catalog.printSetDeclarations()
    catalog.printIteratorDeclarations()
    printSection("defines")
    catalog.printDefines()
    printSection("access declarations")
    catalog.printAccessDeclarations()
    printSection("count declarations")
    catalog.printCountDeclarations()
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
    print("#include <stdio.h>");
    print("#include <strings.h>");
    print('#include "common.h"');
    print('#ifdef DEBUG_ALLOC')
    print('#include "debugging_on.h"')
    print('#else')
    print('#include "debugging_off.h"')
    print('#endif')
    printSection("constructor functions")
    catalog.printNewFunctions()
    printSection("copy functions")
    catalog.printCopyFunctions()
    printSection("push functions")
    catalog.printPushFunctions()
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
