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

    def printMarkDeclarations(self):
        for entity in self.contents.values():
            entity.printMarkDeclaration(self)

    def printAccessDeclarations(self):
        for entity in self.contents.values():
            entity.printAccessDeclarations(self)

    def printPushDeclarations(self):
        for entity in self.contents.values():
            entity.printPushDeclaration(self)

    def printPushFunctions(self):
        for entity in self.contents.values():
            entity.printPushFunction(self)

    def printFreeDeclarations(self):
        for entity in self.contents.values():
            entity.printFreeDeclaration(self)

    def printNewDeclarations(self):
        for entity in self.contents.values():
            entity.printNewDeclaration(self)

    def printPrintFunctions(self):
        for entity in self.contents.values():
            entity.printPrintFunction(self)

    def printPrintDeclarations(self):
        for entity in self.contents.values():
            entity.printPrintDeclaration(self)

    def printDefines(self):
        for entity in self.contents.values():
            entity.printDefines(self)

    def printNewFunctions(self):
        for entity in self.contents.values():
            entity.printNewFunction(self)

    def printMarkFunctions(self):
        for entity in self.contents.values():
            entity.printMarkFunction(self)

    def printFreeFunctions(self):
        for entity in self.contents.values():
            entity.printFreeFunction(self)

    def printMarkObjFunction(self):
        print(f'void mark{self.typeName.capitalize()}Obj(struct Header *h) {{')
        print('    switch(h->type) {')
        for entity in self.contents.values():
            entity.printMarkObjCase(self)
        print('        default:')
        print(f'            cant_happen("unrecognised type %d in mark{self.typeName.capitalize()}Obj\\n", h->type);')
        print('    }')
        print('}')

    def printFreeObjFunction(self):
        print(f'void free{self.typeName.capitalize()}Obj(struct Header *h) {{')
        print('    switch(h->type) {')
        for entity in self.contents.values():
            entity.printFreeObjCase(self)
        print('        default:')
        print(f'            cant_happen("unrecognised type %d in free{self.typeName.capitalize()}Obj\\n", h->type);')
        print('    }')
        print('}')

    def printTypeObjFunction(self):
        print(f'char *typename{self.typeName.capitalize()}Obj(int type) {{')
        print('    switch(type) {')
        for entity in self.contents.values():
            entity.printTypeObjCase(self)
        print('        default:')
        print(f'            cant_happen("unrecognised type %d in typename{self.typeName.capitalize()}Obj\\n", type);')
        print('    }')
        print('}')

    def printObjTypeDefine(self):
        objTypeArray = []
        for entity in self.contents.values():
            objTypeArray += entity.objTypeArray()
        print("#define {typeName}_OBJTYPES() {a}".format(a=', \\\n'.join(objTypeArray), typeName=self.typeName.upper()))

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

    def printNewFunction(self, catalog):
        pass

    def printPrintDeclaration(self, catalog):
        pass

    def printPrintFunction(self, catalog):
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

    def printPushFunction(selfself, catalog):
        pass

    def isEnum(self):
        return False

    def isUnion(self):
        return False

    def isStruct(self):
        return False

    def isArray(selfself):
        return False

    def printMarkField(self, field, depth, prefix=''):
        pass


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

    def printEnumTypedefLine(self):
        print("    {field},".format(field=self.makeTypeName()))

    def makeTypeName(self):
        v = self.owner + '_type_' + self.name
        v = v.upper().replace('AST', 'AST_')
        return v

    def printPrintCase(self, depth):
        typeName = self.makeTypeName()
        pad(depth)
        print(f'case {typeName}:')
        pad(depth + 1)
        print('pad(depth + 1);')
        pad(depth + 1)
        print(f'eprintf("{typeName}");')
        pad(depth + 1)
        print('break;')

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

    def getTypeDeclaration(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getTypeDeclaration()

    def getSignature(self, catalog):
        return "{type} {name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getArraySignature(self, catalog):
        return "{type} *{name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getFieldName(self):
        return self.name

    def printMarkLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(self.name, depth)

    def printMarkArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(f"{self.name}[{key}]", depth)

    def printPrintLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(self.name, depth)

    def printPrintArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(f"{self.name}[{key}]", depth)

    def printStructTypedefLine(self, catalog):
        print("    {decl};".format(decl=self.getSignature(catalog)))

    def printArrayTypedefLine(self, catalog):
        print("    {decl};".format(decl=self.getArraySignature(catalog)))


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

    def printPrintField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(x->{prefix}{field}, depth+1);')

    def printAccessDeclarations(self, catalog):
        if self.dimension == 2:
            print(f"static inline {self.entries.getTypeDeclaration(catalog)} get{self.getName()}Index({self.getTypeDeclaration()} obj, int x, int y) {{")
            print("    if (x >= obj->width || y >= obj->height || x < 0 || y < 0) {");
            print('        cant_happen("2d matrix bounds exceeded");')
            print("    }")
            print("    return obj->entries[x + y * obj->width];")
            print("}")
            print("")
            print(f"static inline void set{self.getName()}Index({self.getTypeDeclaration()} obj, int x, int y, {self.entries.getTypeDeclaration(catalog)} val) {{")
            print("    if (x >= obj->width || y >= obj->height || x < 0 || y < 0) {");
            print('        cant_happen("2d matrix bounds exceeded");')
            print("    }")
            print("    obj->entries[x + y * obj->width] = val;")
            print("}")

    def printTypedef(self, catalog):
        print("typedef struct {name} {{".format(name=self.getName()))
        print("    Header header;")
        if self.tagged:
            print("    char *_tag;")
        if self.dimension == 2: # 2D arrays are fixed size
            print("    int width;")
            print("    int height;")
        else:                   # 1D arrays can grow
            print("    int size;")
            print("    int capacity;")
        self.entries.printArrayTypedefLine(catalog)
        print("}} {name};\n".format(name=self.getName()))

    def printMarkDeclaration(self, catalog):
        print("{decl};".format(decl=self.getMarkSignature(catalog)))

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def printFreeDeclaration(self, catalog):
        print("{decl};".format(decl=self.getFreeSignature(catalog)))

    def printFreeFunction(self, catalog):
        print("{decl} {{".format(decl=self.getFreeSignature(catalog)))
        if self.dimension == 1:
            print(f"    FREE_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->entries, x->capacity);")
        else:
            print(f"    FREE_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->entries, x->width * x->height);")
        print(f"    FREE(x, {self.getName()});")
        print("}\n")

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getObjType(self):
        return ('objtype_' + self.getName()).upper()

    def objTypeArray(self):
        return [ self.getObjType() ]

    def getNewArgs(self):
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
        for field in self.getNewArgs():
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        return "{myType} new{myName}({args})".format(myType=myType, myName=self.getName(), args=', '.join(args))

    def printNewFunction(self, catalog):
        print("{decl} {{".format(decl=self.getNewSignature(catalog)))
        myType = self.getTypeDeclaration()
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"    {myType} x = NEW({myName}, {myObjType});")
        print("    int save = PROTECT(x);")
        print("#ifdef DEBUG_ALLOC");
        print(f'    eprintf("new {myName} %p\\n", x);')
        print("#endif");
        print("    x->entries = NULL;")
        if self.tagged:
            print("    x->_tag = _tag;")
        if self.dimension == 1:
            print("    x->size = 0;")
            print("    x->capacity = 0;")
            print(f"    x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, 4);")
            print("    x->capacity = 4;")
        else:
            print("    x->width = 0;")
            print("    x->height = 0;")
            print("    if (width * height > 0) {")
            print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, width * height);")
            print(f"        bzero(x->entries, sizeof({self.entries.getTypeDeclaration(catalog)}) * width * height);")
            print("    }")
            print("    x->width = width;")
            print("    x->height = height;")
        print("    UNPROTECT(save);");
        print("    return x;")
        print("}\n")

    def printNewDeclaration(self, catalog):
        print("{decl};".format(decl=self.getNewSignature(catalog)))

    def printPushDeclaration(self, catalog):
        if self.dimension == 1:
            print(f"void push{self.getName()}({self.getTypeDeclaration()} obj, {self.entries.getTypeDeclaration(catalog)} entry);")


    def printPushFunction(self, catalog):
        if self.dimension == 1:
            print(f"void push{self.getName()}({self.getTypeDeclaration()} x, {self.entries.getTypeDeclaration(catalog)} entry) {{")
            print("    if (x->size == x->capacity) {")
            print(f"        x->entries = GROW_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->entries, x->capacity, x->capacity *2);")
            print("        x->capacity *= 2;")
            print("    }")
            print("    x->entries[x->size++] = entry;")
            print("}\n")

    def printMarkFunction(self, catalog):
        print("{decl} {{".format(decl=self.getMarkSignature(catalog)))
        print("    if (x == NULL) return;")
        print("    if (MARKED(x)) return;")
        print("    MARK(x);")
        self.printMarkFunctionBody(catalog)
        print("}\n")

    def printMarkFunctionBody(self, catalog):
        if self.dimension == 1:
            self.printMark1dFunctionBody(catalog)
        else:
            self.printMark2dFunctionBody(catalog)

    def printMark1dFunctionBody(self, catalog):
        print("    for (int i = 0; i < x->size; i++) {")
        self.entries.printMarkArrayLine(catalog, "i", 2)
        print("    }")

    def printMark2dFunctionBody(self, catalog):
        print("    int size = x->width * x->height;")
        print("    for (int i = 0; i < size; i++) {")
        self.entries.printMarkArrayLine(catalog, "i", 2)
        print("    }")

    def printPrintDeclaration(self, catalog):
        print("{decl};".format(decl=self.getPrintSignature(catalog)))

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def printPrintFunction(self, catalog):
        myName = self.getName()
        print("{decl} {{".format(decl=self.getPrintSignature(catalog)))
        print("    pad(depth);")
        print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }}')
        if self.tagged:
            print('    eprintf("<<%s>>", x->_tag);')
        if self.dimension == 1:
            print(f'    eprintf("{myName}(%d)[\\n", x->size);')
        else:
            print(f'    eprintf("{myName}(%d * %d)[\\n", x->width, x->height);')
        self.printPrintFunctionBody(catalog)
        print("    pad(depth);")
        print('    eprintf("]");')
        print("}\n")

    def printPrintFunctionBody(self, catalog):
        if self.dimension == 1:
            self.print1dPrintFunctionBody(catalog)
        else:
            self.print2dPrintFunctionBody(catalog)

    def print1dPrintFunctionBody(self, catalog):
        print("    for (int i = 0; i < x->size; i++) {")
        self.entries.printPrintArrayLine(catalog, "i", 2)
        print('        eprintf("\\n");')
        print("    }")

    def print2dPrintFunctionBody(self, catalog):
        print("    for (int i = 0; i < x->height; i++) {")
        print("        pad(depth);")
        print('        eprintf("[\\n");')
        print("        for (int j = 0; j < x->width; j++) {")
        self.entries.printPrintArrayLine(catalog, "i * x->width + j", 3)
        print('            eprintf("\\n");')
        print("        }")
        print("        pad(depth);")
        print('        eprintf("]\\n");')
        print("    }")

    def printFreeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}:')
        pad(3)
        print('free{name}(({name} *)h);'.format(name=self.getName()))
        pad(3)
        print('break;')

    def printMarkObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}:')
        pad(3)
        print('mark{name}(({name} *)h);'.format(name=self.getName()))
        pad(3)
        print('break;')

    def printTypeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}:')
        pad(3)
        print('return "{name}";'.format(name=self.getName()))

    def printMarkField(self, field, depth, prefix=''):
        pad(depth)
        print("mark{myName}(x->{prefix}{field});".format(field=field, myName=self.getName(), prefix=prefix))


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
        print("typedef struct {name} {{".format(name=self.getName()))
        print("    Header header;")
        for field in self.fields:
            field.printStructTypedefLine(catalog)
        print("}} {name};\n".format(name=self.getName()))

    def isStruct(self):
        return True

    def makeField(self, fieldName, fieldType):
        return SimpleField(self.name, fieldName, fieldType)

    def getTypeDeclaration(self):
        return "struct {name} *".format(name=self.getName())

    def getObjType(self):
        return ('objtype_' + self.getName()).upper()

    def objTypeArray(self):
        return [ self.getObjType() ]

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration()
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def getNewArgs(self):
        return [x for x in self.fields if x.default is None]

    def getDefaultArgs(self):
        return [x for x in self.fields if x.default is not None]

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration()
        args = []
        for field in self.getNewArgs():
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        return "{myType} new{myName}({args})".format(myType=myType, myName=self.getName(), args=', '.join(args))

    def printNewDeclaration(self, catalog):
        print("{decl};".format(decl=self.getNewSignature(catalog)))

    def printFreeDeclaration(self, catalog):
        print("{decl};".format(decl=self.getFreeSignature(catalog)))

    def printMarkDeclaration(self, catalog):
        print("{decl};".format(decl=self.getMarkSignature(catalog)))

    def printPrintDeclaration(self, catalog):
        print("{decl};".format(decl=self.getPrintSignature(catalog)))

    def printNewFunction(self, catalog):
        print("{decl} {{".format(decl=self.getNewSignature(catalog)))
        myType = self.getTypeDeclaration()
        myObjType = self.getObjType()
        myName = self.getName()
        print("    {myType} x = NEW({myName}, {myObjType});".format(myType=myType, myName=myName, myObjType=myObjType))
        print("#ifdef DEBUG_ALLOC");
        print(f'    eprintf("new {myName} %p\\n", x);')
        print("#endif");
        for field in self.getNewArgs():
            print("    x->{f} = {f};".format(f=field.getFieldName()))
        for field in self.getDefaultArgs():
            print("    x->{f} = {d};".format(f=field.getFieldName(), d=field.default))
        print("    return x;")
        print("}\n")

    def printMarkFunctionBody(self, catalog):
        for field in self.fields:
            field.printMarkLine(catalog, 1)

    def printPrintFunctionBody(self, catalog):
        for field in self.fields:
            field.printPrintLine(catalog, 1)
            print('    eprintf("\\n");')

    def printMarkField(self, field, depth, prefix=''):
        pad(depth)
        print("mark{myName}(x->{prefix}{field});".format(field=field, myName=self.getName(), prefix=prefix))

    def printPrintField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(x->{prefix}{field}, depth + 1);')

    def printMarkFunction(self, catalog):
        print("{decl} {{".format(decl=self.getMarkSignature(catalog)))
        print("    if (x == NULL) return;")
        print("    if (MARKED(x)) return;")
        print("    MARK(x);")
        self.printMarkFunctionBody(catalog)
        print("}\n")

    def printFreeFunction(self, catalog):
        print("{decl} {{".format(decl=self.getFreeSignature(catalog)))
        print(f"    FREE(x, {self.getName()});")
        print("}\n")

    def printMarkObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}:')
        pad(3)
        print('mark{name}(({name} *)h);'.format(name=self.getName()))
        pad(3)
        print('break;')

    def printFreeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}:')
        pad(3)
        print('free{name}(({name} *)h);'.format(name=self.getName()))
        pad(3)
        print('break;')

    def printTypeObjCase(self, catalog):
        pad(2)
        print(f'case {self.getObjType()}:')
        pad(3)
        print('return "{name}";'.format(name=self.getName()))

    def printPrintFunction(self, catalog):
        myName = self.getName()
        print("{decl} {{".format(decl=self.getPrintSignature(catalog)))
        print("    pad(depth);")
        print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }}')
        print(f'    eprintf("{myName}[\\n");')
        self.printPrintFunctionBody(catalog)
        print("    pad(depth);")
        print('    eprintf("]");')
        print("}\n")

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
        print("    {type} {name};".format(type=obj.getTypeDeclaration(), name=self.name))

    def getSignature(self, catalog):
        obj = catalog.get(self.typeName)
        return "{type} {name}".format(type=obj.getTypeDeclaration(), name=self.name)

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
        print(f"        case {typeName}:")
        obj = catalog.get(self.typeName)
        obj.printMarkField(self.name, 3, 'val.')
        print("            break;")

    def printPrintCase(self, catalog):
        typeName = self.makeTypeName()
        print(f"        case {typeName}:")
        print(f'            pad(depth + 1);')
        print(f'            eprintf("{typeName}\\n");')
        obj = catalog.get(self.typeName)
        obj.printPrintField(self.name, 3, 'val.')
        print("            break;")


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
        print("typedef struct {name} {{".format(name=self.getName()))
        print("    Header header;")
        print("    {enum} {field};".format(enum=self.enum.getTypeDeclaration(), field=self.enum.getFieldName()))
        print("    {union} {field};".format(union=self.union.getTypeDeclaration(), field=self.union.getFieldName()))
        print("}} {name};\n".format(name=self.getName()))

    def getNewArgs(self):
        return [self.enum, self.union]

    def getUnion(self):
        return self.union

    def printDefines(self, catalog):
        for field in self.fields:
            field.printDefines(catalog)

    def printMarkFunctionBody(self, catalog):
        print("    switch(x->type) {")
        for field in self.fields:
            field.printMarkCase(catalog)
        print("        default:")
        print('            cant_happen("unrecognised type %d in mark{myName}", x->type);'.format(myName=self.getName()))
        print("    }")

    def printPrintFunctionBody(self, catalog):
        print("    switch(x->type) {")
        for field in self.fields:
            field.printPrintCase(catalog)
        print("        default:")
        print('            cant_happen("unrecognised type %d in print{myName}", x->type);'.format(myName=self.getName()))
        print("    }")
        print('    eprintf("\\n");')


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
        print("typedef union {name} {{".format(name=self.getName()))
        for field in self.fields:
            field.printStructTypedefLine(catalog)
        print("}} {name};\n".format(name=self.getName()))


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
        print("typedef enum {name} {{".format(name=self.getName()))
        for  field in self.fields:
            field.printEnumTypedefLine()
        print("}} {name};\n".format(name=self.getName()))

    def isEnum(self):
        return True

    def printPrintField(self, field, depth, prefix=''):
        pad(depth)
        print('switch (x->type) {')
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print('}')



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
        print("typedef enum {name} {{".format(name=self.getName()))
        for  field in self.fields:
            field.printEnumTypedefLine()
        print("}} {name};\n".format(name=self.getName()))

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

    def printMarkCase(self, catalog):
        if self.markFn is not None:
            typeName = self.makeTypeName()
            print(f"        case {typeName}:")
            self.printMarkField(self.name, 3, 'val.')
            print("            break;")

    def printMarkField(self, field, depth, prefix=''):
        if self.markFn is not None:
            pad(depth)
            print("{markFn}(x->{prefix}{field});".format(field=field, markFn=self.markFn, prefix=prefix))

    def getTypeDeclaration(self):
        return self.cname

    def printPrintField(self, field, depth, prefix=''):
        pad(depth)
        pad(depth)
        if self.printFn == 'printf':
            print('pad(depth + 1);')
            print(f'eprintf("{self.cname} {self.printf}", x->{prefix}{field});')
        else:
            print(f'{self.printFn}(x->{prefix}{field}, depth + 1);')

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

    print(f" * generated from {file} by makeAST.py")
    print(" */")

##################################################################

parser = argparse.ArgumentParser()
parser.add_argument("yaml", help="input yaml file")
parser.add_argument("type",
                    type=str,
                    choices=["h", "c", "objtypes_h", "debug_h", "debug_c"],
                    help="the type of output to produce")
args = parser.parse_args()

stream = open(args.yaml, 'r')

document = yaml.load(stream, Loader=yaml.Loader)

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

if "arrays" in document:
    for name in document["arrays"]:
        catalog.add(SimpleArray(name, document["arrays"][name]))

if "tags" in document:
    for tag in document["tags"]:
        catalog.tag(tag);

catalog.build()

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
    print("")
    catalog.printTypedefs()
    catalog.printNewDeclarations()
    print("")
    catalog.printMarkDeclarations()
    print("")
    catalog.printFreeDeclarations()
    print("")
    catalog.printPushDeclarations()
    print("")
    catalog.printDefines()
    print("")
    catalog.printAccessDeclarations()
    print("")
    print("#endif")
elif args.type == "objtypes_h":
    print(f"#ifndef cekf_{typeName}_objtypes_h")
    print(f"#define cekf_{typeName}_objtypes_h")
    printGpl(args.yaml, document)
    print("")
    catalog.printObjTypeDefine()
    print("")
    catalog.printObjCasesDefine()
    print("")
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
    print("")
    catalog.printNewFunctions()
    print("")
    catalog.printPushFunctions()
    print("")
    print("/************************************/")
    print("")
    catalog.printMarkFunctions()
    print("")
    catalog.printMarkObjFunction()
    print("")
    print("/************************************/")
    print("")
    catalog.printFreeFunctions()
    print("")
    catalog.printFreeObjFunction()
    print("")
    catalog.printTypeObjFunction()
    print("")
elif args.type == 'debug_h':
    print(f"#ifndef cekf_debug_{typeName}_h")
    print(f"#define cekf_debug_{typeName}_h")
    printGpl(args.yaml, document)
    print("")
    print(f'#include "{typeName}_helper.h"')
    for include in includes:
        print(f'#include "debug_{include}"')
    for include in limited_includes:
        print(f'#include "{include}"')
    print("")
    catalog.printPrintDeclarations()
    print("")
    print("#endif")
elif args.type == 'debug_c':
    printGpl(args.yaml, document)
    print("")
    print('#include <stdio.h>')
    print("")
    print(f'#include "debug_{typeName}.h"')
    for include in limited_includes:
        print(f'#include "{include}"')
    print("")
    print('static void pad(int depth) { eprintf("%*s", depth * 4, ""); }')
    print("")
    catalog.printPrintFunctions()
