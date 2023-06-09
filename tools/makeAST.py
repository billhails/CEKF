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

class Catalog:
    def __init__(self, typeName):
        self.typeName = typeName
        self.contents = {}

    def add(self, value):
        name = value.getName()
        if name in self.contents:
            raise Exception("attempt to overwtite " + name + " in catalog")
        self.contents[name] = value

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

    def printMarkDeclarations(self):
        for entity in self.contents.values():
            entity.printMarkDeclaration(self)

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
        print('    }')
        print('}')

    def printFreeObjFunction(self):
        print(f'void free{self.typeName.capitalize()}Obj(struct Header *h) {{')
        print('    switch(h->type) {')
        for entity in self.contents.values():
            entity.printFreeObjCase(self)
        print('    }')
        print('}')

    def printTypeObjFunction(self):
        print(f'char *typename{self.typeName.capitalize()}Obj(int type) {{')
        print('    switch(type) {')
        for entity in self.contents.values():
            entity.printTypeObjCase(self)
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

    def objTypeArray(self):
        return []

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

    def isEnum(self):
        return False

    def isUnion(self):
        return False

    def isStruct(self):
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
        print(f'printf("{typeName}");')
        pad(depth + 1)
        print('break;')

class SimpleField:
    """
    Represents a field in a SimpleStruct object
    """
    def __init__(self, owner, name, typeName):
        self.owner = owner
        self.name = name
        self.typeName = typeName

    def getSignature(self, catalog):
        obj = catalog.get(self.typeName)
        return "{type} {name}".format(type=obj.getTypeDeclaration(), name=self.name)

    def getFieldName(self):
        return self.name

    def printMarkLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(self.name, depth)

    def printPrintLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(self.name, depth)

    def printStructTypedefLine(self, catalog):
        print("    {decl};".format(decl=self.getSignature(catalog)))


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
        return self.fields

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration()
        args = []
        for field in self.getNewArgs():
            args += [field.getSignature(catalog)]
        return "{myType} new{myName}({args})".format(myType=myType, myName=self.getName(), args=', '.join(args))

    def printNewDeclaration(self, catalog):
        print("{decl};".format(decl=self.getNewSignature(catalog)));

    def printFreeDeclaration(self, catalog):
        print("{decl};".format(decl=self.getFreeSignature(catalog)));

    def printMarkDeclaration(self, catalog):
        print("{decl};".format(decl=self.getMarkSignature(catalog)));

    def printPrintDeclaration(self, catalog):
        print("{decl};".format(decl=self.getPrintSignature(catalog)));

    def printNewFunction(self, catalog):
        print("{decl} {{".format(decl=self.getNewSignature(catalog)));
        myType = self.getTypeDeclaration()
        myObjType = self.getObjType()
        myName = self.getName()
        print("    {myType} x = NEW({myName}, {myObjType});".format(myType=myType, myName=myName, myObjType=myObjType))
        for field in self.getNewArgs():
            print("    x->{f} = {f};".format(f=field.getFieldName()))
        print("    return x;")
        print("}\n")

    def printMarkFunctionBody(self, catalog):
        for field in self.fields:
            field.printMarkLine(catalog, 1)

    def printPrintFunctionBody(self, catalog):
        for field in self.fields:
            field.printPrintLine(catalog, 1)
            print('    printf("\\n");')

    def printMarkField(self, field, depth, prefix=''):
        pad(depth)
        print("mark{myName}(x->{prefix}{field});".format(field=field, myName=self.getName(), prefix=prefix))

    def printPrintField(self, field, depth, prefix=''):
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(x->{prefix}{field}, depth + 1);')

    def printMarkFunction(self, catalog):
        print("{decl} {{".format(decl=self.getMarkSignature(catalog)));
        print("    if (x == NULL) return;")
        print("    if (MARKED(x)) return;")
        print("    MARK(x);")
        self.printMarkFunctionBody(catalog)
        print("}\n")

    def printFreeFunction(self, catalog):
        print("{decl} {{".format(decl=self.getFreeSignature(catalog)));
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
        print("{decl} {{".format(decl=self.getPrintSignature(catalog)));
        print("    pad(depth);")
        print(f'    if (x == NULL) {{ printf("{myName} (NULL)"); return; }}')
        print(f'    printf("{myName}[\\n");')
        self.printPrintFunctionBody(catalog)
        print("    pad(depth);")
        print('    printf("]");')
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
        print(f'            printf("{typeName}\\n");')
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
        print('    printf("\\n");')


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
        print('switch (x->type) {');
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print('}');



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
            print(f'printf("{self.cname} {self.printf}", x->{prefix}{field});')
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

def printGpl():
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
 */

// generated by makeAST.py

""")

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

catalog = Catalog(typeName)

for name in document["structs"]:
    catalog.add(SimpleStruct(name, document["structs"][name]))

for name in document["unions"]:
    catalog.add(DiscriminatedUnion(name, document["unions"][name]))

for name in document["enums"]:
    catalog.add(SimpleEnum(name, document["enums"][name]))

for name in document["primitives"]:
    catalog.add(Primitive(name, document["primitives"][name]))

catalog.build()

if args.type == "h":
    print(f"#ifndef cekf_{typeName}_h")
    print(f"#define cekf_{typeName}_h")
    printGpl()
    print("")
    print('#include "hash.h"')
    print('#include "memory.h"')
    print("")
    catalog.printTypedefs()
    catalog.printNewDeclarations()
    print("")
    catalog.printMarkDeclarations()
    print("")
    catalog.printFreeDeclarations()
    print("")
    catalog.printDefines()
    print("")
    print("#endif");
elif args.type == "objtypes_h":
    print(f"#ifndef cekf_{typeName}_objtypes_h")
    print(f"#define cekf_{typeName}_objtypes_h")
    printGpl()
    print("")
    catalog.printObjTypeDefine()
    print("")
    catalog.printObjCasesDefine()
    print("")
    print(f'void mark{typeName.capitalize()}Obj(struct Header *h);')
    print(f'void free{typeName.capitalize()}Obj(struct Header *h);')
    print(f'char *typename{typeName.capitalize()}Obj(int type);')
    print("")
    print("#endif");
elif args.type == "c":
    printGpl()
    print("")
    print(f'#include "{typeName}.h"')
    print("")
    catalog.printNewFunctions()
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
    printGpl()
    print("")
    print(f'#include "{typeName}_helper.h"')
    print("")
    catalog.printPrintDeclarations()
    print("")
    print("#endif");
elif args.type == 'debug_c':
    printGpl()
    print("")
    print('#include <stdio.h>')
    print("")
    print(f'#include "debug_{typeName}.h"')
    print("")
    print('static void pad(int depth) { printf("%*s", depth * 4, ""); }')
    print("")
    catalog.printPrintFunctions()
