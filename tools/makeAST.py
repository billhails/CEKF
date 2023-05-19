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

# reads YAML and outputs C code for the Yacc parser AST

import yaml

class Catalog:
    def __init__(self):
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
        for entity in self.contents.values():
            values += [entity]
        for entity in values:
            entity.build(self)

    def printTypedefs(self):
        for entity in self.contents.values():
            entity.printTypedef(self)

    def printMarkDeclarations(self):
        for entity in self.contents.values():
            entity.printMarkDeclaration()

    def printNewDeclarations(self):
        for entity in self.contents.values():
            entity.printNewDeclaration(self)

    def printDefines(self):
        for entity in self.contents.values():
            entity.printDefines(self)

    def printNewFunctions(self):
        for entity in self.contents.values():
            entity.printNewFunction(self)

    def printMarkFunctions(self):
        for entity in self.contents.values():
            entity.printMarkFunction(self)

        


class Base:
    def __init__(self, name, data):
        self.name = name
        self.data = data

    def build(self, catalog):
        pass

    def printMarkCode(self, fieldName, catalog):
        return False

    def isEmpty(self):
        return False

    def getName(self):
        return self.name

    def printTypedef(self, catalog):
        pass

    def printMarkDeclaration(self):
        pass

    def printNewDeclaration(self, catalog):
        pass

    def printDefines(self, catalog):
        pass

    def printNewFunction(self, catalog):
        pass

    def printMarkFunction(self, catalog):
        pass

    def getFieldDeclaration(self):
        raise Exception("sub-class must implement getFieldDeclaration")


class Primitive(Base):
    def __init__(self, name, data):
        super().__init__(name, data)

    def isEmpty(self):
        result = self.data != 0
        return result

    def getDefault(self):
        return self.data

    def getFieldDeclaration(self):
        return self.name + " ";



class Struct(Base):
    def __init__(self, name, data, isDiscriminatedUnion=False):
        super().__init__(name, data)
        self.isDiscriminatedUnion = isDiscriminatedUnion
        self.fields = {}
        for typeName in self.data.keys():
            if type(self.data[typeName]) is str:
                fieldName = self.data[typeName]
                self.fields[fieldName] = typeName
            elif type(self.data[typeName]) is list:
                for fieldName in self.data[typeName]:
                    self.fields[fieldName] = typeName
            else:
                raise Exception("expected str or list in {n}.{f}".format(n=name,f=typeName))
                    
    def printTypedef(self, catalog):
        print("typedef struct {name} {{".format(name=self.name))
        print("    Header header;");
        for field in self.fields.keys():
            obj = catalog.get(self.fields[field])
            objType = obj.getFieldDeclaration()
            print("    {f}{v};".format(f=objType,v=field))
        print("}} {name};\n".format(name=self.name))

    def getFieldDeclaration(self):
        return "struct {name} *".format(name = self.getName())

    def getMarkDeclaration(self):
        return("void mark{name}({name} *x)".format(name = self.getName()))

    def printMarkDeclaration(self):
        print("{d};".format(d = self.getMarkDeclaration()))

    def getNewDeclaration(self, catalog):
        array = []
        for field in self.fields.keys():
            obj = catalog.get(self.fields[field])
            objType = obj.getFieldDeclaration()
            array += ["{t}{n}".format(t=objType, n=field)]
        args = ", ".join(array)

        f = self.getFieldDeclaration()
        n = self.getName()
        s = "{f}new{n}({a})".format(f=f,n=n, a=args)
        return s

    def printNewDeclaration(self, catalog):
        print("{d};".format(d=self.getNewDeclaration(catalog)))

    def printNewFunction(self, catalog):
        print("{d} {{".format(d=self.getNewDeclaration(catalog)))
        print("    {name} *x = NEW({name}, OBJTYPE_AST);".format(name=self.name))
        for field in self.fields.keys():
            print("    x->{field} = {field};".format(field=field))
        print("    return x;");
        print("}\n")

    def printMarkFunction(self, catalog):
        print("{d} {{".format(d=self.getMarkDeclaration()))
        print("    if (x == NULL) return;")
        print("    if (MARKED(x)) return;")
        print("    MARK(x);")
        if self.isDiscriminatedUnion:
            print("    switch (x->type) {")
            unionObj = catalog.get(self.fields['val'])
            unionObj.printMarkCases(catalog)
            print("    }")
        else:
            for field in self.fields.keys():
                obj = catalog.get(self.fields[field])
                obj.printMarkCode(field, catalog)
        print("}\n")

    def printMarkCode(self, fieldName, catalog):
        print("    mark{f}(x->{n});".format(f=self.getName(), n=fieldName))
        return True


class UnionField:
    def __init__(self, name, data, container):
        self.name = name
        self.data = data
        self.container = container

    def getEnumFields(self):
        enumFields = []
        if type(self.data) is str:
            enumFields += [self.data]
        elif type(self.data) is dict:
            count = 0
            for fieldName in self.data:
                count += 1
                if type(self.data[fieldName]) is dict:
                    enumFields += self.data[fieldName].keys()
                elif type(self.data[fieldName]) is list:
                    enumFields += self.data[fieldName]
                else:
                    raise Exception("expected dict or list value in " + self.name + " dict")
            if count != 1:
                raise Exception("expected exactly 1 value in " + self.name + " dict")
        else:
            raise Exception("bad type for union value " + self.name + ", got " + str(type(value)) + ", expected str or dict")
        return enumFields

    def hasValueFor(self, enumField):
        if type(self.data) is str:
            return False
        else:
            for fieldName in self.data:
                if type(self.data[fieldName]) is dict:
                    if enumField in self.data[fieldName]:
                        return True
        return False

    def getValueFor(self, enumField):
        for fieldName in self.data:
            if type(self.data[fieldName]) is dict:
                if enumField in self.data[fieldName]:
                    return self.data[fieldName][enumField]
        raise Exception("can't find value for enumField " + enumField)

    def getFieldName(self):
        if type(self.data) is str:
            return self.data
        elif type(self.data) is dict:
            name = ''
            count = 0
            for fieldName in self.data.keys():
                count += 1
                name = fieldName
            if count != 1:
                raise Exception("expected exactly 1 value in " + self.name + " dict")
            return name
        else:
            raise Exception("bad type for union value " + self.name + ", got " + str(type(value)) + ", expected str or dict")
        
    def printTypedefField(self, catalog):
        obj = catalog.get(self.name);
        print("    {f}{n};".format(f=obj.getFieldDeclaration(), n=self.getFieldName()))
    
    def printDefines(self, catalog):
        obj = catalog.get(self.name)
        fieldName = self.getFieldName()
        enumFields = self.getEnumFields()
        for enumField in enumFields:
            v = self.container + "_" + enumField
            v = v.upper().replace("VAL_", "_VAL_").replace("AST", "AST_")
            arg = "x"
            val = "x"
            if obj.isEmpty():
                arg = ""
                val = obj.getDefault()
            elif self.hasValueFor(enumField):
                arg = ""
                val = self.getValueFor(enumField)
            print("#define {v}({a}) (({p}){{.{f} = ({x})}})".format(v=v,a=arg,p=self.container, f=fieldName, x=val))

    def printMarkCase(self, catalog):
        enumFields = self.getEnumFields()
        for enumField in enumFields:
            v = self.container + "_" + enumField
            v = v.upper().replace("VAL_", "_TYPE_").replace("AST", "AST_")
            print("        case {f}:".format(f=v))
            obj = catalog.get(self.name)
            print("        ", end='')
            if obj.printMarkCode(enumField, catalog):
                print("            break;")
            else:
                print("    break;");



class Union(Base):
    def __init__(self, name, data):
        super().__init__(name, data)
        self.fields = {}
        for typeName in self.data:
            unionField = UnionField(typeName, self.data[typeName], self.getName())
            fieldName = unionField.getFieldName()
            self.fields[fieldName] = unionField;

    def build(self, catalog):
        enumName = self.name + "Type"
        structName = self.name
        enumFields = []

        catalog.add(Struct(structName, {enumName: "type", self.getName(): "val"}, True))

        for unionField in self.fields.values():
            enumFields = enumFields + unionField.getEnumFields()

        catalog.add(Enum(enumName, enumFields))

    def getName(self):
        return self.name + "Val"

    def getFieldDeclaration(self):
        return "union {name} ".format(name = self.getName())

    def printTypedef(self, catalog):
        print("typedef union {name} {{".format(name=self.getName()))
        for field in self.fields.values():
            field.printTypedefField(catalog);
        print("}} {name};\n".format(name=self.getName()))

    def printDefines(self, catalog):
        for unionField in self.fields.values():
            unionField.printDefines(catalog)

    def printMarkCases(self, catalog):
        for unionField in self.fields.values():
            unionField.printMarkCase(catalog)
        print("        default:");
        print('            cant_happen("unrecognised type in mark{f} %d", x->type);'.format(f=self.name));
        


class Enum(Base):
    def printTypedef(self, catalog):
        print("typedef enum {name} {{".format(name=self.name))
        for v in self.data:
            v = self.name + "_" + v
            v = v.upper().replace("TYPE_", "_TYPE_").replace("AST", "AST_")
            print("    {val},".format(val=v))
        print("}} {name};\n".format(name=self.name))

    def getFieldDeclaration(self):
        return "enum {name} ".format(name = self.getName())


stream = open('./src/ast.yaml', 'r');

document = yaml.load(stream, Loader=yaml.Loader)

catalog = Catalog()
catalog.add(Primitive("hash_t", 0))
catalog.add(Primitive("char*", 0))
catalog.add(Primitive("char *", 0))
catalog.add(Primitive("void*", "NULL"))
catalog.add(Primitive("void *", "NULL"))
catalog.add(Primitive("int", 0))
catalog.add(Primitive("char", 0))
catalog.add(Primitive("bool", 0))

for name in document["structs"]:
    catalog.add(Struct(name, document["structs"][name]))

for name in document["unions"]:
    catalog.add(Union(name, document["unions"][name]))

for name in document["enums"]:
    catalog.add(Enum(name, document["enums"][name]))

catalog.build()
catalog.printTypedefs()
catalog.printNewDeclarations()
print("")
catalog.printMarkDeclarations()
print("")
catalog.printDefines()
print("")
catalog.printNewFunctions()
print("")
catalog.printMarkFunctions()
