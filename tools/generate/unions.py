"""
Union structure classes for makeAST code generation.

This module contains:
- DiscriminatedUnion: Tagged unions with type field and alternatives
- InlineDiscriminatedUnion: Inline (call-by-value) discriminated unions
- DiscriminatedUnionUnion: Union of union alternatives
"""

from .base import Base, EnumField
from .fields import SimpleField, DiscriminatedUnionField
from .structs import SimpleStruct
from .enums import DiscriminatedUnionEnum
from .utils import pad
from .comment_gen import CommentGen
from .type_helper import TypeHelper
from .accessor_helper import AccessorHelper


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
        for field in self.fields:
            field.printMakeHelperDeclaration(catalog, self, self.isInline(catalog))

    def printGetterDeclarations(self, catalog):
        for field in self.fields:
            field.printGetterDeclaration(catalog, self, self.isInline(catalog))

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
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"    switch(x{a}type) {{ {c}")
        for field in self.fields:
            field.printMarkCase(self.isInline(catalog), catalog)
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d in mark{myName}", x{a}type); {c}')
        print(f"    }} {c}")

    def printCompareFunctionBody(self, catalog):
        c = self.comment('printCompareFunctionBody')
        myName=self.getName()
        a = AccessorHelper.accessor(self.isInline(catalog))
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
        a = AccessorHelper.accessor(self.isInline(catalog))
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
        a = AccessorHelper.accessor(self.isInline(catalog))
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

    def printHelperNewDeclarations(self, catalog):
        # Inline unions don't need make helpers - they're stack-allocated
        # and don't require GC protection
        for field in self.fields:
            field.printHelperNewDeclaration(catalog, self.isInline(catalog))

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
        a = AccessorHelper.accessor(self.isInline(catalog))
        c = self.comment('printProtectFunction')
        print(f"/**")
        print(f" * Protects the {self.getName()} union from garbage collection.")
        print(f" * It will recursively protect the appropriate type of the contained object.")
        print(f" */")
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

    def getName(self):
        return self.name + "Val"

    def getTypeDeclaration(self, catalog):
        return TypeHelper.union_type(self.getName())

    def getFieldName(self):
        return 'val'

    def isUnion(self):
        return True

    def printMermaid(self, catalog):
        pass
    
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

