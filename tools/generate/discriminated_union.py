"""
DiscriminatedUnion class for tagged unions with type field.
"""

from .simple_struct import SimpleStruct
from .discriminated_union_field import DiscriminatedUnionField
from .discriminated_union_enum import DiscriminatedUnionEnum
from .accessor_helper import AccessorHelper

# Forward reference - DiscriminatedUnionUnion will be imported from discriminated_union_union.py
# but we can't import it yet due to circular dependency
class DiscriminatedUnion(SimpleStruct):
    """
    Contains the data from a union specification in the yaml.
    Prints as the struct { type, val }
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # Import here to avoid circular dependency at module load time
        from .discriminated_union_union import DiscriminatedUnionUnion
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
        print(f"    switch(_x{a}type) {{ {c}")
        for field in self.fields:
            field.printMarkCase(self.isInline(catalog), catalog)
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d in mark{myName}", _x{a}type); {c}')
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
        print(f'    _x{a}type = o{a}type; {c}')

    def printPrintFunctionBody(self, catalog):
        c = self.comment('printPrintFunctionBody')
        myName=self.getName()
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"    switch(_x{a}type) {{ {c}")
        for field in self.fields:
            field.printPrintCase(catalog, self.isInline(catalog))
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d in print{myName}", _x{a}type); {c}')
        print(f"    }} {c}")
        print(f'    eprintf("\\n"); {c}')
    
    def generateVisitorDecl(self):
        """Generate forward declaration for union visitor (dispatcher + variants)"""
        # TODO: Implement union visitor declarations
        return ""

    def generateVisitor(self, catalog):
        """Generate union visitor dispatcher and variant visitors"""
        # TODO: Implement union visitor generation
        return ""
