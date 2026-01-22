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
        if catalog.getParserInfo(self.name):
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

    def printSetterDeclarations(self, catalog):
        for field in self.fields:
            field.printSetterDeclaration(catalog, self, self.isInline(catalog))

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

    def printEqFunctionBody(self, catalog):
        c = self.comment('printEqFunctionBody')
        myName=self.getName()
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"    if (a{a}type != b{a}type) return false; {c}")
        print(f"    switch(a{a}type) {{ {c}")
        for field in self.fields:
            field.printEqCase(self.isInline(catalog), catalog)
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
    
    def generateVisitorDecl(self, target):
        """Generate forward declaration for union visitor (dispatcher)"""
        myName = self.getName()
        return f"static {myName} *{target}{myName}({myName} *node, VisitorContext *context);\n"

    def generateVisitor(self, catalog, target):
        """Generate union visitor dispatcher that switches on type and calls variant visitors"""
        myName = self.getName()
        output = []
        
        output.append(f"static {myName} *{target}{myName}({myName} *node, VisitorContext *context) {{\n")
        output.append(f"    ENTER({target}{myName});\n")
        output.append(f"    if (node == NULL) {{\n")
        output.append(f"        LEAVE({target}{myName});\n")
        output.append(f"        return NULL;\n")
        output.append(f"    }}\n")
        output.append(f"\n")
        output.append(f"    int save = PROTECT(NULL);\n")
        output.append(f"    {myName} *result = node;\n")
        output.append(f"\n")
        output.append(f"    switch (node->type) {{\n")
        
        # Generate a case for each variant
        for field in self.fields:
            variantName = field.getName()
            variantType = field.typeName
            enumVal = field.makeTypeName()
            
            output.append(f"        case {enumVal}: {{\n")
            output.append(f"            // {variantType}\n")
            
            # Check if this type needs protection (i.e., is it memory-managed?)
            try:
                entity = catalog.get(variantType)
                needsProtection = entity.needsProtection(catalog)
            except:
                # If type not found in catalog, assume it needs protection
                needsProtection = True
            
            if not needsProtection:
                # Primitive or non-GC type - leave empty case for user to fill
                output.append(f"            break;\n")
            else:
                # Use generated getter for type-safe variant extraction
                variantNameCap = variantName[0].upper() + variantName[1:] if variantName else variantName
                output.append(f"            {variantType} *variant = get{myName}_{variantNameCap}(node);\n")
                output.append(f"            {variantType} *new_variant = {target}{variantType}(variant, context);\n")
                output.append(f"            if (new_variant != variant) {{\n")
                output.append(f"                PROTECT(new_variant);\n")
                
                # Determine constructor call based on parser info
                if catalog.getParserInfo(self.name):
                    output.append(f"                result = new{myName}_{variantNameCap}(CPI(node), new_variant);\n")
                else:
                    output.append(f"                result = new{myName}_{variantNameCap}(new_variant);\n")
                
                output.append(f"            }}\n")
                output.append(f"            break;\n")
            
            output.append(f"        }}\n")
        
        output.append(f"        default:\n")
        output.append(f'            cant_happen("unrecognized {myName} type %d", node->type);\n')
        output.append(f"    }}\n")
        output.append(f"\n")
        output.append(f"    UNPROTECT(save);\n")
        output.append(f"    LEAVE({target}{myName});\n")
        output.append(f"    return result;\n")
        output.append(f"}}\n\n")
        
        return ''.join(output)
