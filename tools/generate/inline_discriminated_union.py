"""
InlineDiscriminatedUnion class for inline (stack-allocated) discriminated unions.
"""

from .discriminated_union import DiscriminatedUnion
from .accessor_helper import AccessorHelper

class InlineDiscriminatedUnion(DiscriminatedUnion):
    """
    Inline (call by value) structs live on the stack not the heap, they are
    passed by value not by reference, and they are not directly memory-managed
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
        return f'int protect{myName}({myType} _x)'

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
        print(f'    switch(_x{a}type) {{ {c}')
        for field in self.fields:
            field.printProtectCase(self.isInline(catalog), catalog)
        print(f"        default: {c}")
        print(f'            cant_happen("unrecognised type %d", _x{a}type); {c}')
        print(f'    }} {c}')
        print(f'}} {c}')
        print('')

    def objTypeArray(self):
        return []

    def printCopyDeclaration(self, catalog):
        c = self.comment('printCopyDeclaration')
        decl = self.getCopySignature(catalog)
        print(f"{decl}; {c}")

    def printCopyFunction(self, catalog):
        c = self.comment('printCopyFunction')
        decl = self.getCopySignature(catalog)
        myName = self.getName()
        print(f"/**")
        print(f" * Copies a {myName} union (returns by value).")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    return o; {c}")
        print(f"}} {c}\n")

    def generateVisitorDecl(self):
        """Generate forward declaration for union visitor (dispatcher + variants)"""
        # TODO: Implement union visitor declarations
        return ""

    def generateVisitor(self, catalog, target):
        """Generate union visitor dispatcher and variant visitors"""
        # TODO: Implement union visitor generation
        return ""
