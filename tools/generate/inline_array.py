"""
Array structure classes for makeAST code generation.

This module contains:
- SimpleArray: Dynamic 1D/2D arrays with push/pop/peek operations
- SimpleStack: Stack structures with frame pointers and stack pointers
- InlineArray: Call-by-value inline array structures
"""

from .base import Base
from .simple_field import SimpleField
from .utils import pad
from .comment_gen import CommentGen
from .type_helper import TypeHelper
from .signature_helper import SignatureHelper
from .accessor_helper import AccessorHelper
from .compare_helper import EqHelper
from .objtype_helper import ObjectTypeHelper


from .simple_array import SimpleArray

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
        print(f'void init{myName}({typeName} *_x, Index size); {c}')

    def printInitFunction(self, catalog):
        if self.dimension != 1:
            return
        typeName = self.getTypeDeclaration(catalog)
        myName = self.getName()
        c = self.comment('printInitFunction')
        print(f"/**")
        print(f" * Initializes an inline (not directly memory-managed) {myName} with the given size.")
        print(" */")
        print(f'void init{myName}({typeName} *_x, Index size) {{ {c}')
        print(f"    _x->size = 0; {c}")
        print(f"    _x->capacity = 0; {c}")
        print(f"    _x->entries = NULL; {c}")
        print(f"    if (size > 0) {{ {c}")
        print(f"        _x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, size); {c}")
        print(f"        _x->capacity = size; {c}")
        print(f'    }} {c}')
        print(f'}} {c}')
        print("")

    def printMarkObjCase(self, catalog):
        pass
