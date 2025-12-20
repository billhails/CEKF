"""
DiscriminatedUnionUnion class - aggregate containing union-specific data.
"""

from .base import Base
from .type_helper import TypeHelper

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
