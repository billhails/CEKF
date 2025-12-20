"""
InlineStruct class for inline (call-by-value) structs.
"""

from .simple_struct import SimpleStruct

class InlineStruct(SimpleStruct):
    """
    Inline (call by value) structs live on the stack not the heap, they are
    used for small data structures that don't need GC. They have no Header
    field and are passed by value rather than by pointer.
    
    These are useful for nested configurations or small value types that
    are always part of a larger structure.
    """
    
    def isInline(self, catalog):
        return True
    
    def printNewFunction(self, catalog):
        # Inline structs don't have a heap allocator - they're initialized inline
        # Generate a simple constructor that returns by value
        c = self.comment('printNewFunction')
        myName = self.getName()
        sig = self.getNewSignature(catalog)
        print(f"/**")
        print(f" * Creates a new {myName} struct with the given arguments.")
        print(f" */")
        print(f"{sig} {{ {c}")
        print(f"    {self.getTypeDeclaration(catalog)} _x; {c}")
        for field in self.fields:
            fname = field.getName()
            if field.default is not None:
                print(f"    _x.{fname} = {field.default}; {c}")
            else:
                print(f"    _x.{fname} = {fname}; {c}")
        print(f"    return _x; {c}")
        print(f"}} {c}\n")
    
    def printNewDeclaration(self, catalog):
        # Inline structs do need a constructor declaration
        c = self.comment('printNewDeclaration')
        sig = self.getNewSignature(catalog)
        print(f"{sig}; {c}")
    
    def printMarkDeclaration(self, catalog):
        # Inline structs need mark declarations for their fields
        c = self.comment('printMarkDeclaration')
        decl = self.getMarkSignature(catalog)
        print(f"{decl}; {c}")
    
    def printMarkFunction(self, catalog):
        # Mark function for inline struct - marks any pointer fields
        c = self.comment('printMarkFunction')
        decl = self.getMarkSignature(catalog)
        myName = self.getName()
        print(f"/**")
        print(f" * Marks all pointer fields in a {myName} struct for GC.")
        print(f" */")
        print(f"{decl} {{ {c}")
        # Mark all fields using their printMarkLine method
        for field in self.fields:
            field.printMarkLine(True, catalog, 1)  # True = isInline
        print(f"}} {c}\n")
    
    def printFreeDeclaration(self, catalog):
        # Inline structs don't need free functions
        pass
    
    def printFreeFunction(self, catalog):
        # No free function needed
        pass
    
    def printCopyDeclaration(self, catalog):
        # Inline structs are copied by value, but we still provide a copy function
        c = self.comment('printCopyDeclaration')
        decl = self.getCopySignature(catalog)
        print(f"{decl}; {c}")
    
    def printCopyFunction(self, catalog):
        # Inline struct copy just returns the value
        c = self.comment('printCopyFunction')
        decl = self.getCopySignature(catalog)
        myName = self.getName()
        print(f"/**")
        print(f" * Copies a {myName} struct (returns by value).")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    return o; {c}")
        print(f"}} {c}\n")
    
    def generateVisitorDecl(self):
        """Generate forward declaration for visitor function"""
        myName = self.getName()
        return f"static {myName} *visit{myName}({myName} *node, VisitorContext *context);\n"
    
    def objTypeArray(self):
        # Inline structs don't participate in object type dispatch
        return []
    
    def getObjType(self):
        # Inline structs don't have object types
        return None
