"""
Vector structure classes for makeAST code generation.

This module contains:
- SimpleVector: Fixed-size arrays with simpler and more efficient flat implementation
"""

from .base import Base
from .fields import SimpleField
from .utils import pad
from .comment_gen import CommentGen


class SimpleVector(Base):
    """
    Simple vectors declared directly in the yaml.
    Vectors are fixed-size arrays with a simpler
    and more efficient flat implementation
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.entries = SimpleField(self.name, "entries", data["entries"])
        else:
            raise ValueError(f"SimpleVector {name} must have 'data' field with 'entries'")

    def isVector(self):
        return True

    def printTypedef(self, catalog):
        self.noteTypedef()
        c = self.comment('printTypedef')
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        print(f"    Index size; {c}")
        self.entries.printVectorTypedefLine(catalog)
        print(f"}} {name}; {c}")
        print("")

    def printMermaid(self, catalog):
        myName = self.getName()
        print(f'{myName}["({myName})"] --entries--> {self.entries.getObjName(catalog)}')

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"{myType} new{myName}(int size)"

    def comment(self, method):
        return CommentGen.method_comment('SimpleVector', method)

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl=self.getNewSignature(catalog)
        print(f"{decl}; {c}")

    def printNewFunction(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        fieldType = self.entries.getTypeDeclaration(catalog)
        decl = self.getNewSignature(catalog)
        c = self.comment('printNewFunction')
        print("/**")
        print(f" * Creates a new {myName} with the given size.")
        print(f" * {myName} is a vector of {self.entries.getObjName(catalog)}.")
        print(" */")
        print(f"{decl} {{ {c}")
        print(f"    {myType} x = NEW_VECTOR(size, {myName}, {fieldType}, {myObjType}); {c}")
        print(f'    DEBUG("new {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName}) + size * sizeof({fieldType})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    x->size = size; {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printCopyField(self, isInline, field, depth, prefix=''):
        myName=self.getName()
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f'x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def getTypeDeclaration(self, catalog):
        return "struct {name} *".format(name=self.getName())

    def getDefineValue(self):
        return 'x'

    def getDefineArg(self):
        return 'x'

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        myName=self.getName()
        a = '.' if isInline else '->'
        pad(depth)
        print(f'print{myName}(x{a}{prefix}{field}, depth+1); {c}')

    def printCompareField(self, catalog, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        myName=self.getName()
        extraCmpArgs = self.getExtraCmpAargs(catalog)
        a = '.' if isInline else '->'
        pad(depth)
        print(f"if (!eq{myName}(a{a}{prefix}{field}, b{a}{prefix}{field}{extraCmpArgs})) return false; {c}")

    def getExtraCmpFargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            ctype = self.getCtype(self.extraCmpArgs[name], catalog)
            extra += [f"{ctype}{name}"]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def getExtraCmpAargs(self, catalog):
        extra = []
        for name in self.extraCmpArgs:
            extra += [name]
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""

    def objTypeArray(self):
        return [ self.getObjType() ]

    def getObjType(self):
        return ('objtype_' + self.getName()).upper()

    def printCopyDeclaration(self, catalog):
        decl = self.getCopySignature(catalog)
        c = self.comment('printCopyDectaration')
        print(f"{decl}; {c}")

    def printCopyFunction(self, catalog):
        c = self.comment('printCopyFunction')
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        fieldType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        decl = self.getCopySignature(catalog)
        print(f"/**")
        print(f" * Creates a deep copy of a {myName} object.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    if (o == NULL) return NULL; {c}")
        print(f"    {myType} x = NEW_VECTOR(o->size, {myName}, {fieldType}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    int save = PROTECT(x); {c}")
        if self.entries.isInline(catalog):
            print(f"    COPY_ARRAY({fieldType}, x->entries, o->entries, o->size); {c}")
        else:
            print(f"    for (Index i = 0; i < o->size; ++i) {{ {c}")
            self.entries.printCopyArrayLine(catalog, "i", 3)
            print(f"    }} {c}")
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def getCopySignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"{myType} copy{myName}({myType} o)"

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl=self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printCompareDeclaration(self, catalog):
        c = self.comment('printCompareDeclaration')
        decl=self.getCompareSignature(catalog)
        print(f"{decl}; {c}")

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return f"bool eq{myName}({myType} a, {myType} b{extraCmpArgs})"

    def printMarkFunction(self, catalog):
        decl = self.getMarkSignature(catalog)
        c = self.comment('printMarkFunction')
        print(f"/**")
        print(f" * Marks a {self.getName()} object to protect it from garbage collection.")
        print(f" * will recursively mark the vector's entries.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    if (x == NULL) return; {c}")
        print(f"    if (MARKED(x)) return; {c}")
        print(f"    MARK(x); {c}")
        print(f"    for (Index i = 0; i < x->size; i++) {{ {c}")
        self.entries.printMarkArrayLine(False, catalog, "i", 2)
        print(f"    }} {c}")
        print(f"}} {c}")
        print("")

    def printCompareFunction(self, catalog):
        decl = self.getCompareSignature(catalog)
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for")
            print(f"// {decl}")
            print("")
            return
        myName = self.getName()
        c = self.comment('printCompareFunction')
        print(f"/**")
        print(f" * Compares two {myName} vectors for deep equality.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    if (a == b) return true; {c}")
        print(f"    if (a == NULL || b == NULL) return false; {c}")
        print(f"    if (a->size != b->size) return false; {c}")
        print(f"    for (Index i = 0; i < a->size; i++) {{ {c}")
        self.entries.printCompareArrayLine(False, catalog, "i", 2)
        print(f"    }} {c}")
        print(f"    return true; {c}")
        print(f"}} {c}")
        print("")

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printCountDeclaration')
        print(f'static inline Index count{myName}({myType} x) {{ {c}')
        print(f'    return x->size; {c}')
        print(f'}} {c}')
        print('')

    def printPrintFunction(self, catalog):
        myName = self.getName()
        decl = self.getPrintSignature(catalog)
        c = self.comment('printPrintFunction')
        print(f"/**")
        print(f" * Prints the contents of a {myName} object for debugging.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        print(f'    eprintf("{myName}(%d)[\\n", x->size); {c}')
        print(f"    for (Index i = 0; i < x->size; i++) {{ {c}")
        self.entries.printPrintArrayLine(False, catalog, "i", 2)
        print(f'        eprintf("\\n"); {c}')
        print(f"    }} {c}")
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}")
        print("")

    def printMarkDeclaration(self, catalog):
        c = self.comment('printMarkDeclaration')
        decl=self.getMarkSignature(catalog)
        print(f"{decl}; {c}")

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void mark{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def printFreeDeclaration(self, catalog):
        c = self.comment('printFreeDeclaration')
        decl=self.getFreeSignature(catalog)
        print(f"{decl}; {c}")

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def printFreeFunction(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        fieldType = self.entries.getTypeDeclaration(catalog)
        decl = self.getFreeSignature(catalog)
        c = self.comment('printFreeFunction')
        print(f"/**")
        print(f" * Frees a {myName} object.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    FREE_VECTOR(x, {myName}, {fieldType}, x->size); {c}")
        print(f"}} {c}")
        print("")

    def printMarkObjCase(self, catalog):
        c = self.comment('printMarkObjCase')
        objType = self.getObjType()
        name = self.getName()
        pad(2)
        print(f'case {objType}: {c}')
        pad(3)
        print(f'mark{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

    def printTypeObjCase(self, catalog):
        objType = self.getObjType()
        name = self.getName()
        c = self.comment('printTypeObjCase')
        pad(2)
        print(f'case {objType}: {c}')
        pad(3)
        print(f'return "{name}"; {c}')

    def printMarkHashField(self, depth):
        c = self.comment('printMarkHashField')
        myName = self.getName()
        pad(depth)
        print(f'mark{myName}(*({myName} **)ptr); {c}')

    def printMarkField(self, isInline, field, depth, prefix=''):
        c = self.comment('printMarkField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f"mark{myName}(x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        myName=self.getName()
        pad(depth)
        a = '.' if isInline else '->'
        print(f"return PROTECT(x{a}{prefix}{field}); {c}")

    def printFreeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printFreeObjCase')
        name=self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'free{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

