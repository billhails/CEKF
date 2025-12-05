"""
Array structure classes for makeAST code generation.

This module contains:
- SimpleArray: Dynamic 1D/2D arrays with push/pop/peek operations
- SimpleStack: Stack structures with frame pointers and stack pointers
- InlineArray: Call-by-value inline array structures
"""

from .base import Base
from .fields import SimpleField
from .utils import pad
from .comment_gen import CommentGen


class SimpleArray(Base):
    """
    Array structures declared in the yaml
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.dimension = data["dimension"] if "dimension" in data else 1
            if self.dimension > 2:
                raise Exception("only 1 or 2 dimensional arrays supported for now")
            if self.dimension == 2:
                self.width = SimpleField(self.name, "width", "int")
                self.height = SimpleField(self.name, "height", "int")
            self.entries = SimpleField(self.name,"entries", data["entries"])
        else:
            raise Exception(f"SimpleArray {name} must have 'data' field")

    def printMermaid(self, catalog):
        myName = self.getName()
        mySpec = '[]' * self.dimension
        print(f'{myName}["{myName}{mySpec}"] --entries--> {self.entries.getObjName(catalog)}')

    def getDefineValue(self):
        return 'x'

    def getDefineArg(self):
        return 'x'

    def tag(self):
        super().tag()
        self.tagField = SimpleField(self.name, "_tag", "string")

    def getTypeDeclaration(self, catalog):
        a = '' if self.isInline(catalog) else '*'
        name = self.getName()
        return f"struct {name} {a}"

    def printCompareField(self, catalog, isInline, field, depth, prefix=''):
        myName=self.getName()
        extraCmpArgs = self.getExtraCmpAargs(catalog)
        a = '.' if isInline else '->'
        c = self.comment('printCompareField')
        pad(depth)
        print(f"if (!eq{myName}(a{a}{prefix}{field}, b{a}{prefix}{field}{extraCmpArgs})) return false; {c}")

    def comment(self, method):
        return CommentGen.method_comment('SimpleArray', method)

    def printCopyField(self, isInline, field, depth, prefix=''):
        myName=self.getName()
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f'x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        pad(depth)
        myName=self.getName()
        print(f'print{myName}(*({myName} **)ptr, depth + 1); {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        myName=self.getName()
        a = '.' if isInline else '->'
        pad(depth)
        print(f'print{myName}(x{a}{prefix}{field}, depth + 1); {c}')

    def printAccessDeclarations(self, catalog):
        entryType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printAccessDeclarations')
        if self.dimension == 2:
            print(f"static inline {entryType} get{myName}Index({myType} obj, Index x, Index y) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x >= obj->width || y >= obj->height) {{ {c}")
            print(f'        cant_happen("{myName} 2d matrix bounds exceeded"); {c}')
            print(f"    }}")
            print(f"#endif")
            print(f"    return obj->entries[x + y * obj->width]; {c}")
            print(f"}} {c}")
            print("")
            print(f"static inline void set{myName}Index({myType} obj, Index x, Index y, {entryType} val) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x >= obj->width || y >= obj->height) {{ {c}")
            print(f'        cant_happen("{myName} 2d matrix bounds exceeded"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    obj->entries[x + y * obj->width] = val; {c}")
            print(f"}} {c}")
            print("")

    def printIndexFields(self):
        c = self.comment('printIndexFields')
        print(f"    Index size; {c}")
        print(f"    Index capacity; {c}")

    def printExtraStackEntries(self):
        pass

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        if self.tagged:
            print(f"    char *_tag; {c}")
        if self.dimension == 2: # 2D arrays are fixed size
            print(f"    Index width; {c}")
            print(f"    Index height; {c}")
        else:                   # 1D arrays can grow
            self.printIndexFields()
        self.entries.printArrayTypedefLine(catalog)
        self.printExtraStackEntries()
        print(f"}} {name}; {c}\n")

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

    def printFreeFunction(self, catalog):
        myName = self.getName()
        decl = decl=self.getFreeSignature(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printFreeFunction')
        print(f"/**")
        print(f" * @brief Free a {myName}.")
        print(f" *")
        print(f" * This function frees the memory allocated for the {myName} structure")
        print(f" */")
        print(f"{decl} {{ {c}")
        if self.dimension == 1:
            print(f"    FREE_ARRAY({entryType}, x->entries, x->capacity); {c}")
        else:
            print(f"    FREE_ARRAY({entryType}, x->entries, x->width * x->height); {c}")
        print(f"    FREE(x, {myName}); {c}")
        print(f"}} {c}")
        print("")

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void free{myName}({myType} x)".format(myName=self.getName(), myType=myType)

    def getObjType(self):
        return ('objtype_' + self.getName()).upper()

    def objTypeArray(self):
        return [ self.getObjType() ]

    def getNewArgs(self, catalog):
        if self.tagged:
            return [self.tagField] + self._getNewArgs()
        else:
            return self._getNewArgs()

    def _getNewArgs(self):
        if self.dimension == 1:
            return []
        return [self.width, self.height]

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        args = []
        for field in self.getNewArgs(catalog):
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        return "{myType} new{myName}({args})".format(myType=myType, myName=self.getName(), args=', '.join(args))

    def getCopySignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"{myType} copy{myName}({myType} o)"

    def printNullEntries(self):
        c = self.comment('printNullEntries')
        print(f"    x->entries = NULL; {c}")

    def printZeroCapacities(self):
        c = self.comment('printZeroCapacities')
        print(f"    x->size = 0; {c}")
        print(f"    x->capacity = 0; {c}")

    def printInitEntries(self, catalog):
        c = self.comment('printInitEntries')
        print(f"    x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, 8); {c}")
        print(f"    x->capacity = 8; {c}")

    def printNewFunction(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        decl = self.getNewSignature(catalog)
        c = self.comment('printNewFunction')
        print("/**")
        print(f" * @brief Create a new {myName}.")
        print(f" * This generated function initializes a new {myName} structure,")
        print(f" * which is an array of {self.entries.getTypeDeclaration(catalog)}.")
        print(" */")
        print(f"{decl} {{ {c}")
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("new {myName} %p", x); {c}')
        if self.tagged:
            print(f"    x->_tag = _tag; {c}")
        self.printNullEntries()
        if self.dimension == 1:
            self.printZeroCapacities()
            print(f"    int save = PROTECT(x); {c}")
            self.printInitEntries(catalog)
        else:
            print(f"    x->width = 0; {c}")
            print(f"    x->height = 0; {c}")
            print(f"    int save = PROTECT(x); {c}")
            print(f"    if (width * height > 0) {{ {c}")
            print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, width * height); {c}")
            print(f"        bzero(x->entries, sizeof({self.entries.getTypeDeclaration(catalog)}) * width * height); {c}")
            print(f"    }} {c}")
            print(f"    x->width = width; {c}")
            print(f"    x->height = height; {c}")
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl=self.getNewSignature(catalog)
        print(f"{decl}; {c}")

    def printCopyDeclaration(self, catalog):
        c = self.comment('printCopyDeclaration')
        decl=self.getCopySignature(catalog)
        print(f"{decl}; {c}")

    def printExtendDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printExtendDeclaration')
            print(f"void extend{name}({myType} {a}obj, Index size); {c}")

    def printSizeDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printSizeDeclaration')
            print(f"static inline Index size{name}({myType} {a}obj) {{ return obj->size; }} {c}")

    def printPushDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPushDeclaration')
            print(f"Index push{name}({myType} {a}obj, {entryType} entry); {c}")

    def printPopDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopDeclaration')
            print(f"{entryType} pop{name}({myType} {a}obj); {c}")

    def printPopnDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopnDeclaration')
            print("#ifdef SAFETY_CHECKS")
            print(f"void popn{name}({myType} {a}obj, int n); {c}")
            print("#else")
            print(f"static inline void popn{name}({myType} {a}obj, int n) {{ obj->size -= n; }}; {c}")
            print("#endif")

    def printMoveDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printMoveDeclaration')
            print(f"void move{name}({myType} {a}obj, int b, int n); {c}")

    def printPushnDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPushnDeclaration')
            print(f"void pushn{name}({myType} {a}obj, int n, {entryType} entry); {c}")

    def printCopyTopDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyTopDeclaration')
            print(f"void copyTop{name}({myType} {a}dest, {myType} {a}src, int n); {c}")

    def printCopyExceptTopDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyExceptTopDeclaration')
            print(f"void copyExceptTop{name}({myType} {a}dest, {myType} {a}src, int n); {c}")

    def printCopyEntriesDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyEntriesDeclaration')
            print(f"void copy{name}Entries({myType} {a}dest, {myType} {a}src); {c}")

    def printClearDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printClearDeclaration')
            print(f"static inline void clear{name}({myType} {a}obj) {{ obj->size = 0; }}; {c}")

    def printPeekDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeekDeclaration')
            print(f"{entryType} peek{name}({myType} {a}obj); {c}")

    def printPeeknDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeeknDeclaration')
            print(f"{entryType} peekn{name}({myType} {a}obj, int offset); {c}")

    def printPokeDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPokeDeclaration')
            print(f"void poke{name}({myType} {a}obj, int offset, {entryType} val); {c}")

    def printExtendFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            c = self.comment('printExtendFunction')
            a = '*' if self.isInline(catalog) else ''
            print(f"/**")
            print(f" * Ensures that the {myType} `x` has at least a capacity of `size`.")
            print(f" */")
            print(f"void extend{name}({myType} {a}x, Index size) {{ {c}")
            print(f'    DEBUG("extend{name}(%p, %u)", x, size);')
            print(f"    if (size > 0) {{ {c}")
            print(f"        size = size < 8 ? 8 : size; {c}")
            print(f"        if (x->capacity == 0) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"            if (x->entries != NULL) {{ {c}")
            print(f'                cant_happen("{name} non-null entries with zero capacity"); {c}')
            print(f"            }} {c}")
            print(f"#endif")
            print(f"            x->entries = NEW_ARRAY({entryType}, size); {c}")
            print(f"            x->capacity = size; {c}")
            print(f"        }} else {{ {c}")
            print(f"            while (size > x->capacity) {{ {c}")
            print(f"                x->entries = GROW_ARRAY({entryType}, x->entries, x->capacity, x->capacity *2); {c}")
            print(f"                x->capacity *= 2; {c}")
            print(f"            }} {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}\n")

    def printPushFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            c = self.comment('printPushFunction')
            a = '*' if self.isInline(catalog) else ''
            print(f"/**")
            print(f" * Pushes `entry` on to `x`, extending `x` if required.")
            print(f" * Returns the stack pointer after the push.")
            print(f" */")
            print(f"Index push{name}({myType} {a}x, {entryType} entry) {{ {c}")
            print(f'    DEBUG("push{name}(%p)", x);')
            print(f"    if (x->size + 1 > x->capacity) {{ {c}")
            print(f"        extend{name}(x, x->size + 1); {c}")
            print(f"    }} {c}")
            print(f"    x->entries[x->size++] = entry; {c}")
            print(f"    return x->size - 1; {c}")
            print(f"}} {c}\n")

    def printPopFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopFunction')
            print(f"/**")
            print(f" * Pops the top entry from `x` and returns it.")
            print(f" */")
            print(f"{entryType} pop{name}({myType} {a}x) {{ {c}")
            print(f'    DEBUG("pop{name}(%p)", x);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x->size == 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return x->entries[--(x->size)]; {c}")
            print(f"}} {c}\n")

    def printPopnFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopnFunction')
            print(f"/**")
            print(f" * Discards the top `n` entries from `x`.")
            print(f" */")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"void popn{name}({myType} {a}x, int n) {{ {c}")
            print(f'    DEBUG("popn{name}(%p, %d)", x, n);')
            print(f"    if (((int) x->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, x->size); {c}')
            print(f"    }} {c}")
            print(f"    x->size -= n; {c}")
            print(f"}} {c}")
            print(f"#endif")
            print("")

    def printMoveFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printMoveFunction')
            print(f"/**")
            print(f" * Safe move `n` entries from `sp - n` to `b`,")
            print(f" * sets sp to `b + n`.")
            print(f" */")
            print(f"void move{name}({myType} {a}x, int b, int n) {{ {c}")
            print(f'    DEBUG("move{name}(%p, %d, %d)", x, b, n);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (((int) x->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, x->size); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(x, b + n); {c}")
            print(f"        MOVE_ARRAY({entryType}, &x->entries[b], &x->entries[x->size - n], n); {c}")
            print(f"    }} {c}")
            print(f"    x->size = (Index) (b + n); {c}")
            print(f"}} {c}\n")

    def printPushnFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPushnFunction')
            print(f"/**")
            print(f" * Pushes `n` copies of `entry` on to `x`.")
            print(f" */")
            print(f"void pushn{name}({myType} {a}x, int n, {entryType} entry) {{ {c}")
            print(f'    DEBUG("pushn{name}(%p, %d)", x, n);')
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(x, x->size + n); {c}")
            print(f"        while (n-- > 0) {{ {c}")
            print(f"            x->entries[x->size++] = entry; {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}\n")

    def printCopyTopFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyTopFunction')
            print(f"/**")
            print(f" * Copies the top `n` entries from `src` to the base of `dest`,")
            print(f" * sets `dest` size (sp) to `n`.")
            print(f" */")
            print(f"void copyTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
            print(f'    DEBUG("copyTop{name}(%p, %p, %d)", dest, src, n);')
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(dest, n); {c}")
            print(f"        COPY_ARRAY({entryType}, dest->entries, &src->entries[src->size - n], n); {c}")
            print(f"    }} {c}")
            print(f"    dest->size = n; {c}")
            print(f"}} {c}\n")

    def printCopyExceptTopFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyExceptTopFunction')
            print(f"/**")
            print(f" * Copies all but top `n` entries from `src` to the base of `dest`,")
            print(f" * sets `dest` sp to `src->sp - n`.")
            print(f" */")
            print(f"void copyExceptTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
            print(f'    DEBUG("copyExceptTop{name}(%p, %p, %d)", dest, src, n);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (((int) src->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, src->size); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    if (((Index) n) < src->size) {{ {c}")
            print(f"        extend{name}(dest, src->size - n); {c}")
            print(f"        COPY_ARRAY({entryType}, dest->entries, src->entries, src->size - n); {c}")
            print(f"    }} {c}")
            print(f"    dest->size = src->size - n; {c}")
            print(f"}} {c}\n")

    def printCopyEntriesFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printCopyEntriesFunction')
            print(f"/**")
            print(f" * Copies all entries from `src` to `dest`,")
            print(f" * sets `dest` sp to `src->sp`.")
            print(f" */")
            print(f"void copy{name}Entries({myType} {a}dest, {myType} {a}src) {{ {c}")
            print(f'    DEBUG("copyEntries{name}(%p, %p)", dest, src);')
            print(f"    extend{name}(dest, src->size); {c}")
            print(f"    COPY_ARRAY({entryType}, dest->entries, src->entries, src->size); {c}")
            print(f"    dest->size = src->size; {c}")
            print(f"}} {c}\n")

    def printPeekFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeekFunction')
            print(f"/**")
            print(f" * Returns the value at the top of `x`.")
            print(f" */")
            print(f"{entryType} peek{name}({myType} {a}x) {{ {c}")
            print(f'    DEBUG("peek{name}(%p)", x);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (x->size == 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return x->entries[x->size - 1]; {c}")
            print(f"}} {c}\n")

    def printPeeknFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPeeknFunction')
            print(f"/**")
            print(f" * If `n` is negative, returns the value at `sp - |n|`,")
            print(f" * otherwise returns the value at `n`.")
            print(f" */")
            print(f"{entryType} peekn{name}({myType} {a}x, int offset) {{ {c}")
            print(f'    DEBUG("peekn{name}(%p, %d)", x, offset);')
            print(f"    if (offset < 0) offset = ((int) x->size) + offset; {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (offset >= (int) x->size) {{ {c}")
            print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->size); {c}')
            print(f"    }} {c}")
            print(f"    if (offset < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return x->entries[offset]; {c}")
            print(f"}} {c}\n")

    def printPokeFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPokeFunction')
            print(f"/**")
            print(f" * If `n` is negative, replaces the value at `sp - |n|`,")
            print(f" * otherwise replaces the value at `n`.")
            print(f" */")
            print(f"void poke{name}({myType} {a}x, int offset, {entryType} val) {{ {c}")
            print(f'    DEBUG("poke{name}(%p, %d)", x, offset);')
            print(f"    if (offset < 0) offset = ((int) x->size) + offset; {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (offset >= (int) x->size) {{ {c}")
            print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->size); {c}')
            print(f"    }} {c}")
            print(f"    if (offset < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    x->entries[offset] = val; {c}")
            print(f"}} {c}\n")

    def printMarkFunction(self, catalog):
        decl = self.getMarkSignature(catalog)
        c = self.comment('printMarkFunction')
        print(f"/**")
        print(f" * @brief Mark a {self.getName()} to protect it from garbage collection.")
        print(f" *")
        print(f" * This function recursively marks the {self.getName()} structure `x` to protect it from garbage collection.")
        print(f" */")
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (x == NULL) return; {c}")
            print(f"    if (MARKED(x)) return; {c}")
            print(f"    MARK(x); {c}")
        self.printMarkFunctionBody(catalog)
        print(f"}} {c}")
        print("")

    def printMarkFunctionBody(self, catalog):
        if self.dimension == 1:
            self.printMark1dFunctionBody(catalog)
        else:
            self.printMark2dFunctionBody(catalog)

    def printMark1dFunctionBody(self, catalog):
        c = self.comment('print1dFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    for (Index i = 0; i < x{a}size; i++) {{ {c}")
        self.entries.printMarkArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")

    def printMark2dFunctionBody(self, catalog):
        c = self.comment('print2dFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    Index size = x{a}width * x{a}height; {c}")
        print(f"    for (Index i = 0; i < size; i++) {{ {c}")
        self.entries.printMarkArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl=self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printCompareDeclaration(self, catalog):
        c = self.comment('printCompareDeclaration')
        decl=self.getCompareSignature(catalog)
        print(f"{decl}; {c}")

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return "void print{myName}({myType} x, int depth)".format(myName=self.getName(), myType=myType)

    def getCtype(self, astType, catalog):
        return f"{astType} *"

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printCountDeclaration')
        a = '.' if self.isInline(catalog) else '->'
        print(f'static inline Index count{myName}({myType} x) {{ {c}')
        if self.dimension == 1:
            print(f'    return x{a}size; {c}')
        else:
            print(f'    return x{a}width * x{a}height; {c}')
        print(f'}} {c}')
        print('')

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

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return f"bool eq{myName}({myType} a, {myType} b{extraCmpArgs})"

    def printCompareFunction(self, catalog):
        c = self.comment('printCompareFunction')
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for")
            print("// {decl}".format(decl=self.getCompareSignature(catalog)))
            print("")
            return
        myName = self.getName()
        decl = self.getCompareSignature(catalog)
        a = '.' if self.isInline(catalog) else '->'
        print(f"/**")
        print(f" * @brief Deep compare two {myName} objects for equality.")
        print(f" */")
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (a == b) return true; {c}")
            print(f"    if (a == NULL || b == NULL) return false; {c}")
        if self.dimension == 1:
            print(f"    if (a{a}size != b{a}size) return false; {c}")
            print(f"    for (Index i = 0; i < a{a}size; i++) {{ {c}")
            self.entries.printCompareArrayLine(self.isInline(catalog), catalog, "i", 2)
            print(f"    }} {c}")
        else:
            print(f"    if (a{a}width != b{a}width || a{a}height != b{a}height) return false; {c}")
            print(f"    for (Index i = 0; i < (a{a}width * a{a}height); i++) {{ {c}")
            self.entries.printCompareArrayLine(self.isInline(catalog), catalog, "i", 2)
            print(f"    }} {c}")
        print(f"    return true; {c}")
        print(f"}} {c}\n")

    def printCopyFunction(self, catalog):
        c = self.comment('printCopyFunction')
        decl = self.getCopySignature(catalog)
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"/**")
        print(f" * @brief Creates a deep copy of the {myName} object `o`.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    if (o == NULL) return NULL; {c}")
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    int save = PROTECT(x); {c}")
        self.printCopyFunctionBody(catalog)
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}\n")

    def printCopyFunctionBody(self, catalog):
        if self.dimension == 1:
            self.print1dCopyFunctionBody(catalog)
        else:
            self.print2dCopyFunctionBody(catalog)

    def print1dCopyFunctionBody(self, catalog):
        c = self.comment('print1dCopyFunctionBody')
        entryType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        print(f"    if (o->entries != NULL) {{ {c}")
        print(f"        extend{myName}(x, o->size); {c}")
        if self.entries.isInline(catalog):
            print(f"        COPY_ARRAY({entryType}, x->entries, o->entries, o->size); {c}")
            print(f"        x->size = o->size; {c}")
        else:
            print(f"        x->size = 0; {c}")
            print(f"        for (Index i = 0; i < o->size; i++) {{ {c}")
            self.entries.printCopyArrayLine(catalog, "i", 3)
            print(f"            x->size++; {c}")
            print(f"        }} {c}")
        print(f"    }} else {{ {c}")
        print(f"        x->size = 0; {c}")
        print(f"    }} {c}")

    def print2dCopyFunctionBody(self, catalog):
        c = self.comment('print2dCopyFunctionBody')
        print(f"    if (o->entries != NULL) {{ {c}")
        print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, x->width * x->height); {c}")
        print(f"        x->width = 0; {c}")
        print(f"        x->height = 0; {c}")
        print(f"        for (Index i = 0; i < (o->width * o->height); i++) {{ {c}")
        self.entries.printCopyArrayLine(catalog, "i", 3)
        print(f"        }} {c}")
        print(f"        x->height = o->height; {c}")
        print(f"        x->width = o->width; {c}")
        print(f"    }} {c}")

    def printPrintFunction(self, catalog):
        myName = self.getName()
        decl = self.getPrintSignature(catalog)
        a = '.' if self.isInline(catalog) else '->'
        c = self.comment('printPrintFunction')
        print(f"/**")
        print(f" * @brief Prints the {myName} object `x` for debugging.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        if not self.isInline(catalog):
            print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        if self.tagged:
            print(f'    eprintf("<<%s>>", x{a}_tag); {c}')
        if self.dimension == 1:
            print(f'    eprintf("{myName}(%d)[\\n", x{a}size); {c}')
        else:
            print(f'    eprintf("{myName}(%d * %d)[\\n", x{a}width, x{a}height); {c}')
        self.printPrintFunctionBody(catalog)
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}")
        print("")

    def printPrintFunctionBody(self, catalog):
        if self.dimension == 1:
            self.print1dPrintFunctionBody(catalog)
        else:
            self.print2dPrintFunctionBody(catalog)

    def print1dPrintFunctionBody(self, catalog):
        c = self.comment('print1dPrintFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    for (Index i = 0; i < x{a}size; i++) {{ {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f'        eprintf("\\n"); {c}')
        print(f"    }} {c}")

    def print2dPrintFunctionBody(self, catalog):
        c = self.comment('print2dPrintFunctionBody')
        a = '.' if self.isInline(catalog) else '->'
        print(f"    for (Index i = 0; i < x{a}height; i++) {{ {c}")
        print(f"        pad(depth); {c}")
        print(f'        eprintf("[\\n"); {c}')
        print(f"        for (Index j = 0; j < x{a}width; j++) {{ {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, f"i * x{a}width + j", 3)
        print(f'            eprintf("\\n"); {c}')
        print(f"        }} {c}")
        print(f"        pad(depth); {c}")
        print(f'        eprintf("]\\n"); {c}')
        print(f"    }} {c}")

    def printFreeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printFreeObjCase')
        name = self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'free{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

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
        if self.isInline(catalog):
            return
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
        pad(depth)
        a = '.' if isInline else '->'
        print(f"return PROTECT(x{a}{prefix}{field}); {c}")

    def getIterator1DDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        myContainedType = self.entries.getTypeDeclaration(catalog)
        return f'bool iterate{myName}({myType} table, Index *i, {myContainedType} *res, bool *more)'

    def getIterator2DDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        myContainedType = self.entries.getTypeDeclaration(catalog)
        return f'bool iterate{myName}({myType} table, Index *x, Index *y, {myContainedType} *res, bool *more_x, bool *more_y)'

    def printIteratorDeclaration(self, catalog):
        if self.dimension == 2:
            self.printIterator2DDeclaration(catalog)
        else:
            self.printIterator1DDeclaration(catalog)

    def printIterator1DDeclaration(self, catalog):
        c = self.comment('printIterator1DDeclaration')
        decl = self.getIterator1DDeclaration(catalog)
        print(f'{decl}; {c}')

    def printIterator2DDeclaration(self, catalog):
        c = self.comment('printIterator2DDeclaration')
        decl = self.getIterator2DDeclaration(catalog)
        print(f'{decl}; {c}')

    def printIteratorFunction(self, catalog):
        if self.dimension == 2:
            self.printIterator2DFunction(catalog)
        else:
            self.printIterator1DFunction(catalog)

    def printIterator1DFunction(self, catalog):
        c = self.comment('printIterator1DFunction')
        decl = self.getIterator1DDeclaration(catalog)
        a = '.' if self.isInline(catalog) else '->'
        print(f"/**")
        print(f" * @brief Iterates over the entries in the 1D {self.getName()} object `table`.")
        print(f" *")
        print(f" * The `Index *i` is used to keep state between calls")
        print(f" * and should be initialised to zero before first calling this function.")
        if self.entries is not None:
            entry = self.entries.getTypeDeclaration(catalog)
            print(f" * If `{entry}*value` is not `NULL` then the `{entry}` associated with the key is placed in the pointer.")
        print(f" *")
        print(f" * @return the next key in the hash table, or `NULL` if there are no more keys.")
        print(f" */")
        print(f'{decl} {{ {c}')
        print(f'    if (*i >= table{a}size) {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = (*i + 1 < table{a}size); {c}')
        print(f'        }} {c}')
        print(f'        if (res != NULL) {{ {c}')
        print(f'            *res = table{a}entries[*i]; {c}')
        print(f'        }} {c}')
        print(f'        *i = *i + 1; {c}')
        print(f'        return true; {c}')
        print(f'    }} {c}')
        print(f'}} {c}')
        print('')

    def printIterator2DFunction(self, catalog):
        c = self.comment('printIterator2DFunction')
        decl = self.getIterator2DDeclaration(catalog)
        a = '.' if self.isInline(catalog) else '->'
        print(f"/**")
        print(f" * @brief Iterates over the entries in the 2D {self.getName()} object `table`.")
        print(f" */")
        print(f'{decl} {{ {c}')
        print(f'    if (*x >= table{a}width) {{ {c}')
        print(f'        if (more_x != NULL) {{ {c}')
        print(f'            *more_x = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else if (*y >= table{a}height) {{ {c}')
        print(f'        if (more_y != NULL) {{ {c}')
        print(f'            *more_y = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else {{ {c}')
        print(f'        if (more_x != NULL) {{ {c}')
        print(f'            *more_x = (*x + 1 < table{a}width); {c}')
        print(f'        }} {c}')
        print(f'        if (more_y != NULL) {{ {c}')
        print(f'            *more_y = (*y + 1 < table{a}height); {c}')
        print(f'        }} {c}')
        print(f'        if (res != NULL) {{ {c}')
        print(f'            *res = table{a}entries[*x * table{a}width + *y]; {c}')
        print(f'        }} {c}')
        print(f'        if (*x + 1 == table->width) {{ {c}')
        print(f'            *x = 0; {c}')
        print(f'            *y = *y + 1; {c}')
        print(f'        }} else {{ {c}')
        print(f'            *x = *x + 1; {c}')
        print(f'        }} {c}')
        print(f'        return true; {c}')
        print(f'    }} {c}')
        print(f'}} {c} {c}')
        print('')

    def isArray(self):
        return True

    pass


class SimpleStack(SimpleArray):
    """
    Stacks don't expose a size, instead they have a frame pointer and a stack pointer
    where the stack pointer is an offset from the frame pointer rather than an absolute
    value. They have an additional pushable and popable array of frame pointers and
    stack pointers.
    """
    def __init__(self, name, data):
        super().__init__(name, data)
        if self.dimension != 1:
            raise Exception("stacks must have dimension 1")

    def comment(self, method):
        return CommentGen.method_comment('SimpleStack', method)

    def printIndexFields(self):
        c = self.comment('printIndexFields')
        print(f"    Index frame; {c}")
        print(f"    Index offset; {c}")
        print(f"    Index entries_capacity; {c}")
        print(f"    Index frames_capacity; {c}")
        print(f"    Index frames_index; {c}")

    def printExtraStackEntries(self):
        c = self.comment('printExtraStackEntries')
        print(f"    StackFrame *frames; {c}")

    def printNullEntries(self):
        c = self.comment('printNullEntries')
        print(f"    x->entries = NULL; {c}")
        print(f"    x->frames = NULL; {c}")

    def printZeroCapacities(self):
        c = self.comment('printZeroCapacities')
        print(f"    x->frame = 0; {c}")
        print(f"    x->offset = 0; {c}")
        print(f"    x->entries_capacity = 0; {c}")
        print(f"    x->frames_capacity = 0; {c}")
        print(f"    x->frames_index = 0; {c}")

    def printFreeFunction(self, catalog):
        myName = self.getName()
        decl = decl=self.getFreeSignature(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printFreeFunction')
        print(f"/**")
        print(f" * @brief Frees the {myName} object `x`.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    FREE_ARRAY({entryType}, x->entries, x->entries_capacity); {c}")
        print(f"    FREE_ARRAY(StackFrame, x->frames, x->frames_capacity); {c}")
        print(f"    FREE(x, {myName}); {c}")
        print(f"}} {c}")
        print("")

    def printInitEntries(self, catalog):
        c = self.comment('printInitEntries')
        print(f"    x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, 8); {c}")
        print(f"    x->entries_capacity = 8; {c}")
        print(f"    x->frames = NEW_ARRAY(StackFrame, 8); {c}")
        print(f"    x->frames_capacity = 8; {c}")

    def printExtendDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printExtendDeclaration')
        print(f"void extend{name}Entries({myType} {a}obj, Index size); {c}")
        print(f"void extend{name}Frames({myType} {a}obj, Index size); {c}")

    def printSizeDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printSizeDeclaration')
        print(f"static inline Index totalSize{name}({myType} {a}obj) {{ return obj->frame + obj->offset; }} {c}")
        print(f"static inline Index frameSize{name}({myType} {a}obj) {{ return obj->frames_index; }} {c}")
        print(f"static inline Index offset{name}({myType} {a}obj) {{ return obj->offset; }} {c}")

    def printPushDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPushDeclaration')
        print(f"Index push{name}Entry({myType} {a}x, {entryType} entry); {c}")
        print(f"Index let{name}Frame({myType} {a}x); {c}")
        print(f"void push{name}Frame({myType} {a}x); {c}")

    def printCopyEntriesDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printCopyEntriesDeclaration')
        print(f"void copyCurrent{name}Entries({myType} dest, {myType} src); {c}")
        print(f"void copyAll{name}Entries({myType} dest, {myType} src); {c}")
        print(f"void copy{name}Continuation({myType} dest, {myType} src); {c}")

    def printPopDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopDeclaration')
        print(f"{entryType} pop{name}Entry({myType} {a}x); {c}")
        print(f"void pop{name}Frame({myType} {a}x); {c}")

    def printPopnDeclaration(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopnDeclaration')
        print("#ifdef SAFETY_CHECKS")
        print(f"void popn{name}({myType} {a}obj, int n); {c}")
        print("#else")
        print(f"static inline void popn{name}({myType} {a}obj, int n) {{ obj->offset -= n; }}; {c}")
        print("#endif")

    def printClearDeclaration(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printClearDeclaration')
            print(f"static inline void clear{name}Entries({myType} {a}x) {{ x->offset = 0; }}; {c}")
            print(f"static inline void clear{name}Frames({myType} {a}x) {{ x->frames_index = x->offset = x->frame = 0; }}; {c}")

    def printExtendFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            c = self.comment('printExtendFunction')
            a = '*' if self.isInline(catalog) else ''
            print(f"/**")
            print(f" * Ensures that `x` has at least an absolute entries_capacity of `size`.")
            print(f" */")
            print(f"void extend{name}Entries({myType} {a}x, Index size) {{ {c}")
            print(f'    DEBUG("extend{name}Entries(%p, %u)", x, size);')
            print(f"    if (size > 0) {{ {c}")
            print(f"        size = size < 8 ? 8 : size; {c}")
            print(f"        if (x->entries_capacity == 0) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"            if (x->entries != NULL) {{ {c}")
            print(f'                cant_happen("{name} non-null entries with zero capacity"); {c}')
            print(f"            }} {c}")
            print(f"#endif")
            print(f"            x->entries = NEW_ARRAY({entryType}, size); {c}")
            print(f"            x->entries_capacity = size; {c}")
            print(f"        }} else {{ {c}")
            print(f"            while (size > x->entries_capacity) {{ {c}")
            print(f"                x->entries = GROW_ARRAY({entryType}, x->entries, x->entries_capacity, x->entries_capacity *2); {c}")
            print(f"                x->entries_capacity *= 2; {c}")
            print(f"            }} {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}")
            print(f"")
            print(f"/**")
            print(f" * Ensures that `x` has at least a frames_capacity of `size`.")
            print(f" */")
            print(f"void extend{name}Frames({myType} {a}x, Index size) {{ {c}")
            print(f'    DEBUG("extend{name}Frames(%p, %u)", x, size);')
            print(f"    if (size > 0) {{ {c}")
            print(f"        size = size < 8 ? 8 : size; {c}")
            print(f"        if (x->frames_capacity == 0) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"            if (x->frames != NULL) {{ {c}")
            print(f'                cant_happen("{name} non-null frames with zero capacity"); {c}')
            print(f"            }} {c}")
            print(f"#endif")
            print(f"            x->frames = NEW_ARRAY(StackFrame, size); {c}")
            print(f"            x->frames_capacity = size; {c}")
            print(f"        }} else {{ {c}")
            print(f"            while (size > x->frames_capacity) {{ {c}")
            print(f"                x->frames = GROW_ARRAY(StackFrame, x->frames, x->frames_capacity, x->frames_capacity *2); {c}")
            print(f"                x->frames_capacity *= 2; {c}")
            print(f"            }} {c}")
            print(f"        }} {c}")
            print(f"    }} {c}")
            print(f"}} {c}")
            print(f"")

    def printPushFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        c = self.comment('printPushFunction')
        a = '*' if self.isInline(catalog) else ''
        print(f"/**")
        print(f" * Pushes `entry` on to `x`, extending `x` if required.")
        print(f" * Returns the stack pointer after the push.")
        print(f" */")
        print(f"Index push{name}Entry({myType} {a}x, {entryType} entry) {{ {c}")
        print(f'    DEBUG("push{name}Entry(%p)", x);')
        print(f"    if (x->frame + x->offset + 1 > x->entries_capacity) {{ {c}")
        print(f"        extend{name}Entries(x, x->frame + x->offset + 1); {c}")
        print(f"    }} {c}")
        print(f"    x->entries[x->frame + x->offset] = entry; {c}")
        print(f"    x->offset++; {c}")
        print(f"    return x->offset - 1; {c}")
        print(f"}} {c}")
        print(f"")
        print(f"/**")
        print(f" * Duplicates the top frame.")
        print(f" */")
        print(f"Index let{name}Frame({myType} {a}x) {{ {c}")
        print(f'    DEBUG("let{name}Frame(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x == NULL) {{ {c}")
        print(f'        cant_happen("{name} null stack"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    if (x->frames_index + 1 > x->frames_capacity) {{ {c}")
        print(f"        extend{name}Frames(x, x->frames_index + 1); {c}")
        print(f"    }} {c}")
        print(f"    x->frames[x->frames_index++] = (StackFrame) {{.frame = x->frame, .offset = x->offset }}; {c}")
        print(f"    if (x->frame + x->offset * 2 > x->entries_capacity) {{ {c}")
        print(f"        extend{name}Entries(x, x->frame + x->offset * 2); {c}")
        print(f"    }} {c}")
        print(f"    COPY_ARRAY({entryType}, &x->entries[x->frame + x->offset], &x->entries[x->frame], x->offset); {c}")
        print(f"    x->frame += x->offset; {c}")
        print(f"    return x->offset; {c}")
        print(f"}} {c}")
        print(f"")
        print(f"/**")
        print(f" * Creates new empty top frame.")
        print(f" */")
        print(f"void push{name}Frame({myType} {a}x) {{ {c}")
        print(f'    DEBUG("push{name}Frame(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x == NULL) {{ {c}")
        print(f'        cant_happen("{name} null stack"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    if (x->frames_index + 1 > x->frames_capacity) {{ {c}")
        print(f"        extend{name}Frames(x, x->frames_index + 1); {c}")
        print(f"    }} {c}")
        print(f"    x->frames[x->frames_index++] = (StackFrame) {{.frame = x->frame, .offset = x->offset }}; {c}")
        print(f"    x->frame += x->offset; {c}")
        print(f"    x->offset = 0; {c}")
        print(f"}} {c}")
        print(f"")

    def printPopFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopFunction')
        print(f"/**")
        print(f" * Pops the top entry from `x` and returns it.")
        print(f" */")
        print(f"{entryType} pop{name}Entry({myType} {a}x) {{ {c}")
        print(f'    DEBUG("pop{name}Entry(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x->offset == 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    return x->entries[--(x->offset) + x->frame]; {c}")
        print(f"}} {c}")
        print(f"")
        print(f"/**")
        print(f" * Pops the top frame from `x`.")
        print(f" */")
        print(f"void pop{name}Frame({myType} {a}x) {{ {c}")
        print(f'    DEBUG("pop{name}Frame(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x->frames_index == 0) {{ {c}")
        print(f'        cant_happen("{name} stack frame underflow"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    x->frames_index--; {c}")
        print(f"    x->frame = x->frames[x->frames_index].frame; {c}")
        print(f"    x->offset = x->frames[x->frames_index].offset; {c}")
        print(f"}} {c}")
        print(f"")

    def printPopnFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPopnFunction')
        print(f"/**")
        print(f" * Discards the top `n` entries from `x`.")
        print(f" */")
        print(f"#ifdef SAFETY_CHECKS")
        print(f"void popn{name}({myType} {a}x, int n) {{ {c}")
        print(f'    DEBUG("popn{name}(%p, %d)", x, n);')
        print(f"    if (((int) x->offset) - n < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d/%u", n, x->offset); {c}')
        print(f"    }} {c}")
        print(f"    x->offset -= n; {c}")
        print(f"}} {c}")
        print(f"#endif")
        print("")

    def printMoveFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printMoveFunction')
        print(f"/**")
        print(f" * Safe move `n` entries from `sp - n` to `b`,")
        print(f" * sets sp to `b + n`.")
        print(f" */")
        print(f"void move{name}({myType} {a}x, int b, int n) {{ {c}")
        print(f'    DEBUG("move{name}(%p, %d, %d)", x, b, n);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (((int) x->offset) - n < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d/%u", n, x->offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    if (n > 0) {{ {c}")
        print(f"        extend{name}Entries(x, x->frame + b + n); {c}")
        print(f"        MOVE_ARRAY({entryType}, &x->entries[x->frame + b], &x->entries[x->frame + x->offset - n], n); {c}")
        print(f"    }} {c}")
        print(f"    x->offset = (Index) (b + n); {c}")
        print(f"}} {c}\n")

    def printPushnFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPushnFunction')
        print(f"/**")
        print(f" * Pushes `n` copies of `entry` on to `x`.")
        print(f" */")
        print(f"void pushn{name}({myType} {a}x, int n, {entryType} entry) {{ {c}")
        print(f'    DEBUG("pushn{name}(%p, %d)", x, n);')
        print(f"    if (n > 0) {{ {c}")
        print(f"        extend{name}Entries(x, x->frame + x->offset + n); {c}")
        print(f"        while (n-- > 0) {{ {c}")
        print(f"            x->entries[x->frame + x->offset++] = entry; {c}")
        print(f"        }} {c}")
        print(f"    }} {c}")
        print(f"}} {c}\n")

    def printCopyTopFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printCopyTopFunction')
        print(f"/**")
        print(f" * Copies the top `n` entries from `src` to the base of `dest`,")
        print(f" * sets `dest` offset (sp) to `n`.")
        print(f" */")
        print(f"void copyTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
        print(f'    DEBUG("copyTop{name}(%p, %p, %d)", dest, src, n);')
        print(f"    if (n > 0) {{ {c}")
        print(f"        extend{name}Entries(dest, dest->frame + n); {c}")
        print(f"        COPY_ARRAY({entryType}, &dest->entries[dest->frame], &src->entries[src->frame + src->offset - n], n); {c}")
        print(f"    }} {c}")
        print(f"    dest->offset = n; {c}")
        print(f"}} {c}\n")

    def printCopyExceptTopFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printCopyExceptTopFunction')
        print(f"/**")
        print(f" * Copies all but top `n` entries from `src` to the base of `dest`,")
        print(f" * sets `dest` sp to `src->sp - n`.")
        print(f" */")
        print(f"void copyExceptTop{name}({myType} {a}dest, {myType} {a}src, int n) {{ {c}")
        print(f'    DEBUG("copyExceptTop{name}(%p, %p, %d)", dest, src, n);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (((int) src->offset) - n < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d/%u", n, src->offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    if (((Index) n) < src->offset) {{ {c}")
        print(f"        extend{name}Entries(dest, dest->frame + src->offset - n); {c}")
        print(f"        COPY_ARRAY({entryType}, &dest->entries[dest->frame], &src->entries[src->frame], src->offset - n); {c}")
        print(f"    }} {c}")
        print(f"    dest->offset = src->offset - n; {c}")
        print(f"}} {c}\n")

    def printCopyEntriesFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printCopyEntriesFunction')
        print(f'/**')
        print(f" * Copies the curent frame's entries from `src` to `dest`,")
        print(f' * sets `dest` offset to `src->offset`.')
        print(f' */')
        print(f'void copyCurrent{name}Entries({myType} {a}dest, {myType} {a}src) {{ {c}')
        print(f'    DEBUG("copyCurrent{name}Entries(%p, %p)", dest, src);')
        print(f'    extend{name}Entries(dest, dest->frame + src->offset); {c}')
        print(f'    COPY_ARRAY({entryType}, &dest->entries[dest->frame], &src->entries[src->frame], src->offset); {c}')
        print(f'    dest->offset = src->offset; {c}')
        print(f'}} {c}')
        print(f'')
        print(f'/**')
        print(f' * Copies all entries from `src` to `dest`,')
        print(f' */')
        print(f'void copyAll{name}Entries({myType} {a}dest, {myType} {a}src) {{ {c}')
        print(f'    DEBUG("copyAll{name}Entries(%p, %p)", dest, src);')
        print(f'    extend{name}Entries(dest, src->frame + src->offset); {c}')
        print(f'    extend{name}Frames(dest, src->frames_index); {c}')
        print(f'    COPY_ARRAY({entryType}, dest->entries, src->entries, src->frame + src->offset); {c}')
        print(f'    COPY_ARRAY(StackFrame, dest->frames, src->frames, src->frames_index); {c}')
        print(f'    dest->frames_index = src->frames_index; {c}')
        print(f'    dest->frame = src->frame; {c}')
        print(f'    dest->offset = src->offset; {c}')
        print(f'}} {c}')
        print(f'')
        print(f'/**')
        print(f' * Copies all entries from `src` to `dest`, except the current frame,')
        print(f' */')
        print(f'void copy{name}Continuation({myType} {a}dest, {myType} {a}src) {{ {c}')
        print(f'    DEBUG("copy{name}Continuation(%p, %p)", dest, src);')
        print(f'    if (src->frames_index == 0) {{ {c}')
        print(f'        dest->frames_index = dest->frame = dest->offset = 0; {c}')
        print(f'        return; {c}')
        print(f'    }} {c}')
        print(f'    StackFrame sf = src->frames[src->frames_index - 1]; {c}')
        print(f'    Index newSize = sf.frame + sf.offset; {c}')
        print(f'    extend{name}Entries(dest, newSize); {c}')
        print(f'    extend{name}Frames(dest, src->frames_index); {c}')
        print(f'    COPY_ARRAY({entryType}, dest->entries, src->entries, newSize); {c}')
        print(f'    COPY_ARRAY(StackFrame, dest->frames, src->frames, src->frames_index - 1); {c}')
        print(f'    dest->frames_index = src->frames_index - 1; {c}')
        print(f'    dest->frame = sf.frame; {c}')
        print(f'    dest->offset = sf.offset; {c}')
        print(f'}} {c}')
        print(f'')

    def printPeekFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPeekFunction')
        print(f"/**")
        print(f" * Returns the value at the top of `x`.")
        print(f" */")
        print(f"{entryType} peek{name}({myType} {a}x) {{ {c}")
        print(f'    DEBUG("peek{name}(%p)", x);')
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (x->offset == 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow"); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    return x->entries[x->frame + x->offset - 1]; {c}")
        print(f"}} {c}\n")

    def printPeeknFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPeeknFunction')
        print(f"/**")
        print(f" * If `n` is negative, returns the value at `sp - |n|`,")
        print(f" * otherwise returns the value at `n`.")
        print(f" */")
        print(f"{entryType} peekn{name}({myType} {a}x, int offset) {{ {c}")
        print(f'    DEBUG("peekn{name}(%p, %d)", x, offset);')
        print(f"    if (offset < 0) offset = ((int) x->offset) + offset; {c}")
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (offset >= (int) x->offset) {{ {c}")
        print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->offset); {c}')
        print(f"    }} {c}")
        print(f"    if (offset < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    return x->entries[x->frame + offset]; {c}")
        print(f"}} {c}\n")

    def printPokeFunction(self, catalog):
        name = self.getName()
        myType = self.getTypeDeclaration(catalog)
        entryType = self.entries.getTypeDeclaration(catalog)
        a = '*' if self.isInline(catalog) else ''
        c = self.comment('printPokeFunction')
        print(f"/**")
        print(f" * If `n` is negative, replaces the value at `sp - |n|`,")
        print(f" * otherwise replaces the value at `n`.")
        print(f" */")
        print(f"void poke{name}({myType} {a}x, int offset, {entryType} val) {{ {c}")
        print(f'    DEBUG("poke{name}(%p, %d)", x, offset);')
        print(f"    if (offset < 0) offset = ((int) x->offset) + offset; {c}")
        print(f"#ifdef SAFETY_CHECKS")
        print(f"    if (offset >= (int) x->offset) {{ {c}")
        print(f'        cant_happen("{name} stack overflow %d/%u", offset, x->offset); {c}')
        print(f"    }} {c}")
        print(f"    if (offset < 0) {{ {c}")
        print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
        print(f"    }} {c}")
        print(f"#endif")
        print(f"    x->entries[x->frame + offset] = val; {c}")
        print(f"}} {c}\n")

    def printMark1dFunctionBody(self, catalog):
        c = self.comment('print1dFunctionBody')
        print(f'    DEBUG("markStack(%p, %d + %d)", x, x->frame, x->offset); {c}')
        print(f"    for (Index i = 0; i < x->frame + x->offset; i++) {{ {c}")
        self.entries.printMarkArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printCountDeclaration')
        print(f'static inline Index count{myName}Frame({myType} x) {{ {c}')
        print(f'    return x->offset; {c}')
        print(f'}} {c}')
        print('')
        print(f'static inline Index count{myName}Entries({myType} x) {{ {c}')
        print(f'    return x->frame + x->offset; {c}')
        print(f'}} {c}')
        print('')
        print(f'static inline Index count{myName}Frames({myType} x) {{ {c}')
        print(f'    return x->frames_index; {c}')
        print(f'}} {c}')
        print('')

    def printCompareFunction(self, catalog):
        c = self.comment('printCompareFunction')
        decl = self.getCompareSignature(catalog)
        if self.bespokeCmpImplementation:
            print(f"// Bespoke implementation required for {decl}")
            print("")
            return
        myName = self.getName()
        print(f"/**")
        print(f" * Compares two {myName} objects for deep equality.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    if (a == b) return true; {c}")
        print(f"    if (a == NULL || b == NULL) return false; {c}")
        print(f"    if (a->frame != b->frame) return false; {c}")
        print(f"    if (a->offset != b->offset) return false; {c}")
        print(f"    if (a->frames_index != b->frames_index) return false; {c}")
        print(f"    for (Index i = 0; i < a->frames_index; i++) {{ {c}")
        print(f"        if (a->frames[i].frame != b->frames[i].frame) return false; {c}")
        print(f"        if (a->frames[i].offset != b->frames[i].offset) return false; {c}")
        print(f"    }} {c}")
        print(f"    for (Index i = 0; i < a->frame + a->offset; i++) {{ {c}")
        self.entries.printCompareArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")
        print(f"    return true; {c}")
        print(f"}} {c}")
        print("")

    def print1dCopyFunctionBody(self, catalog):
        c = self.comment('print1dCopyFunctionBody')
        entryType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        print(f"    if (o->entries != NULL) {{ {c}")
        print(f"        extend{myName}Entries(x, o->frame + o->offset); {c}")
        if self.entries.isInline(catalog):
            print(f"        COPY_ARRAY({entryType}, x->entries, o->entries, o->frame + o->offset); {c}")
        else:
            print(f"        for (Index i = 0; i < o->frame + o->offset; i++) {{ {c}")
            self.entries.printCopyArrayLine(catalog, "i", 3)
            print(f"        }} {c}")
        print(f"        x->frame = o->frame; {c}")
        print(f"        x->offset = o->offset; {c}")
        print(f"    }} {c}")
        print(f"    if (o->frames != NULL) {{ {c}")
        print(f"        extend{myName}Frames(x, o->frames_index); {c}")
        print(f"        COPY_ARRAY(StackFrame, x->frames, o->frames, o->frames_index); {c}")
        print(f"        x->frames_index = o->frames_index; {c}")
        print(f"    }} {c}")

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
        if self.tagged:
            print(f'    eprintf("<<%s>>", x->_tag); {c}')
        print(f'    eprintf("{myName}(%d)[\\n", x->frames_index); {c}')
        print(f"    for (Index i = 0; i < x->frames_index; i++) {{ {c}")
        print(f"        for (Index j = 0; j < x->frames[i].offset; j++) {{ {c}")
        print(f"            Index k = x->frames[i].frame + j; {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, "k", 3)
        print(f'            eprintf("\\n"); {c}')
        print(f"        }} {c}")
        print(f"        pad(depth + 1); {c}")
        print(f'        eprintf("---\\n"); {c}')
        print(f"    }} {c}")
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}")
        print("")

    def printIterator1DFunction(self, catalog):
        c = self.comment('printIterator1DFunction')
        decl = self.getIterator1DDeclaration(catalog)
        print(f"/**")
        print(f" * Iterates over the entries in a {self.getName()} object.")
        print(f" * The pointer to an Index `i` is used to track the current position in the array")
        print(f" * and should be initialised to zero before first calling this function.")
        print(f" * If `res` is not NULL, it will be set to the next entry.")
        print(f" * If `more` is not NULL, it will be set to true if there are more entries to iterate over.")
        print(f" */")
        print(f'{decl} {{ {c}')
        print(f'    if (*i >= table->offset) {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else {{ {c}')
        print(f'        if (more != NULL) {{ {c}')
        print(f'            *more = (*i + 1 < table->offset); {c}')
        print(f'        }} {c}')
        print(f'        if (res != NULL) {{ {c}')
        print(f'            *res = table->entries[table->frame + *i]; {c}')
        print(f'        }} {c}')
        print(f'        *i = *i + 1; {c}')
        print(f'        return true; {c}')
        print(f'    }} {c}')
        print(f'}} {c}')
        print('')

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

    def comment(self, method):
        return CommentGen.method_comment('InlineArray', method)

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
        print(f'void init{myName}({typeName} *x, Index size); {c}')

    def printInitFunction(self, catalog):
        if self.dimension != 1:
            return
        typeName = self.getTypeDeclaration(catalog)
        myName = self.getName()
        c = self.comment('printInitFunction')
        print(f"/**")
        print(f" * Initializes an inline (not directly memory-managed) {myName} with the given size.")
        print(" */")
        print(f'void init{myName}({typeName} *x, Index size) {{ {c}')
        print(f"    x->size = 0; {c}")
        print(f"    x->capacity = 0; {c}")
        print(f"    x->entries = NULL; {c}")
        print(f"    if (size > 0) {{ {c}")
        print(f"        x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, size); {c}")
        print(f"        x->capacity = size; {c}")
        print(f'    }} {c}')
        print(f'}} {c}')
        print("")

    def printMarkObjCase(self, catalog):
        pass
