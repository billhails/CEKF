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
from .compare_helper import CompareHelper
from .objtype_helper import ObjectTypeHelper


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
        return '_x'

    def getDefineArg(self):
        return '_x'

    def tag(self):
        super().tag()
        self.tagField = SimpleField(self.name, "_tag", "string")

    def getTypeDeclaration(self, catalog):
        return TypeHelper.struct_type(self.getName(), self.isInline(catalog))

    def printCompareField(self, catalog, isInline, field, depth, prefix=''):
        myName=self.getName()
        extraCmpArgs = self.getExtraCmpAargs(catalog)
        a = AccessorHelper.accessor(isInline)
        c = self.comment('printCompareField')
        pad(depth)
        print(f"if (!eq{myName}(a{a}{prefix}{field}, b{a}{prefix}{field}{extraCmpArgs})) return false; {c}")

    def printCopyField(self, isInline, field, depth, prefix=''):
        myName=self.getName()
        c = self.comment('printCopyField')
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f'_x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        pad(depth)
        myName=self.getName()
        print(f'print{myName}(*({myName} **)ptr, depth + 1); {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        myName=self.getName()
        a = AccessorHelper.accessor(isInline)
        pad(depth)
        print(f'print{myName}(_x{a}{prefix}{field}, depth + 1); {c}')

    def printAccessDeclarations(self, catalog):
        entryType = self.entries.getTypeDeclaration(catalog)
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printAccessDeclarations')
        if self.dimension == 2:
            print(f"static inline {entryType} get{myName}Index({myType} obj, Index _x, Index _y) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (_x >= obj->width || _y >= obj->height) {{ {c}")
            print(f'        cant_happen("{myName} 2d matrix bounds exceeded"); {c}')
            print(f"    }}")
            print(f"#endif")
            print(f"    return obj->entries[_x + _y * obj->width]; {c}")
            print(f"}} {c}")
            print("")
            print(f"static inline void set{myName}Index({myType} obj, Index _x, Index _y, {entryType} val) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (_x >= obj->width || _y >= obj->height) {{ {c}")
            print(f'        cant_happen("{myName} 2d matrix bounds exceeded"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    obj->entries[_x + _y * obj->width] = val; {c}")
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
        return SignatureHelper.mark_signature(self.getName(), myType)

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
            print(f"    FREE_ARRAY({entryType}, _x->entries, _x->capacity); {c}")
        else:
            print(f"    FREE_ARRAY({entryType}, _x->entries, _x->width * _x->height); {c}")
        print(f"    FREE(_x, {myName}); {c}")
        print(f"}} {c}")
        print("")

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return SignatureHelper.free_signature(self.getName(), myType)

    def getObjType(self):
        return ObjectTypeHelper.obj_type_name(self.getName())

    def objTypeArray(self):
        return ObjectTypeHelper.obj_type_array(self.getName())

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
        return SignatureHelper.new_signature(self.getName(), myType, args)

    def getCopySignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return SignatureHelper.copy_signature(myName, myType)

    def printNullEntries(self):
        c = self.comment('printNullEntries')
        print(f"    _x->entries = NULL; {c}")

    def printZeroCapacities(self):
        c = self.comment('printZeroCapacities')
        print(f"    _x->size = 0; {c}")
        print(f"    _x->capacity = 0; {c}")

    def printInitEntries(self, catalog):
        c = self.comment('printInitEntries')
        print(f"    _x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, 8); {c}")
        print(f"    _x->capacity = 8; {c}")

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
        print(f"    {myType} _x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("new {myName} %p", _x); {c}')
        if self.tagged:
            print(f"    _x->_tag = _tag; {c}")
        self.printNullEntries()
        if self.dimension == 1:
            self.printZeroCapacities()
            print(f"    int save = PROTECT(_x); {c}")
            self.printInitEntries(catalog)
        else:
            print(f"    _x->width = 0; {c}")
            print(f"    _x->height = 0; {c}")
            print(f"    int save = PROTECT(_x); {c}")
            print(f"    if (width * height > 0) {{ {c}")
            print(f"        _x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, width * height); {c}")
            print(f"        bzero(_x->entries, sizeof({self.entries.getTypeDeclaration(catalog)}) * width * height); {c}")
            print(f"    }} {c}")
            print(f"    _x->width = width; {c}")
            print(f"    _x->height = height; {c}")
        print(f"    UNPROTECT(save); {c}")
        print(f"    return _x; {c}")
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
            print(f" * Ensures that the {myType} `_x` has at least a capacity of `size`.")
            print(f" */")
            print(f"void extend{name}({myType} {a}_x, Index size) {{ {c}")
            print(f'    DEBUG("extend{name}(%p, %u)", _x, size);')
            print(f"    if (size > 0) {{ {c}")
            print(f"        size = size < 8 ? 8 : size; {c}")
            print(f"        if (_x->capacity == 0) {{ {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"            if (_x->entries != NULL) {{ {c}")
            print(f'                cant_happen("{name} non-null entries with zero capacity"); {c}')
            print(f"            }} {c}")
            print(f"#endif")
            print(f"            _x->entries = NEW_ARRAY({entryType}, size); {c}")
            print(f"            _x->capacity = size; {c}")
            print(f"        }} else {{ {c}")
            print(f"            while (size > _x->capacity) {{ {c}")
            print(f"                _x->entries = GROW_ARRAY({entryType}, _x->entries, _x->capacity, _x->capacity *2); {c}")
            print(f"                _x->capacity *= 2; {c}")
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
            print(f" * Pushes `entry` on to `_x`, extending `_x` if required.")
            print(f" * Returns the stack pointer after the push.")
            print(f" */")
            print(f"Index push{name}({myType} {a}_x, {entryType} entry) {{ {c}")
            print(f'    DEBUG("push{name}(%p)", _x);')
            print(f"    if (_x->size + 1 > _x->capacity) {{ {c}")
            print(f"        extend{name}(_x, _x->size + 1); {c}")
            print(f"    }} {c}")
            print(f"    _x->entries[_x->size++] = entry; {c}")
            print(f"    return _x->size - 1; {c}")
            print(f"}} {c}\n")

    def printPopFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopFunction')
            print(f"/**")
            print(f" * Pops the top entry from `_x` and returns it.")
            print(f" */")
            print(f"{entryType} pop{name}({myType} {a}_x) {{ {c}")
            print(f'    DEBUG("pop{name}(%p)", _x);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (_x->size == 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return _x->entries[--(_x->size)]; {c}")
            print(f"}} {c}\n")

    def printPopnFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPopnFunction')
            print(f"/**")
            print(f" * Discards the top `n` entries from `_x`.")
            print(f" */")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"void popn{name}({myType} {a}_x, int n) {{ {c}")
            print(f'    DEBUG("popn{name}(%p, %d)", _x, n);')
            print(f"    if (((int) _x->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, _x->size); {c}')
            print(f"    }} {c}")
            print(f"    _x->size -= n; {c}")
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
            print(f"void move{name}({myType} {a}_x, int b, int n) {{ {c}")
            print(f'    DEBUG("move{name}(%p, %d, %d)", _x, b, n);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (((int) _x->size) - n < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d/%u", n, _x->size); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(_x, b + n); {c}")
            print(f"        MOVE_ARRAY({entryType}, &_x->entries[b], &_x->entries[_x->size - n], n); {c}")
            print(f"    }} {c}")
            print(f"    _x->size = (Index) (b + n); {c}")
            print(f"}} {c}\n")

    def printPushnFunction(self, catalog):
        if self.dimension == 1:
            name = self.getName()
            myType = self.getTypeDeclaration(catalog)
            entryType = self.entries.getTypeDeclaration(catalog)
            a = '*' if self.isInline(catalog) else ''
            c = self.comment('printPushnFunction')
            print(f"/**")
            print(f" * Pushes `n` copies of `entry` on to `_x`.")
            print(f" */")
            print(f"void pushn{name}({myType} {a}_x, int n, {entryType} entry) {{ {c}")
            print(f'    DEBUG("pushn{name}(%p, %d)", _x, n);')
            print(f"    if (n > 0) {{ {c}")
            print(f"        extend{name}(_x, _x->size + n); {c}")
            print(f"        while (n-- > 0) {{ {c}")
            print(f"            _x->entries[_x->size++] = entry; {c}")
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
            print(f" * Returns the value at the top of `_x`.")
            print(f" */")
            print(f"{entryType} peek{name}({myType} {a}_x) {{ {c}")
            print(f'    DEBUG("peek{name}(%p)", _x);')
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (_x->size == 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow"); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return _x->entries[_x->size - 1]; {c}")
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
            print(f"{entryType} peekn{name}({myType} {a}_x, int offset) {{ {c}")
            print(f'    DEBUG("peekn{name}(%p, %d)", _x, offset);')
            print(f"    if (offset < 0) offset = ((int) _x->size) + offset; {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (offset >= (int) _x->size) {{ {c}")
            print(f'        cant_happen("{name} stack overflow %d/%u", offset, _x->size); {c}')
            print(f"    }} {c}")
            print(f"    if (offset < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    return _x->entries[offset]; {c}")
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
            print(f"void poke{name}({myType} {a}_x, int offset, {entryType} val) {{ {c}")
            print(f'    DEBUG("poke{name}(%p, %d)", _x, offset);')
            print(f"    if (offset < 0) offset = ((int) _x->size) + offset; {c}")
            print(f"#ifdef SAFETY_CHECKS")
            print(f"    if (offset >= (int) _x->size) {{ {c}")
            print(f'        cant_happen("{name} stack overflow %d/%u", offset, _x->size); {c}')
            print(f"    }} {c}")
            print(f"    if (offset < 0) {{ {c}")
            print(f'        cant_happen("{name} stack underflow %d", offset); {c}')
            print(f"    }} {c}")
            print(f"#endif")
            print(f"    _x->entries[offset] = val; {c}")
            print(f"}} {c}\n")

    def printMarkFunction(self, catalog):
        decl = self.getMarkSignature(catalog)
        c = self.comment('printMarkFunction')
        print(f"/**")
        print(f" * @brief Mark a {self.getName()} to protect it from garbage collection.")
        print(f" *")
        print(f" * This function recursively marks the {self.getName()} structure `_x` to protect it from garbage collection.")
        print(f" */")
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (_x == NULL) return; {c}")
            print(f"    if (MARKED(_x)) return; {c}")
            print(f"    MARK(_x); {c}")
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
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"    for (Index i = 0; i < _x{a}size; i++) {{ {c}")
        self.entries.printMarkArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f"    }} {c}")

    def printMark2dFunctionBody(self, catalog):
        c = self.comment('print2dFunctionBody')
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"    Index size = _x{a}width * _x{a}height; {c}")
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
        return SignatureHelper.print_signature(self.getName(), myType)

    def getCtype(self, astType, catalog):
        return TypeHelper.pointer_type(astType)

    def printCountDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        c = self.comment('printCountDeclaration')
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f'static inline Index count{myName}({myType} _x) {{ {c}')
        if self.dimension == 1:
            print(f'    return _x{a}size; {c}')
        else:
            print(f'    return _x{a}width * _x{a}height; {c}')
        print(f'}} {c}')
        print('')

    def getExtraCmpFargs(self, catalog):
        return CompareHelper.get_extra_formal_args(self.extraCmpArgs, lambda t: self.getCtype(t, catalog))

    def getExtraCmpAargs(self, catalog):
        return CompareHelper.get_extra_actual_args(self.extraCmpArgs)

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return SignatureHelper.compare_signature(myName, myType, extraCmpArgs)

    def printCompareFunction(self, catalog):
        c = self.comment('printCompareFunction')
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for")
            print("// {decl}".format(decl=self.getCompareSignature(catalog)))
            print("")
            return
        myName = self.getName()
        decl = self.getCompareSignature(catalog)
        a = AccessorHelper.accessor(self.isInline(catalog))
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
        print(f"    {myType} _x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", _x); {c}')
        print(f"    Header _h = _x->header; {c}")
        print(f"    bzero(_x, sizeof(struct {myName})); {c}")
        print(f"    _x->header = _h; {c}")
        print(f"    int save = PROTECT(_x); {c}")
        self.printCopyFunctionBody(catalog)
        print(f"    UNPROTECT(save); {c}")
        print(f"    return _x; {c}")
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
        print(f"        extend{myName}(_x, o->size); {c}")
        if self.entries.isInline(catalog):
            print(f"        COPY_ARRAY({entryType}, _x->entries, o->entries, o->size); {c}")
            print(f"        _x->size = o->size; {c}")
        else:
            print(f"        _x->size = 0; {c}")
            print(f"        for (Index i = 0; i < o->size; i++) {{ {c}")
            self.entries.printCopyArrayLine(catalog, "i", 3)
            print(f"            _x->size++; {c}")
            print(f"        }} {c}")
        print(f"    }} else {{ {c}")
        print(f"        _x->size = 0; {c}")
        print(f"    }} {c}")

    def print2dCopyFunctionBody(self, catalog):
        c = self.comment('print2dCopyFunctionBody')
        print(f"    if (o->entries != NULL) {{ {c}")
        print(f"        _x->entries = NEW_ARRAY({self.entries.getTypeDeclaration(catalog)}, _x->width * _x->height); {c}")
        print(f"        _x->width = 0; {c}")
        print(f"        _x->height = 0; {c}")
        print(f"        for (Index i = 0; i < (o->width * o->height); i++) {{ {c}")
        self.entries.printCopyArrayLine(catalog, "i", 3)
        print(f"        }} {c}")
        print(f"        _x->height = o->height; {c}")
        print(f"        _x->width = o->width; {c}")
        print(f"    }} {c}")

    def printPrintFunction(self, catalog):
        myName = self.getName()
        decl = self.getPrintSignature(catalog)
        a = AccessorHelper.accessor(self.isInline(catalog))
        c = self.comment('printPrintFunction')
        print(f"/**")
        print(f" * @brief Prints the {myName} object `_x` for debugging.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        if not self.isInline(catalog):
            print(f'    if (_x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        if self.tagged:
            print(f'    eprintf("<<%s>>", _x{a}_tag); {c}')
        if self.dimension == 1:
            print(f'    eprintf("{myName}(%d)[\\n", _x{a}size); {c}')
        else:
            print(f'    eprintf("{myName}(%d * %d)[\\n", _x{a}width, _x{a}height); {c}')
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
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"    for (Index i = 0; i < _x{a}size; i++) {{ {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, "i", 2)
        print(f'        eprintf("\\n"); {c}')
        print(f"    }} {c}")

    def print2dPrintFunctionBody(self, catalog):
        c = self.comment('print2dPrintFunctionBody')
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"    for (Index i = 0; i < _x{a}height; i++) {{ {c}")
        print(f"        pad(depth); {c}")
        print(f'        eprintf("[\\n"); {c}')
        print(f"        for (Index j = 0; j < _x{a}width; j++) {{ {c}")
        self.entries.printPrintArrayLine(self.isInline(catalog), catalog, f"i * _x{a}width + j", 3)
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
        a = AccessorHelper.accessor(isInline)
        print(f"mark{myName}(_x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f"return PROTECT(_x{a}{prefix}{field}); {c}")

    def generateVisitorDecl(self):
        """Generate forward declaration for visitor function"""
        myName = self.getName()
        return f"static {myName} *visit{myName}({myName} *node, VisitorContext *context);\n"

    def generateVisitor(self, catalog):
        """Generate stub visitor function - TODO: implement array traversal"""
        myName = self.getName()
        output = []
        output.append(f"__attribute__((unused))\n")
        output.append(f"static {myName} *visit{myName}({myName} *node, VisitorContext *context) {{\n")
        output.append(f"    (void)context;  // TODO: implement array visitor\n")
        output.append(f"    return node;  // TODO: traverse and rebuild array if elements change\n")
        output.append(f"}}\n\n")
        return ''.join(output)

    def getIterator1DDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        myContainedType = self.entries.getTypeDeclaration(catalog)
        return f'bool iterate{myName}({myType} table, Index *i, {myContainedType} *res, bool *more)'

    def getIterator2DDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        myContainedType = self.entries.getTypeDeclaration(catalog)
        return f'bool iterate{myName}({myType} table, Index *_x, Index *_y, {myContainedType} *res, bool *more_x, bool *more_y)'

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
        a = AccessorHelper.accessor(self.isInline(catalog))
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
        a = AccessorHelper.accessor(self.isInline(catalog))
        print(f"/**")
        print(f" * @brief Iterates over the entries in the 2D {self.getName()} object `table`.")
        print(f" */")
        print(f'{decl} {{ {c}')
        print(f'    if (*_x >= table{a}width) {{ {c}')
        print(f'        if (more_x != NULL) {{ {c}')
        print(f'            *more_x = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else if (*_y >= table{a}height) {{ {c}')
        print(f'        if (more_y != NULL) {{ {c}')
        print(f'            *more_y = false; {c}')
        print(f'        }} {c}')
        print(f'        return false; {c}')
        print(f'    }} else {{ {c}')
        print(f'        if (more_x != NULL) {{ {c}')
        print(f'            *more_x = (*_x + 1 < table{a}width); {c}')
        print(f'        }} {c}')
        print(f'        if (more_y != NULL) {{ {c}')
        print(f'            *more_y = (*_y + 1 < table{a}height); {c}')
        print(f'        }} {c}')
        print(f'        if (res != NULL) {{ {c}')
        print(f'            *res = table{a}entries[*_x * table{a}width + *_y]; {c}')
        print(f'        }} {c}')
        print(f'        if (*_x + 1 == table->width) {{ {c}')
        print(f'            *_x = 0; {c}')
        print(f'            *_y = *_y + 1; {c}')
        print(f'        }} else {{ {c}')
        print(f'            *_x = *_x + 1; {c}')
        print(f'        }} {c}')
        print(f'        return true; {c}')
        print(f'    }} {c}')
        print(f'}} {c} {c}')
        print('')

    def isArray(self):
        return True

    pass


