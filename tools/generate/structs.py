"""
Struct structure classes for makeAST code generation.

This module contains:
- SimpleStruct: Regular C structs with fields
"""

from .base import Base, EnumField
from .fields import SimpleField
from .utils import pad
from .comment_gen import CommentGen
from .type_helper import TypeHelper
from .signature_helper import SignatureHelper
from .accessor_helper import AccessorHelper
from .compare_helper import CompareHelper
from .objtype_helper import ObjectTypeHelper

class SimpleStruct(Base):
    """
    Simple structs declared directly in the yaml
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.fields = [self.makeField(x, data[x]) for x in data.keys()]
        else:
            raise ValueError(f"SimpleStruct {name} must have 'data' field")

    def hasParserInfo(self, catalog):
        return catalog.parserInfo

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        if catalog.parserInfo:
            print(f"    ParserInfo _yy_parser_info; {c}")
        for field in self.fields:
            field.printStructTypedefLine(catalog)
        print(f"}} {name}; {c}\n")

    def isStruct(self):
        return True

    def printMermaid(self, catalog):
        for field in self.fields:
            print(f"{self.getName()} --{field.getName()}--> {field.getObjName(catalog)}")

    def makeField(self, fieldName, fieldType):
        return SimpleField(self.name, fieldName, fieldType)

    def getTypeDeclaration(self, catalog):
        return TypeHelper.struct_type(self.getName(), self.isInline(catalog))

    def getObjType(self):
        return ObjectTypeHelper.obj_type_name(self.getName())

    def isSinglySelfReferential(self, catalog):
        count = 0
        for field in self.fields:
            if field.isSimpleField() and field.getObjName(catalog) == self.getName():
                count += 1
        return count == 1

    def getSelfReferentialField(self, catalog):
        for field in self.fields:
            if field.isSimpleField() and field.getObjName(catalog) == self.getName():
                return field.getName()
        raise Exception(f'cannot find self-referential field name for {self.getName()}')

    def objTypeArray(self):
        return ObjectTypeHelper.obj_type_array(self.getName())

    def getCountSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f'Index count{myName}({myType} x)'

    def comment(self, method):
        return CommentGen.method_comment('SimpleStruct', method)

    def printCountDeclaration(self, catalog):
        if self.isSinglySelfReferential(catalog):
            c = self.comment('printCountDeclaration')
            print(f'{self.getCountSignature(catalog)}; {c}')

    def printCountFunction(self, catalog):
        if self.isSinglySelfReferential(catalog):
            c = self.comment('printCountFunction')
            selfRefField = self.getSelfReferentialField(catalog)
            print(f'/**')
            print(f' * Counts the number of entries in the {self.getName()} linked list,')
            print(f' * by following the self-referential field `{selfRefField}`.')
            print(f' */')
            print(f'{self.getCountSignature(catalog)} {{ {c}')
            print(f'    Index count = 0; {c}')
            print(f'    while (x != NULL) {{ {c}')
            print(f'        x = x->{selfRefField}; {c}')
            print(f'        count++; {c}')
            print(f'    }} {c}')
            print(f'    return count; {c}')
            print(f'}} {c}')
            print('')

    def getMarkSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return SignatureHelper.mark_signature(self.getName(), myType)

    def getFreeSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return SignatureHelper.free_signature(self.getName(), myType)

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        return SignatureHelper.print_signature(self.getName(), myType)

    def getCtype(self, astType, catalog):
        return f"{astType} *"

    def getExtraCmpFargs(self, catalog):
        return CompareHelper.get_extra_formal_args(self.extraCmpArgs, lambda t: self.getCtype(t, catalog))

    def getExtraCmpAargs(self, catalog):
        return CompareHelper.get_extra_actual_args(self.extraCmpArgs)

    def getCompareSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        extraCmpArgs = self.getExtraCmpFargs(catalog)
        return SignatureHelper.compare_signature(myName, myType, extraCmpArgs)

    def getNewArgs(self, catalog):
        return [x for x in self.fields if x.default is None and not x.isSelfInitializing(catalog)]

    def getDefaultArgs(self, catalog):
        return [x for x in self.fields if x.default is not None or x.isSelfInitializing(catalog)]

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName=self.getName()
        args = []
        for field in self.getNewArgs(catalog):
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        if catalog.parserInfo:
            args = ['ParserInfo _PI'] + args

        return SignatureHelper.new_signature(myName, myType, args)

    def getCopySignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return SignatureHelper.copy_signature(myName, myType)

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl = self.getNewSignature(catalog)
        print(f"{decl}; {c}")

    def printCopyDeclaration(self, catalog):
        c = self.comment('printCopyDeclaration')
        decl=self.getCopySignature(catalog)
        print(f"{decl}; {c}")

    def printFreeDeclaration(self, catalog):
        c = self.comment('printFreeDeclaration')
        decl=self.getFreeSignature(catalog)
        print(f"{decl}; {c}")

    def printMarkDeclaration(self, catalog):
        c = self.comment('printMarkDeclaration')
        decl=self.getMarkSignature(catalog)
        print(f"{decl}; {c}")

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl=self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printCompareDeclaration(self, catalog):
        c = self.comment('printCompareDeclaration')
        decl=self.getCompareSignature(catalog)
        print(f"{decl}; {c}")

    def printNewFunction(self, catalog):
        c = self.comment('printNewFunction')
        decl = self.getNewSignature(catalog)
        print("/**")
        print(f" * Creates a new {self.getName()} struct with the given arguments.")
        print(" */")
        print(f"{decl} {{ {c}")
        hasInternalConstructors = False
        for field in self.getDefaultArgs(catalog):
            if field.isSelfInitializing(catalog) and field.default is None:
                hasInternalConstructors = True
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        if hasInternalConstructors:
            print(f"    Header _h = x->header; {c}")
            print(f"    bzero(x, sizeof(struct {myName})); {c}")
            print(f"    x->header = _h; {c}")
            print(f"    int save = PROTECT(x); {c}")
        print(f'    DEBUG("new {myName} %p", x); {c}')
        if catalog.parserInfo:
            print(f"    x->_yy_parser_info = _PI; {c}")
        for field in self.getNewArgs(catalog):
            f = field.getFieldName()
            print(f"    x->{f} = {f}; {c}")
        for field in self.getDefaultArgs(catalog):
            f = field.getFieldName()
            if field.isSelfInitializing(catalog) and field.default is None:
                d = f'{field.getConstructorName(catalog)}()'
            else:
                d = field.default
            print(f"    x->{f} = {d}; {c}")
        if hasInternalConstructors:
            print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printMarkFunctionBody(self, catalog):
        for field in self.fields:
            field.printMarkLine(self.isInline(catalog), catalog, 1)

    def printCompareFunctionBody(self, catalog):
        for field in self.fields:
            field.printCompareLine(self.isInline(catalog), catalog, 1)

    def printCopyFunctionBody(self, catalog):
        for field in self.fields:
            field.printCopyLine(self.isInline(catalog), catalog, 1)

    def printPrintFunctionBody(self, catalog):
        c = self.comment('printPrintFunctionBody')
        for field in self.fields:
            field.printPrintLine(self.isInline(catalog), catalog, 1)
            print(f'    eprintf("\\n"); {c}')

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
        print(f"mark{myName}(x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        myName=self.getName()
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f"return PROTECT(x{a}{prefix}{field}); {c}")

    def printCompareField(self, catalog, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        myName=self.getName()
        extraArgs = self.getExtraCmpAargs({})
        a = AccessorHelper.accessor(isInline)
        pad(depth)
        print(f"if (!eq{myName}(a{a}{prefix}{field}, b{a}{prefix}{field}{extraArgs})) return false; {c}")

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        myName=self.getName()
        pad(depth)
        print(f'print{myName}(*({myName} **)ptr, depth + 1); {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        myName=self.getName()
        a = AccessorHelper.accessor(isInline)
        pad(depth)
        print(f'print{myName}(x{a}{prefix}{field}, depth + 1); {c}')

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        myName=self.getName()
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f'x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def printMarkFunction(self, catalog):
        c = self.comment('printMarkFunction')
        decl = self.getMarkSignature(catalog)
        print(f"/**")
        print(f" * Marks the {self.getName()} object to protect it from garbage collection.")
        print(f" * It will recursively mark all the fields of the object.")
        print(f" */")
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (x == NULL) return; {c}")
            print(f"    if (MARKED(x)) return; {c}")
            print(f"    MARK(x); {c}")
        self.printMarkFunctionBody(catalog)
        print(f"}} {c}\n")

    def printFreeFunction(self, catalog):
        c = self.comment('printFreeFunction')
        decl = self.getFreeSignature(catalog)
        print(f"/**")
        print(f" * Frees the {self.getName()} object.")
        print(f" * It will only free the object itself,")
        print(f" * not the fields of the object which")
        print(f" * should only be freed by the sweep phase directly.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    FREE(x, {self.getName()}); {c}")
        print(f"}} {c}\n")

    def printMarkObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printMarkObjCase')
        name=self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'mark{name}(({name} *)h); {c}')
        pad(3)
        print(f'break; {c}')

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

    def printTypeObjCase(self, catalog):
        if self.isInline(catalog):
            return
        c = self.comment('printTypeObjCase')
        name=self.getName()
        pad(2)
        print(f'case {self.getObjType()}: {c}')
        pad(3)
        print(f'return "{name}"; {c}')

    def printCompareFunction(self, catalog):
        if self.bespokeCmpImplementation:
            print("// Bespoke implementation required for")
            print("// {decl}".format(decl=self.getCompareSignature(catalog)))
            print("")
            return
        myName = self.getName()
        c = self.comment('printCompareFunction')
        decl=self.getCompareSignature(catalog)
        print(f"/**")
        print(f" * Compares two {myName} objects for equality.")
        print(f" * It will recursively compare all the fields of the object.")
        print(f" */")
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (a == b) return true; {c}")
            print(f"    if (a == NULL || b == NULL) return false; {c}")
        self.printCompareFunctionBody(catalog)
        print(f"    return true; {c}")
        print(f"}} {c}\n")

    def printCopyFunction(self, catalog):
        c = self.comment('printCopyFunction')
        decl = self.getCopySignature(catalog)
        myType = self.getTypeDeclaration(catalog)
        myObjType = self.getObjType()
        myName = self.getName()
        print(f"/**")
        print(f" * Creates a deep copy of the {myName} object.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    if (o == NULL) return NULL; {c}")
        print(f"    {myType} x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", x); {c}')
        print(f"    Header _h = x->header; {c}")
        print(f"    bzero(x, sizeof(struct {myName})); {c}")
        print(f"    x->header = _h; {c}")
        print(f"    int save = PROTECT(x); {c}")
        if catalog.parserInfo:
            print(f"    x->_yy_parser_info = o->_yy_parser_info; {c}")
        self.printCopyFunctionBody(catalog)
        print(f"    UNPROTECT(save); {c}")
        print(f"    return x; {c}")
        print(f"}} {c}")
        print("")

    def printPrintFunction(self, catalog):
        c = self.comment('printPrintFunction')
        myName = self.getName()
        print(f"/**")
        print(f" * Prints a representation of the {myName} object for debugging.")
        print(f" * It will recursively print all the fields of the object.")
        print(f" */")
        decl=self.getPrintSignature(catalog)
        print(f"{decl} {{ {c}")
        print(f"    pad(depth); {c}")
        if not self.isInline(catalog):
            print(f'    if (x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        print(f'    eprintf("{myName}[\\n"); {c}')
        self.printPrintFunctionBody(catalog)
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}\n")

    def getDefineValue(self):
        return 'x'

    def getDefineArg(self):
        return 'x'


class DiscriminatedUnionField(EnumField):
    """
    Contains all the information from a field in a discriminated union.
    Shared between DiscriminatedUnionEnum, Union and DiscriminatedUnion objects
    """
    def __init__(self, owner, name, typeName):
        if name is True:
            raise Exception("DiscriminatedUnionField passed a boolean")
        super().__init__(owner, name)
        self.typeName = typeName
        self.default = None

    def getObjName(self, catalog):
        return self.typeName

    def comment(self, method):
        return CommentGen.method_comment('DiscriminatedUnionField', method)

    def printHelperNewDeclaration(self, catalog, isInline):
        ucfirst = self.getName()[0].upper() + self.getName()[1:]
        c = self.comment('printHelperNewDeclaration')
        arg = self.getDefineArg(catalog)
        macroArg = arg
        typeName = self.makeTypeName()
        argMacro = self.getDefineMacro(catalog, self.getName())
        obj = catalog.get(self.typeName)
        owner = catalog.get(self.owner)
        argType = ''
        if arg != '':
            argType = obj.getTypeDeclaration(catalog) + ' '
        parserInfoFarg = ''
        parserInfoAarg = ''
        if owner.hasParserInfo(catalog):
            parserInfoFarg = 'struct ParserInfo I'
            parserInfoAarg = 'I, '
            if argType != '':
                argType = ', ' + argType
        else:
            if arg == '':
                arg = 'void'
        if isInline:
            consPrefix = self.owner[0].lower() + self.owner[1:]
            print(f'static inline {self.owner} {consPrefix}_{ucfirst}({parserInfoFarg}{argType}{arg}) {{ {c}')
            print(f'    return ({self.owner}) {{ .type = {parserInfoAarg}{typeName}, .val = {argMacro}({macroArg}) }}; {c}')
        else:
            print(f'static inline {self.owner} *new{self.owner}_{ucfirst}({parserInfoFarg}{argType}{arg}) {{ {c}')
            print(f'    return new{self.owner}({parserInfoAarg}{typeName}, {argMacro}({macroArg})); {c}')
        print(f'}} {c}')
        print('')

    def printStructTypedefLine(self, catalog):
        c = self.comment('printStructTypedefLine')
        obj = catalog.get(self.typeName)
        otype=obj.getTypeDeclaration(catalog)
        name=self.name
        print(f"    {otype} {name}; {c}")

    def getSignature(self, catalog):
        obj = catalog.get(self.typeName)
        return "{type} {name}".format(type=obj.getTypeDeclaration(catalog), name=self.name)

    def getCopyCall(self, arg, catalog):
        return f'DiscriminatedUnionField_getCopyCall({arg})'

    def getFieldName(self):
        return 'DiscriminatedUnionField_getFieldName'

    def getDefineMacro(self, catalog, user):
        v = self.owner + '_val_' + user
        v = v.upper().replace('AST', 'AST_')
        return v

    def getDefineArg(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getDefineArg()

    def getDefineType(self, catalog):
        return catalog.get(self.owner).getUnion().getTypeDeclaration(catalog)

    def getDefineField(self, catalog):
        return self.name

    def getDefineValue(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getDefineValue()

    def printDefine(self, catalog, user, value):
        keys = {
            "macro": self.getDefineMacro(catalog, user),
            "arg": self.getDefineArg(catalog),
            "type": self.getDefineType(catalog),
            "field": self.getDefineField(catalog),
            "value": value
        }
        print("#define {macro}({arg}) (({type}){{.{field} = ({value})}})".format_map(keys))

    def printDefines(self, catalog):
        self.printDefine(catalog, self.name, self.getDefineValue(catalog))

    def printMarkCase(self, isInline, catalog):
        c = self.comment('printMarkCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printMarkField(isInline, self.name, 3, 'val.')
        print(f"            break; {c}")

    def printProtectCase(self, isInline, catalog):
        c = self.comment('printProtectCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printProtectField(isInline, self.name, 3, 'val.')

    def printCompareCase(self, isInline, catalog):
        c = self.comment('printCompareCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printCompareField(catalog, isInline, self.name, 3, 'val.')
        print(f"            break; {c}")

    def printPrintCase(self, catalog, isInline):
        c = self.comment('printPrintCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        print(f'            pad(depth + 1); {c}')
        print(f'            eprintf("{typeName}\\n"); {c}')
        obj = catalog.get(self.typeName)
        obj.printPrintField(isInline, self.name, 3, 'val.')
        print(f"            break; {c}")

    def printCopyCase(self, catalog, isInline):
        c = self.comment('printCopyCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printCopyField(isInline, self.name, 3, 'val.')
        print(f"            break; {c}")


