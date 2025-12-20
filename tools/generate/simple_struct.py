"""
SimpleStruct class for regular C structs with fields.
"""

from .base import Base
from .enum_field import EnumField
from .simple_field import SimpleField
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
            self.fields = [self.makeField(_x, data[_x]) for _x in data.keys()]
        else:
            raise ValueError(f"SimpleStruct {name} must have 'data' field")

    def hasParserInfo(self, catalog):
        return catalog.getParserInfo(self.name)

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef struct {name} {{ {c}")
        if not self.isInline(catalog):
            print(f"    Header header; {c}")
        if catalog.getParserInfo(self.name):
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
        return f'Index count{myName}({myType} _x)'

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
            print(f'    while (_x != NULL) {{ {c}')
            print(f'        _x = _x->{selfRefField}; {c}')
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
        return TypeHelper.pointer_type(astType)

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
        return [_x for _x in self.fields if _x.default is None and not _x.isSelfInitializing(catalog)]

    def getDefaultArgs(self, catalog):
        return [_x for _x in self.fields if _x.default is not None or _x.isSelfInitializing(catalog)]

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName=self.getName()
        args = []
        for field in self.getNewArgs(catalog):
            args += [field.getSignature(catalog)]
        if len(args) == 0:
            args += ['void']
        if catalog.getParserInfo(self.name):
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
        print(f"    {myType} _x = NEW({myName}, {myObjType}); {c}")
        if hasInternalConstructors:
            print(f"    Header _h = _x->header; {c}")
            print(f"    bzero(_x, sizeof(struct {myName})); {c}")
            print(f"    _x->header = _h; {c}")
            print(f"    int save = PROTECT(_x); {c}")
        print(f'    DEBUG("new {myName} %p", _x); {c}')
        if catalog.getParserInfo(self.name):
            print(f"    _x->_yy_parser_info = _PI; {c}")
        for field in self.getNewArgs(catalog):
            f = field.getFieldName()
            print(f"    _x->{f} = {f}; {c}")
        for field in self.getDefaultArgs(catalog):
            f = field.getFieldName()
            if field.isSelfInitializing(catalog) and field.default is None:
                d = f'{field.getConstructorName(catalog)}()'
            else:
                d = field.default
            print(f"    _x->{f} = {d}; {c}")
        if hasInternalConstructors:
            print(f"    UNPROTECT(save); {c}")
        print(f"    return _x; {c}")
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
        print(f"mark{myName}(_x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        myName=self.getName()
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f"return PROTECT(_x{a}{prefix}{field}); {c}")

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
        print(f'print{myName}(_x{a}{prefix}{field}, depth + 1); {c}')

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        myName=self.getName()
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f'_x{a}{prefix}{field} = copy{myName}(o{a}{prefix}{field}); {c}')

    def printMarkFunction(self, catalog):
        c = self.comment('printMarkFunction')
        decl = self.getMarkSignature(catalog)
        print(f"/**")
        print(f" * Marks the {self.getName()} object to protect it from garbage collection.")
        print(f" * It will recursively mark all the fields of the object.")
        print(f" */")
        print(f"{decl} {{ {c}")
        if not self.isInline(catalog):
            print(f"    if (_x == NULL) return; {c}")
            print(f"    if (MARKED(_x)) return; {c}")
            print(f"    MARK(_x); {c}")
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
        print(f"    FREE(_x, {self.getName()}); {c}")
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
        print(f"    {myType} _x = NEW({myName}, {myObjType}); {c}")
        print(f'    DEBUG("copy {myName} %p", _x); {c}')
        print(f"    Header _h = _x->header; {c}")
        print(f"    bzero(_x, sizeof(struct {myName})); {c}")
        print(f"    _x->header = _h; {c}")
        print(f"    int save = PROTECT(_x); {c}")
        if catalog.getParserInfo(self.name):
            print(f"    _x->_yy_parser_info = o->_yy_parser_info; {c}")
        self.printCopyFunctionBody(catalog)
        print(f"    UNPROTECT(save); {c}")
        print(f"    return _x; {c}")
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
            print(f'    if (_x == NULL) {{ eprintf("{myName} (NULL)"); return; }} {c}')
        print(f'    eprintf("{myName}[\\n"); {c}')
        self.printPrintFunctionBody(catalog)
        print(f"    pad(depth); {c}")
        print(f'    eprintf("]"); {c}')
        print(f"}} {c}\n")

    def getDefineValue(self):
        return '_x'

    def getDefineArg(self):
        return '_x'

    def generateVisitorDecl(self):
        """Generate forward declaration for visitor function"""
        myName = self.getName()
        return f"static {myName} *visit{myName}({myName} *node, VisitorContext *context);\n"

    def generateVisitor(self, catalog):
        """Generate visitor function implementation"""
        myName = self.getName()
        output = []
        
        output.append(f"static {myName} *visit{myName}({myName} *node, VisitorContext *context) {{\n")
        output.append(f"    if (node == NULL) return NULL;\n")
        output.append(f"\n")
        
        # Track which fields need visiting
        visitableFields = []
        for field in self.fields:
            if field.default is None:  # Skip auto-init fields
                visitableFields.append(field)
        
        if not visitableFields:
            # No fields to visit, just return the node
            output.append(f"    (void)context;  // Unused parameter\n")
            output.append(f"    return node;\n")
            output.append(f"}}\n\n")
            return ''.join(output)
        
        # Track whether we've done the initial PROTECT yet and whether we have visitable fields
        saved = False
        visitedFields = []  # Fields that were actually visited (have new_ variables)
        hasMemoryManagedFields = False
        
        # First pass: determine if we have any memory-managed fields
        for field in visitableFields:
            fieldType = field.typeName
            try:
                fieldObj = catalog.get(fieldType)
                if fieldObj.needsProtection(catalog):
                    hasMemoryManagedFields = True
                    break
            except:
                # Field type not in catalog - assume it needs protection
                hasMemoryManagedFields = True
                break
        
        # Only declare changed if we have memory-managed fields
        if hasMemoryManagedFields:
            output.append(f"    bool changed = false;\n")
        
        # Visit each field and track changes
        
        for field in visitableFields:
            fieldName = field.getName()
            fieldType = field.typeName
            
            try:
                fieldObj = catalog.get(fieldType)
                
                if fieldObj.needsProtection(catalog):
                    # Memory-managed type - visit and protect result
                    output.append(f"    {fieldType} *new_{fieldName} = visit{fieldType}(node->{fieldName}, context);\n")
                    # On first visitable field, capture the PROTECT result in save
                    if not saved:
                        output.append(f"    int save = PROTECT(new_{fieldName});\n")
                        saved = True
                    else:
                        output.append(f"    PROTECT(new_{fieldName});\n")
                    output.append(f"    changed = changed || (new_{fieldName} != node->{fieldName});\n")
                    visitedFields.append(field)
                else:
                    # Non-memory-managed type - pass through unchanged
                    output.append(f"    // Pass through {fieldName} (type: {fieldType}, not memory-managed)\n")
                    
            except Exception as e:
                # Field type not in catalog - this should be rare, log for debugging
                import sys
                print(f"Warning: Field {fieldName} has type {fieldType} not in catalog", file=sys.stderr)
                # Assume it's a primitive/external type that doesn't need visiting
                output.append(f"    // Pass through {fieldName} (type: {fieldType}, not in catalog)\n")
        
        output.append(f"\n")
        
        # Check if anything changed
        if visitedFields:
            output.append(f"    if (changed) {{\n")
            output.append(f"        // Create new node with modified fields\n")
            
            # Build constructor call
            constructorArgs = []
            for field in self.getNewArgs(catalog):
                fieldName = field.getName()
                if field in visitedFields:
                    constructorArgs.append(f"new_{fieldName}")
                else:
                    constructorArgs.append(f"node->{fieldName}")
            
            if catalog.getParserInfo(self.name):
                constructorArgs.insert(0, "CPI(node)")
                
            args_str = ", ".join(constructorArgs) if constructorArgs else ""
            output.append(f"        {myName} *result = new{myName}({args_str});\n")
            if saved:
                output.append(f"        UNPROTECT(save);\n")
            output.append(f"        return result;\n")
            output.append(f"    }}\n")
            output.append(f"\n")
        else:
            # No fields were actually visited (all pass-through)
            output.append(f"    (void)context;  // Unused parameter - all fields are pass-through\n")
        
        if saved:
            output.append(f"    UNPROTECT(save);\n")
        output.append(f"    return node;\n")
        output.append(f"}}\n\n")
        
        return ''.join(output)
