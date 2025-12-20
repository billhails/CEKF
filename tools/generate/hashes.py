#
# CEKF - VM supporting amb
# Copyright (C) 2022-2025  Bill Hails
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

"""
Hash table structures
"""

from .base import Base
from .simple_field import SimpleField
from .utils import pad
from .comment_gen import CommentGen
from .type_helper import TypeHelper
from .signature_helper import SignatureHelper
from .accessor_helper import AccessorHelper


class SimpleHash(Base):
    """
    Hash tables
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            if "entries" in data:
                self.entries = SimpleField(self.name, "entries", data["entries"])
            else:
                self.entries = None
        else:
            raise Exception(f"SimpleHash {name} must have 'data' field")

    def isHash(self):
        return True

    def printMermaid(self, catalog):
        myName = self.getName()
        if self.entries is None:
            print(f'{myName} --entries--> NULL')
        else:
            print(f'{myName} --entries--> {self.entries.getObjName(catalog)}')

    def isSelfInitializing(self):
        return True # other constructors will call this automatically

    def getConstructorName(self):
        myName = self.getName()
        return f"new{myName}"

    def getTypeDeclaration(self, catalog):
        return TypeHelper.struct_type(self.getName(), is_inline=False)

    def printNewDeclaration(self, catalog):
        c = self.comment('printNewDeclaration')
        decl=self.getNewSignature(catalog)
        print(f"{decl}; {c}")

    def getNewSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myConstructor = self.getConstructorName()
        # Hash uses full constructor name (already has 'new' prefix)
        return f"{myType}{myConstructor}(void)"

    def getSetDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        if self.entries is None:
            return f'void set{myName}({myType} table, HashSymbol *key)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'void set{myName}({myType} table, HashSymbol *key, {valueType} value)'

    def printSetDeclaration(self, catalog):
        c = self.comment('printSetDeclaration')
        decl = self.getSetDeclaration(catalog)
        print(f'{decl}; {c}')

    def getGetDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        if self.entries is None:
            return f'bool get{myName}({myType} table, HashSymbol *key)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'bool get{myName}({myType} table, HashSymbol *key, {valueType}* value)'

    def printGetDeclaration(self, catalog):
        c = self.comment('printGetDeclaration')
        decl = self.getGetDeclaration(catalog)
        print(f'{decl}; {c}')

    def getIteratorDeclaration(self, catalog):
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        if self.entries is None:
            return f'HashSymbol * iterate{myName}({myType} table, Index *i)'
        else:
            valueType = self.entries.getTypeDeclaration(catalog)
            return f'HashSymbol * iterate{myName}({myType} table, Index *i, {valueType}*value)'

    def printIteratorDeclaration(self, catalog):
        c = self.comment('printIteratorDeclaration')
        decl = self.getIteratorDeclaration(catalog)
        print(f'{decl}; {c}')

    def printIteratorFunction(self, catalog):
        c = self.comment('printIteratorFunction')
        decl = self.getIteratorDeclaration(catalog)
        print(f"/**")
        print(f" * @brief Iterate over the entries in a {self.getName()}.")
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
        if self.entries is None:
            print('    return iterateHashTable((HashTable *)table, i, NULL);')
        else:
            print('    return iterateHashTable((HashTable *)table, i, value);')
        print(f'}} {c}')
        print('')

    def printSetFunction(self, catalog):
        c = self.comment('printSetFunction')
        decl = self.getSetDeclaration(catalog)
        print(f"/**")
        print(f" * @brief Set a key in the {self.getName()}.")
        if self.entries is None:
            print(f" * This sets the key in the underlying HashTable with a null value.")
        else:
            print(f" * This sets the key in a HashTable of {self.entries.getTypeDeclaration(catalog)}.")
        print(f" */")
        print(f'{decl} {{ {c}')
        if self.entries is None:
            print(f'    hashSet((HashTable *)table, key, NULL); {c}')
        else:
            print(f'    hashSet((HashTable *)table, key, &value); {c}')
        print(f'}} {c}')
        print('')

    def printGetFunction(self, catalog):
        c = self.comment('printGetFunction')
        decl = self.getGetDeclaration(catalog)
        print(f"/**")
        print(f" * @brief Get a key from the {self.getName()}.")
        print(f" *")
        print(f" * This checks if the key is present in the underlying HashTable.")
        if self.entries is not None:
            entry = self.entries.getTypeDeclaration(catalog)
            print(f" * If {entry}* value is not NULL then the {entry} associated with the key is placed in the pointer.")
        print(f" *")
        print(f" * @return true if the key is present, false otherwise.")
        print(f" */")
        print(f'{decl} {{ {c}')
        if self.entries is None:
            print(f'    return hashGet((HashTable *)table, key, NULL); {c}')
        else:
            print(f'    return hashGet((HashTable *)table, key, value); {c}')
        print(f'}} {c}')
        print('')

    def printCountDeclaration(self, catalog):
        c = self.comment('printCountDeclaration')
        myName = self.getName()
        myType = self.getTypeDeclaration(catalog)
        print(f"/**")
        print(f" * @brief Get the number of entries in the {myName}.")
        print(f" */")
        print(f'static inline Index count{myName}({myType} table) {{ {c}')
        print(f'    return ((HashTable *)table)->count; {c}')
        print(f'}} {c}')
        print('')

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        myName = self.getName()
        self.printBaseDocumentation()
        print(f'typedef struct {myName} {{ {c}')
        print(f'    struct HashTable wrapped; {c}')
        print(f'}} {myName}; {c}')
        print('')

    def getPrintSignature(self, catalog):
        myType = self.getTypeDeclaration(catalog)
        myName = self.getName()
        return f"void print{myName}({myType} _x, int depth)"

    def printPrintDeclaration(self, catalog):
        c = self.comment('printPrintDeclaration')
        decl = self.getPrintSignature(catalog)
        print(f"{decl}; {c}")

    def printPrintFunction(self, catalog):
        decl = self.getPrintSignature(catalog)
        c = self.comment('printPrintFunction')
        print(f"/**")
        print(f" * @brief Print the contents of a {self.getName()} for debugging.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    printHashTable(&(_x->wrapped), depth); {c}")
        print(f"}} {c}")
        print("")

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        myConstructor = self.getConstructorName()
        a = AccessorHelper.accessor(isInline)
        print(f'    _x{a}{prefix}{field} = {myConstructor}(); {c}')
        print(f'    copyHashTable((HashTable *)_x{a}{prefix}{field}, (HashTable *)o{a}{prefix}{field}); {c}')

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        pad(depth)
        print(f'printHashTable(*(HashTable **)ptr, depth + 1); {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        a = AccessorHelper.accessor(isInline)
        pad(depth)
        print(f'printHashTable((HashTable *)_x{a}{prefix}{field}, depth + 1); {c}')

    def printCompareField(self, catalog, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        pad(depth)
        print(f"return false; {c}")

    def printNewFunction(self, catalog):
        c = self.comment('printNewFunction')
        decl = self.getNewSignature(catalog)
        myName = self.getName()
        if self.entries is None:
            size = '0'
            markFn = 'NULL'
            printFn = 'NULL'
        else:
            size = f'sizeof({self.entries.getTypeDeclaration(catalog)})'
            printFn = f'_print{myName}'
            if self.entries.hasMarkFn(catalog):
                markFn = f'_mark{myName}'
                print(f'static void {markFn}(void *ptr) {{ {c}')
                self.entries.printMarkHashLine(catalog, 1)
                print(f'}} {c}')
                print('')
            else:
                markFn = 'NULL'
            self.entries.printPrintDeclaration(catalog)
            print('')
            print(f'static void {printFn}(void *ptr, int depth) {{ {c}')
            self.entries.printPrintHashLine(catalog, 1)
            print(f'}} {c}')
            print('')
        print(f"/**")
        print(f" * @brief Create a new {myName}.")
        print(f" * This function initializes a new {myName} structure,")
        if self.entries is None:
            print(f" * which is a wrapper around a HashTable with no values (a set of symbols).")
        else:
            print(f" * which is a wrapper around a HashTable of {self.entries.getTypeDeclaration(catalog)}.")
        print(f" */")
        print(f'{decl} {{ {c}')
        print(f'    return ({myName} *)newHashTable({size}, {markFn}, {printFn}); {c}')
        print(f'}} {c}')
        print('')

    def printMarkField(self, isInline, field, depth, prefix=''):
        c = self.comment('printMarkField')
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f"markHashTable((HashTable *)_x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        pad(depth)
        a = AccessorHelper.accessor(isInline)
        print(f"return PROTECT((HashTable *)_x{a}{prefix}{field}); {c}")

    def generateVisitorDecl(self):
        """Generate forward declaration for visitor function"""
        myName = self.getName()
        return f"static {myName} *visit{myName}({myName} *node, VisitorContext *context);\n"

    def generateVisitor(self, catalog):
        """Generate stub visitor function - TODO: implement hash table traversal"""
        myName = self.getName()
        output = []
        output.append(f"__attribute__((unused))\n")
        output.append(f"static {myName} *visit{myName}({myName} *node, VisitorContext *context) {{\n")
        output.append(f"    (void)context;  // TODO: implement hash table visitor\n")
        output.append(f"    return node;  // TODO: traverse and rebuild hash table if values change\n")
        output.append(f"}}\n\n")
        return ''.join(output)
