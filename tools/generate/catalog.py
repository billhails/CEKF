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
Catalog class - central registry for all generated entities.

The Catalog manages the collection of structures, unions, enums, arrays, etc.
that are defined in a YAML schema file, and orchestrates their code generation.
"""

from .comment_gen import CommentGen
from .switch_helper import SwitchHelper


class Catalog:
    """Central registry managing all entities in a schema."""
    
    def __init__(self):
        self.contents = {}

    def add(self, value, external):
        value.setExternal(external)
        name = value.getName()
        if name in self.contents:
            raise Exception("attempt to overwrite " + name + " in catalog")
        self.contents[name] = value

    def get(self, key):
        key = key.strip()
        if key in self.contents:
            return self.contents[key]
        else:
            raise Exception("key '" + key + "' not found in catalog")

    def build(self):
        values = []
        for entity in list(self.contents.values()):
            entity.build(self)
    
    def getParserInfo(self, key):
        key = key.strip()
        if key in self.contents:
            return self.contents[key].getParserInfo()
        else:
            raise Exception("key '" + key + "' not found in catalog")

    def _dispatch(self, method_name, *args):
        """
        Generic dispatcher - calls method_name on all entities.
        
        Args:
            method_name: Name of method to call on each entity
            *args: Arguments to pass to the method (usually self for catalog)
        """
        for entity in self.contents.values():
            if not entity.isExternal():
                method = getattr(entity, method_name)
                method(*args)

    def printHelperNewDeclarations(self):
        self._dispatch('printHelperNewDeclarations', self)

    def printGetterDeclarations(self):
        self._dispatch('printGetterDeclarations', self)

    def printSetterDeclarations(self):
        self._dispatch('printSetterDeclarations', self)

    def printIsTesterDeclarations(self):
        self._dispatch('printIsTesterDeclarations', self)

    def generateVisitor(self, packageName, target):
        """Generate complete visitor boilerplate"""
        output = []
        
        # Includes
        output.append(f'#include "{packageName}.h"\n')
        output.append('#include "memory.h"\n\n')
        output.append(f'#include "{packageName}_{target}.h"\n\n')
        
        # Conditional debugging include
        debug_macro = f"DEBUG_{packageName.upper()}_{target.upper()}"
        output.append(f'#ifdef {debug_macro}\n')
        output.append('#  include "debugging_on.h"\n')
        output.append('#else\n')
        output.append('#  include "debugging_off.h"\n')
        output.append('#endif\n\n')
        
        # Context struct skeleton
        output.append("typedef struct VisitorContext {\n")
        output.append("    // Add your context fields here\n")
        output.append("} VisitorContext;\n\n")
        
        # Forward declarations
        for entity in self.contents.values():
            if entity.isExternal():
                continue
            decl = entity.generateVisitorDecl(target)
            if decl:
                output.append(decl)
        output.append("\n")
        
        # Implementations
        output.append("///////////////////////////\n")
        output.append("// Visitor implementations\n")
        output.append("///////////////////////////\n\n")
        for entity in self.contents.values():
            if entity.isExternal():
                continue
            impl = entity.generateVisitor(self, target)
            if impl:
                output.append(impl)
        
        return ''.join(output)

    def printTypedefs(self):
        for entity in self.contents.values():
            if entity.isEnum() and not entity.isExternal():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isArray() and not entity.isExternal():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isUnion() and not entity.isExternal():
                entity.printTypedef(self)
        # Print inline structs first (no dependencies, can be used by other structs)
        for entity in self.contents.values():
            if entity.isStruct() and entity.isInline(self) and not entity.isExternal():
                entity.printTypedef(self)
        # Then print regular structs
        for entity in self.contents.values():
            if entity.isStruct() and not entity.isInline(self) and not entity.isExternal():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isHash() and not entity.isExternal():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isVector() and not entity.isExternal():
                entity.printTypedef(self)

    def printInitDeclarations(self):
        self._dispatch('printInitDeclaration', self)

    def printInitFunctions(self):
        self._dispatch('printInitFunction', self)

    def printMarkDeclarations(self):
        self._dispatch('printMarkDeclaration', self)

    def printCountDeclarations(self):
        self._dispatch('printCountDeclaration', self)

    def printCountFunctions(self):
        self._dispatch('printCountFunction', self)

    def printAccessDeclarations(self):
        self._dispatch('printAccessDeclarations', self)

    def printPushDeclarations(self):
        self._dispatch('printPushDeclaration', self)

    def printPushFunctions(self):
        self._dispatch('printPushFunction', self)

    def printPopDeclarations(self):
        self._dispatch('printPopDeclaration', self)

    def printPopnDeclarations(self):
        self._dispatch('printPopnDeclaration', self)

    def printMoveDeclarations(self):
        self._dispatch('printMoveDeclaration', self)

    def printPushnDeclarations(self):
        self._dispatch('printPushnDeclaration', self)

    def printCopyTopDeclarations(self):
        self._dispatch('printCopyTopDeclaration', self)

    def printCopyExceptTopDeclarations(self):
        self._dispatch('printCopyExceptTopDeclaration', self)

    def printCopyEntriesDeclarations(self):
        self._dispatch('printCopyEntriesDeclaration', self)

    def printClearDeclarations(self):
        self._dispatch('printClearDeclaration', self)

    def printPeekDeclarations(self):
        self._dispatch('printPeekDeclaration', self)

    def printPeeknDeclarations(self):
        self._dispatch('printPeeknDeclaration', self)

    def printPokeDeclarations(self):
        self._dispatch('printPokeDeclaration', self)

    def printAppendDeclarations(self):
        self._dispatch('printAppendDeclaration', self)

    def printAddDeclarations(self):
        self._dispatch('printAddDeclaration', self)

    def printExtendDeclarations(self):
        self._dispatch('printExtendDeclaration', self)

    def printSizeDeclarations(self):
        self._dispatch('printSizeDeclaration', self)

    def printPopFunctions(self):
        self._dispatch('printPopFunction', self)

    def printPopnFunctions(self):
        self._dispatch('printPopnFunction', self)

    def printMoveFunctions(self):
        self._dispatch('printMoveFunction', self)

    def printPushnFunctions(self):
        self._dispatch('printPushnFunction', self)

    def printCopyTopFunctions(self):
        self._dispatch('printCopyTopFunction', self)

    def printCopyExceptTopFunctions(self):
        self._dispatch('printCopyExceptTopFunction', self)

    def printCopyEntriesFunctions(self):
        self._dispatch('printCopyEntriesFunction', self)

    def printPeekFunctions(self):
        self._dispatch('printPeekFunction', self)

    def printPeeknFunctions(self):
        self._dispatch('printPeeknFunction', self)

    def printPokeFunctions(self):
        self._dispatch('printPokeFunction', self)

    def printAppendFunctions(self):
        self._dispatch('printAppendFunction', self)

    def printAddFunctions(self):
        self._dispatch('printAddFunction', self)

    def printExtendFunctions(self):
        self._dispatch('printExtendFunction', self)

    def printSetDeclarations(self):
        self._dispatch('printSetDeclaration', self)

    def printGetDeclarations(self):
        self._dispatch('printGetDeclaration', self)

    def printSetFunctions(self):
        self._dispatch('printSetFunction', self)

    def printGetFunctions(self):
        self._dispatch('printGetFunction', self)

    def printIteratorDeclarations(self):
        self._dispatch('printIteratorDeclaration', self)

    def printIteratorFunctions(self):
        self._dispatch('printIteratorFunction', self)

    def printFreeDeclarations(self):
        self._dispatch('printFreeDeclaration', self)

    def printProtectDeclarations(self):
        self._dispatch('printProtectDeclaration', self)

    def printProtectFunctions(self):
        self._dispatch('printProtectFunction', self)

    def printNewDeclarations(self):
        self._dispatch('printNewDeclaration', self)

    def printCopyDeclarations(self):
        self._dispatch('printCopyDeclaration', self)

    def printNameFunctionDeclarations(self):
        self._dispatch('printNameFunctionDeclaration')

    def printNameFunctionBodies(self):
        self._dispatch('printNameFunctionBody')

    def printPrintFunctions(self):
        self._dispatch('printPrintFunction', self)

    def printEqFunctions(self):
        self._dispatch('printEqFunction', self)

    def printPrintDeclarations(self):
        self._dispatch('printPrintDeclaration', self)

    def printEqDeclarations(self):
        self._dispatch('printEqDeclaration', self)

    def printDefines(self):
        self._dispatch('printDefines', self)

    def printNewFunctions(self):
        self._dispatch('printNewFunction', self)

    def printCopyFunctions(self):
        self._dispatch('printCopyFunction', self)

    def printMarkFunctions(self):
        self._dispatch('printMarkFunction', self)

    def printFreeFunctions(self):
        self._dispatch('printFreeFunction', self)

    def printMermaid(self):
        self._dispatch('printMermaid', self)

    def printMarkObjFunction(self, packageName):
        SwitchHelper.print_switch_function(
            self, packageName, 'printMarkObjFunction', 'mark{Type}Obj', 'struct Header *h',
            'printMarkObjCase',
            f'cant_happen("unrecognised type %d in mark{packageName.capitalize()}Obj\\n", h->type);'
        )

    def printFreeObjFunction(self, packageName):
        SwitchHelper.print_switch_function(
            self, packageName, 'printFreeObjFunction', 'free{Type}Obj', 'struct Header *h',
            'printFreeObjCase',
            f'cant_happen("unrecognised type %d in free{packageName.capitalize()}Obj\\n", h->type);'
        )

    def printTypeObjFunction(self, packageName):
        SwitchHelper.print_switch_function(
            self, packageName, 'printTypeObjFunction', 'typename{Type}Obj', 'int type',
            'printTypeObjCase',
            'return "???"; // no error, can be used during error reporting',
            'char *'
        )

    def printObjTypeDefine(self, packageName):
        objTypeArray = []
        for entity in self.contents.values():
            if not entity.isExternal():
                objTypeArray += entity.objTypeArray()
        print("#define {packageName}_OBJTYPES() \\\n{a}".format(a=', \\\n'.join(objTypeArray), packageName=packageName.upper()))

    def printObjCasesDefine(self, packageName):
        print(f"#define {packageName.upper()}_OBJTYPE_CASES() \\")
        for entity in self.contents.values():
            if not entity.isExternal():
                objType = entity.objTypeArray()
                if len(objType) == 1:
                    print(f'case {objType[0]}:\\')
        print("")
