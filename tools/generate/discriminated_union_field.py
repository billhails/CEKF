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

"""Field classes for struct and union members."""

import re
from .enum_field import EnumField
from .comment_gen import CommentGen


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

    def printMakeHelperDeclaration(self, catalog, owner, isInline):
        """
        Generate make<Union>_<Field> helper that combines struct construction
        with GC protection and union wrapping.
        """
        from .make_helper import MakeHelperGenerator
        
        obj = catalog.get(self.typeName)
        has_parser_info = owner.hasParserInfo(catalog)
        
        MakeHelperGenerator.print_make_helper(
            self.owner,
            self.getName(),
            obj,
            catalog,
            has_parser_info,
            isInline
        )

    def printGetterDeclaration(self, catalog, owner, isInline):
        """
        Generate get<Union>_<Field> inline function that validates the variant type
        and returns the field value, calling cant_happen if incorrect.
        
        Example: static inline struct LamIff *getLamExp_Iff(struct LamExp *_x)
        """
        c = self.comment('printGetterDeclaration')
        ucfirst = self.getName()[0].upper() + self.getName()[1:]
        typeName = self.makeTypeName()
        obj = catalog.get(self.typeName)
        returnType = obj.getTypeDeclaration(catalog)
        ownerType = owner.getTypeDeclaration(catalog)
        accessor = '.' if isInline else '->'
        
        # Get the typename function for better error messages
        # Convert "LamExp" -> "lamExpTypeName"
        typeNameFunc = self.owner[0].lower() + self.owner[1:] + 'TypeName'
        
        # ownerType already includes '*' for non-inline or ' ' for inline
        # returnType may not have trailing space for primitives, so add one
        if not returnType.endswith(' '):
            returnType = returnType + ' '
        
        # Generate the inline getter function
        print(f'static inline {returnType}get{self.owner}_{ucfirst}({ownerType}_x) {{ {c}')
        print(f'#ifdef SAFETY_CHECKS {c}')
        print(f'    if (_x{accessor}type != {typeName}) {{ {c}')
        print(f'        cant_happen("Expected {typeName}, got %s in get{self.owner}_{ucfirst}", {typeNameFunc}(_x{accessor}type)); {c}')
        print(f'    }} {c}')
        print(f'#endif {c}')
        print(f'    return _x{accessor}val.{self.name}; {c}')
        print(f'}} {c}')
        print('')

    def printSetterDeclaration(self, catalog, owner, isInline):
        """
        Generate set<Union>_<Field> inline function that validates the variant type
        and sets the field value, calling cant_happen if incorrect.
        
        Example: static inline void setLamExp_Iff(struct LamExp *_x, struct LamIff *_val)
        """
        c = self.comment('printSetterDeclaration')
        ucfirst = self.getName()[0].upper() + self.getName()[1:]
        typeName = self.makeTypeName()
        obj = catalog.get(self.typeName)
        valueType = obj.getTypeDeclaration(catalog)
        ownerType = owner.getTypeDeclaration(catalog)
        accessor = '.' if isInline else '->'
        
        # Get the typename function for better error messages
        # Convert "LamExp" -> "lamExpTypeName"
        typeNameFunc = self.owner[0].lower() + self.owner[1:] + 'TypeName'
        
        # valueType may not have trailing space for primitives, so add one
        if not valueType.endswith(' '):
            valueType = valueType + ' '
        
        # Generate the inline setter function
        print(f'static inline void set{self.owner}_{ucfirst}({ownerType}_x, {valueType}_val) {{ {c}')
        print(f'#ifdef SAFETY_CHECKS {c}')
        print(f'    if (_x{accessor}type != {typeName}) {{ {c}')
        print(f'        cant_happen("Expected {typeName}, got %s in set{self.owner}_{ucfirst}", {typeNameFunc}(_x{accessor}type)); {c}')
        print(f'    }} {c}')
        print(f'#endif {c}')
        print(f'    _x{accessor}val.{self.name} = _val; {c}')
        print(f'}} {c}')
        print('')

    def printIsTesterDeclaration(self, catalog, owner, isInline):
        """
        Generate is<Union>_<Field> inline function that tests if the union
        is the specified variant type.
        
        Example: static inline bool isLamExp_Iff(struct LamExp *_x)
        """
        c = self.comment('printIsTesterDeclaration')
        ucfirst = self.getName()[0].upper() + self.getName()[1:]
        typeName = self.makeTypeName()
        ownerType = owner.getTypeDeclaration(catalog)
        accessor = '.' if isInline else '->'
        
        # Generate the inline tester function
        print(f'static inline bool is{self.owner}_{ucfirst}({ownerType}_x) {{ {c}')
        print(f'    return _x{accessor}type == {typeName}; {c}')
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

    def printEqCase(self, isInline, catalog):
        c = self.comment('printEqCase')
        typeName = self.makeTypeName()
        print(f"        case {typeName}: {c}")
        obj = catalog.get(self.typeName)
        obj.printEqField(catalog, isInline, self.name, 3, 'val.')
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
