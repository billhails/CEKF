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
Type declaration utilities.

This module provides utilities for generating C type declarations,
handling the distinction between inline (call-by-value) and normal
(pointer-based, GC-managed) types.
"""


class TypeHelper:
    """Generate C type declarations"""
    
    @staticmethod
    def struct_type(name, is_inline):
        """
        Generate struct type declaration.
        
        Args:
            name: The struct name (e.g., "AstExpression")
            is_inline: True for call-by-value (no pointer), False for heap-allocated
            
        Returns:
            "struct Name " for inline (note trailing space)
            "struct Name *" for normal (pointer)
        """
        if is_inline:
            return f"struct {name} "
        else:
            return f"struct {name} *"
    
    @staticmethod
    def enum_type(name):
        """
        Generate enum type declaration.
        
        Args:
            name: The enum name
            
        Returns:
            "enum Name " (note trailing space)
        """
        return f"enum {name} "
    
    @staticmethod
    def union_type(name):
        """
        Generate union type declaration.
        
        Used for discriminated union value storage.
        
        Args:
            name: The union name
            
        Returns:
            "union Name " (note trailing space, always inline)
        """
        return f"union {name} "
    
    @staticmethod
    def primitive_type(cname):
        """
        Generate primitive type declaration.
        
        Primitives are always inline (passed by value).
        
        Args:
            cname: The C type name (e.g., "int", "char", "BigInt *")
            
        Returns:
            The C type name as-is
        """
        return cname
    
    @staticmethod
    def pointer_type(type_name):
        """
        Generate C pointer type declaration.
        
        Used for extra comparison arguments and similar cases.
        
        Args:
            type_name: Base type name (e.g., "AstExpression")
            
        Returns:
            String like "AstExpression *"
        """
        return f"{type_name} *"
