"""
Signature Helper - Centralized function signature generation.

This module provides utilities for generating C function signatures
with consistent formatting across all entity types.
"""

class SignatureHelper:
    """
    Utilities for generating C function signatures.
    
    Standard patterns:
    - mark: void markName(Type x)
    - free: void freeName(Type x)
    - print: void printName(Type x, int depth)
    - copy: Type copyName(Type x)
    - compare: bool eqName(Type a, Type b)
    - new: Type newName(...args)
    """
    
    @staticmethod
    def mark_signature(name, type_decl):
        """Generate mark function signature: void markName(Type x)"""
        return f"void mark{name}({type_decl} x)"
    
    @staticmethod
    def free_signature(name, type_decl):
        """Generate free function signature: void freeName(Type x)"""
        return f"void free{name}({type_decl} x)"
    
    @staticmethod
    def print_signature(name, type_decl):
        """Generate print function signature: void printName(Type x, int depth)"""
        return f"void print{name}({type_decl} x, int depth)"
    
    @staticmethod
    def copy_signature(name, type_decl):
        """Generate copy function signature: Type copyName(Type x)"""
        return f"{type_decl} copy{name}({type_decl} o)"
    
    @staticmethod
    def compare_signature(name, type_decl, extra_args=""):
        """
        Generate compare function signature: bool eqName(Type a, Type b[, extra...])
        
        Args:
            name: Function base name
            type_decl: Type declaration string
            extra_args: Optional extra arguments (include leading comma if non-empty)
        """
        return f"bool eq{name}({type_decl} a, {type_decl} b{extra_args})"
    
    @staticmethod
    def new_signature(name, type_decl, args):
        """
        Generate new/constructor function signature: Type newName(args...)
        
        Args:
            name: Function base name
            type_decl: Return type declaration
            args: List of argument strings or ['void'] if no args
        """
        return f"{type_decl} new{name}({', '.join(args)})"
