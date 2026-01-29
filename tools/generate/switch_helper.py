"""
Helper for generating switch statement dispatch functions.
Consolidates the pattern of creating switch-based GC/debug functions.
"""

from .comment_gen import CommentGen


class SwitchHelper:
    """
    Generates switch statement dispatcher functions for object type handlers.
    Used for mark/free/typename functions that switch on object type.
    """

    @staticmethod
    def print_switch_function(catalog, packageName, method_name, func_name, param_decl, case_method, default_action, return_type='void'):
        """
        Print a switch-based dispatcher function.
        
        Args:
            catalog: The catalog containing entities
            packageName: The package name (e.g., 'Lambda')
            func_name: Name of function to generate (e.g., 'mark{Type}Obj')
            param_decl: Parameter declaration (e.g., 'struct Header *h')
            case_method: Method name to call on each entity for case generation
            default_action: Code for default case (error handling or return value)
            return_type: Return type of function (default 'void')
        """
        c = CommentGen.method_comment('Catalog', method_name)
        type_cap = packageName.capitalize()
        func_full_name = func_name.format(Type=type_cap)
        
        print(f'{return_type} {func_full_name}({param_decl}) {{ {c}')
        
        # Determine switch expression based on parameter type
        switch_expr = 'type' if param_decl.startswith('int ') else 'h->type'
        print(f'    switch({switch_expr}) {{ {c}')
        
        # Generate cases from entities
        for entity in catalog.contents.values():
            method = getattr(entity, case_method)
            method(catalog)
        
        # Default case
        print(f'        default: {c}')
        # If default_action has an inline comment, insert {c} before it
        if '//' in default_action:
            parts = default_action.split('//', 1)
            print(f'            {parts[0]}{c} {parts[1]}')
        else:
            print(f'            {default_action} {c}')
        print(f'    }} {c}')
        print(f'}} {c}')
