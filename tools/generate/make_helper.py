"""
Helper for generating make<Union>_<Field> convenience functions.
Consolidates the pattern of constructing a union field struct then wrapping it.
"""

from .comment_gen import CommentGen


class MakeHelperGenerator:
    """
    Generates make<Union>_<Field>() static inline functions that combine
    struct construction with GC protection and union wrapping.
    
    Pattern generated:
        static inline UnionType *makeUnion_Field(ParserInfo PI, field_args...) {
            FieldType *x1 = newFieldType(PI, field_args...);
            int save = PROTECT(x1);
            UnionType *x2 = newUnion_Field(PI, x1);
            UNPROTECT(save);
            return x2;
        }
    """

    @staticmethod
    def print_make_helper(union_name, field_name, field_obj, catalog, has_parser_info, is_inline):
        """
        Print a make<Union>_<Field>() helper function.
        
        Args:
            union_name: Name of the union type (e.g., 'LamExp')
            field_name: Name of the field (e.g., 'iff')
            field_obj: The object representing the field's type
            catalog: The catalog for type lookUps
            has_parser_info: Whether to include ParserInfo parameter
            is_inline: Whether the union is inline (call-by-value)
        """
        # Skip if field type doesn't need protection
        if not field_obj.needsProtection(catalog):
            return
        
        # Get the field type name
        field_type_name = field_obj.getName()
        
        # Capitalize first letter of field name for function suffix
        field_suffix = field_name[0].upper() + field_name[1:]
        
        # Get constructor arguments for the field struct
        field_args = MakeHelperGenerator._get_constructor_args(field_obj, catalog, has_parser_info)
        
        # Check if field constructor takes ParserInfo
        field_takes_parser_info = MakeHelperGenerator._field_takes_parser_info(field_obj, catalog)
        
        # Build the function signature
        c = CommentGen.method_comment('MakeHelperGenerator', 'print_make_helper')
        
        # ParserInfo parameter
        params = []
        if has_parser_info:
            params.append('struct ParserInfo I')
        
        # Add field constructor arguments
        params.extend(field_args)
        
        param_str = ', '.join(params) if params else 'void'
        
        # Return type
        if is_inline:
            return_type = union_name
            func_prefix = union_name[0].lower() + union_name[1:]
            func_name = f'{func_prefix}_{field_suffix}'
        else:
            return_type = f'{union_name} *'
            func_name = f'make{union_name}_{field_suffix}'
        
        # Function declaration
        print(f'static inline {return_type} {func_name}({param_str}) {{ {c}')
        
        # Build argument list for newFieldType call
        new_args = []
        if field_takes_parser_info:
            new_args.append('I')
        
        # Extract just the parameter names (strip types)
        for arg in field_args:
            # arg is like "LamExp *test" or "int size" - extract just "test" or "size"
            arg_name = arg.split()[-1].lstrip('*')
            new_args.append(arg_name)
        
        new_args_str = ', '.join(new_args) if new_args else ''
        
        # Generate the function body
        field_type = field_obj.getTypeDeclaration(catalog)
        print(f'    {field_type} x1 = new{field_type_name}({new_args_str}); {c}')
        print(f'    int save = PROTECT(x1); {c}')
        
        # Build argument list for union constructor call
        union_args = []
        if has_parser_info:
            union_args.append('I')
        union_args.append('x1')
        union_args_str = ', '.join(union_args)
        
        if is_inline:
            print(f'    {return_type} x2 = {func_prefix}_{field_suffix}({union_args_str}); {c}')
        else:
            print(f'    {return_type} x2 = new{union_name}_{field_suffix}({union_args_str}); {c}')
        
        print(f'    UNPROTECT(save); {c}')
        print(f'    return x2; {c}')
        print(f'}} {c}')
        print('')

    @staticmethod
    def _field_takes_parser_info(field_obj, catalog):
        """
        Check if the field type's constructor takes a ParserInfo parameter.
        
        Returns:
            bool: True if constructor includes ParserInfo parameter
        """
        if hasattr(field_obj, 'getNewSignature'):
            sig = field_obj.getNewSignature(catalog)
            return 'ParserInfo' in sig
        return False

    @staticmethod
    def _get_constructor_args(field_obj, catalog, has_parser_info):
        """
        Get the list of constructor arguments for a field type.
        
        Returns:
            List of argument strings like "LamExp *test", "int size", etc.
        """
        args = []
        
        # Use getNewSignature to extract constructor args
        # This works for all types: structs, arrays, vectors, hashes
        if hasattr(field_obj, 'getNewSignature'):
            sig = field_obj.getNewSignature(catalog)
            # Signature format: "TypeName * newTypeName(arg1, arg2, ...)"
            # Extract the part between ( and )
            start = sig.find('(')
            end = sig.find(')')
            if start != -1 and end != -1:
                args_str = sig[start+1:end].strip()
                if args_str and args_str != 'void':
                    # Split by comma and strip whitespace
                    args = [arg.strip() for arg in args_str.split(',')]
                    # Filter out ParserInfo if present (we handle that separately)
                    args = [arg for arg in args if 'ParserInfo' not in arg]
        
        return args
