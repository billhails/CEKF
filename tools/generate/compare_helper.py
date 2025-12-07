"""
Compare Helper - Utilities for handling extra comparison arguments.

This module provides utilities for managing extra comparison function parameters
that some types need (e.g., comparing arrays may need size information).
"""

class CompareHelper:
    """
    Utilities for generating extra comparison function parameters.
    
    Some entity types can have additional arguments for their comparison functions
    defined via the extraCmpArgs dictionary in their YAML.
    """
    
    @staticmethod
    def get_extra_formal_args(extra_cmp_args, get_ctype_fn):
        """
        Generate formal parameter string for extra comparison arguments.
        
        Args:
            extra_cmp_args: Dictionary mapping arg names to type names
            get_ctype_fn: Function to convert type name to C type declaration
            
        Returns:
            String like ", Type1 arg1, Type2 arg2" or "" if no extra args
        """
        extra = []
        for name in extra_cmp_args:
            ctype = get_ctype_fn(extra_cmp_args[name])
            extra.append(f"{ctype}{name}")
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""
    
    @staticmethod
    def get_extra_actual_args(extra_cmp_args):
        """
        Generate actual argument string for extra comparison arguments.
        
        Args:
            extra_cmp_args: Dictionary mapping arg names to type names
            
        Returns:
            String like ", arg1, arg2" or "" if no extra args
        """
        extra = []
        for name in extra_cmp_args:
            extra.append(name)
        if len(extra) > 0:
            return ", " + ", ".join(extra)
        return ""
