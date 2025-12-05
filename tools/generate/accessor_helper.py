"""
Accessor Helper - Centralized field accessor generation.

This module provides utilities for generating C field access operators
that handle both inline (call-by-value) and pointer-based structs.
"""

class AccessorHelper:
    """
    Utilities for generating C field access operators.
    
    Inline structs use '.' operator (call-by-value, on stack)
    Normal structs use '->' operator (pointer-based, on heap)
    """
    
    @staticmethod
    def accessor(is_inline):
        """
        Return the appropriate field accessor operator.
        
        Args:
            is_inline: True for inline structs (.), False for pointer structs (->)
            
        Returns:
            '.' for inline structs, '->' for normal structs
        """
        return '.' if is_inline else '->'
