"""
Object Type Helper - Utilities for GC object type enumeration.

This module provides utilities for generating object type enumerations
used by the garbage collector to dispatch mark/free operations.
"""

class ObjectTypeHelper:
    """
    Utilities for generating GC object type identifiers.
    
    The CEKF garbage collector uses an object type enumeration to
    dispatch mark and free operations to the correct handler.
    """
    
    @staticmethod
    def obj_type_name(entity_name):
        """
        Generate the object type enum constant name.
        
        Args:
            entity_name: Name of the entity (e.g., "AstExpression")
            
        Returns:
            String like "OBJTYPE_ASTEXPRESSION"
        """
        return ('objtype_' + entity_name).upper()
    
    @staticmethod
    def obj_type_array(entity_name):
        """
        Generate an object type array with single element.
        
        Args:
            entity_name: Name of the entity
            
        Returns:
            List containing single object type constant
        """
        return [ObjectTypeHelper.obj_type_name(entity_name)]
