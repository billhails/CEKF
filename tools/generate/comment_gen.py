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
Comment and documentation generation utilities.

This module provides utilities for generating consistent C-style comments
throughout the generated code. This is the first step in the code generation
refactoring to reduce duplication.
"""


class CommentGen:
    """Generate C-style comments and documentation"""
    
    @staticmethod
    def method_comment(class_name, method_name):
        """
        Generate standard class.method comment.
        
        This is used throughout the codebase to mark which class/method
        generated a particular section of code for debugging purposes.
        
        Args:
            class_name: Name of the class (e.g., "SimpleStruct")
            method_name: Name of the method (e.g., "printNewFunction")
            
        Returns:
            String like "// SimpleStruct.printNewFunction"
        """
        return f"// {class_name}.{method_name}"
