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
generate package - Modular code generator for C structures from YAML schemas.
"""

from .loader import Loader
from .utils import debug, pad, printGpl, printSection
from .catalog import Catalog
from .base import Base
from .enum_field import EnumField
from .primitives import Primitive
from .simple_field import SimpleField
from .discriminated_union_field import DiscriminatedUnionField
from .hashes import SimpleHash
from .simple_enum import SimpleEnum
from .discriminated_union_enum import DiscriminatedUnionEnum
from .simple_array import SimpleArray
from .simple_stack import SimpleStack
from .inline_array import InlineArray
from .vectors import SimpleVector
from .simple_struct import SimpleStruct
from .inline_struct import InlineStruct
from .discriminated_union import DiscriminatedUnion
from .inline_discriminated_union import InlineDiscriminatedUnion
from .discriminated_union_union import DiscriminatedUnionUnion
from .inline_discriminated_union import InlineDiscriminatedUnion
from .discriminated_union_union import DiscriminatedUnionUnion

__all__ = [
    'Loader',
    'utils',
    'Catalog',
    'Base',
    'EnumField',
    'Primitive',
    'SimpleField',
    'DiscriminatedUnionField',
    'SimpleHash',
    'SimpleEnum',
    'DiscriminatedUnionEnum',
    'SimpleArray',
    'SimpleStack',
    'InlineArray',
    'SimpleVector',
    'SimpleStruct',
    'DiscriminatedUnion',
    'InlineDiscriminatedUnion',
    'DiscriminatedUnionUnion',
]
