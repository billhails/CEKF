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
makeast package - Modular code generator for C structures from YAML schemas.
"""

from .loader import Loader
from .utils import debug, pad, printGpl, printSection
from .catalog import Catalog
from .base import Base, EnumField
from .primitives import Primitive
from .fields import SimpleField, DiscriminatedUnionField
from .hashes import SimpleHash
from .enums import SimpleEnum, DiscriminatedUnionEnum
from .arrays import SimpleArray, SimpleStack, InlineArray
from .vectors import SimpleVector
from .structs import SimpleStruct
from .unions import DiscriminatedUnion, InlineDiscriminatedUnion, DiscriminatedUnionUnion

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
