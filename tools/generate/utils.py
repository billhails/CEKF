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
Utility functions for code generation
"""

import sys
import datetime


def debug(*args, **kwargs):
    """Print debug messages to stderr"""
    print(*args, file=sys.stderr, **kwargs)


def pad(depth):
    """Print indentation for the given depth"""
    print("    " * depth, end='')


def printGpl(file, document):
    """Print GPL header comment"""
    now = datetime.datetime.now()
    print('/*')
    with open('docs/gpl.txt') as gpl:
        line = gpl.readline()
        while line:
            print(' * ', end='')
            print(line.replace('__YEAR__', str(now.year)), end='')
            line = gpl.readline()
    print(" *")
    print(f" * {document['config']['description']}")
    print(f" * Generated from {file} by tools/generate.py")
    print(" */")


def printSection(name):
    """Print a section header comment"""
    print("")
    print("/*")
    print(f" * {name}")
    print(" */")
    print("")
