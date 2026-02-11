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
YAML loader with !include support
"""

import yaml
import os


class Loader(yaml.SafeLoader):
    """YAML loader that supports !include directive for file inclusion"""

    _visited_files = set()

    def __init__(self, stream):
        self._root = os.path.split(stream.name)[0]
        super(Loader, self).__init__(stream)

    def include(self, node):
        fileName = os.path.join(self._root, self.construct_scalar(node))
        if fileName in self._visited_files:
            return {}
        self._visited_files.add(fileName)
        with open(fileName, 'r') as f:
            return yaml.load(f, Loader)


Loader.add_constructor('!include', Loader.include)
