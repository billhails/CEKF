"""
Continuation scaffolding generator for ANF normalization.

Generates environment structs, discriminated union, and static implementation
scaffolding for continuation-passing style ANF conversion.
"""

from typing import Dict, List, Any, TextIO
from .catalog import Catalog
from .simple_struct import SimpleStruct
from .discriminated_union import DiscriminatedUnion
from .primitives import Primitive

def ucFirst(s: str) -> str:
    if not s:
        return s
    return s[0].upper() + s[1:]

class KontinuationGenerator:
    def __init__(self, yaml_data: Dict[str, Any]):
        self.config = yaml_data.get('config', {})
        self.external = yaml_data.get('external', {})
        # Continuations are nested under 'continuations' key
        self.continuations = yaml_data.get('continuations', {})
    
    def populate_catalog(self, catalog: Catalog) -> None:
        # Build union data mapping keys to struct names
        union_data = {}
        
        for name, spec in self.continuations.items():
            if not isinstance(spec, dict):
                continue
                
            struct_name = ucFirst(name) + "KontEnv"
            key = spec.get('key')
            
            # Build struct fields from free_vars
            free_vars = spec.get('free_vars', {})
            struct_yaml = {
                'meta': {
                    'brief': spec.get('brief', f'Environment for {name}'),
                    'description': spec.get('context', '')
                },
                'data': free_vars
            }

            # Add struct to catalog
            catalog.add(SimpleStruct(struct_name, struct_yaml))
            
            # Add to union mapping
            union_data[key] = struct_name
        
        # Add discriminated union to catalog
        union_yaml = {'data': union_data}
        catalog.add(DiscriminatedUnion('KontEnv', union_yaml))
        catalog.add(SimpleStruct('AnfKont', {
            'meta': {
                'brief': 'Continuation Structure for ANF conversion',
            },
            'data': {
                'env': 'KontEnv',
                'wrapper': 'AnfKontProcWrapper'
            }
        }))
        
    def generate_kont_impl_inc(self, output: TextIO, catalog: Catalog, includes) -> None:
        self._write_header(output, includes)
        self._write_forward_decls(output)
        self._write_wrappers(output)
        self._write_constructors(output, catalog)
        self._write_trailer(output)
    
    def _write_header(self, output: TextIO, includes) -> None:
        output.write("""/*
 * 
 * GENERATED FILE - DO NOT EDIT
 * 
 */

#ifndef anf_kont_impl_inc
#define anf_kont_impl_inc

#include "anf_kont.h"

""")
        for include in includes:
            output.write(f'#include "{include}"\n')
        output.write("\n")
    
    def _write_forward_decls(self, output: TextIO) -> None:
        output.write("// Forward declarations\n")
        for name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            struct_name = ucFirst(name) + "KontEnv"
            output.write(f"static LamExp *{name}Kont(LamExp*, {struct_name}*);\n")
        output.write("\n")
    
    def _write_wrappers(self, output: TextIO) -> None:
        output.write("// Wrapper functions - invoke continuation with result\n\n")
        
        for name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            key = spec.get('key')
            
            # Generate wrapper function
            output.write(f"static struct LamExp *{name}Wrapper(")
            output.write(f"struct LamExp *exp, ")
            output.write(f"struct KontEnv *env")
            output.write(") {\n")
            output.write(f"    return {name}Kont(exp, getKontEnv_{ucFirst(key)}(env));\n")
            output.write("}\n\n")
    
    def _getTypeDeclaration(self, typeName, catalog):
        obj = catalog.get(typeName)
        return obj.getTypeDeclaration(catalog)

    def _write_constructors(self, output: TextIO, catalog: Catalog) -> None:
        output.write("// Constructor functions - create continuation environments\n\n")
        
        for name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            key = spec.get('key', name)
            free_vars = spec.get('free_vars', {})
            
            # Generate constructor function
            output.write(f"static struct AnfKont *makeKont_{name}(\n")
            
            # Parameters from free_vars
            params = [f"    {self._getTypeDeclaration(var_type, catalog)} {var_name}" 
                     for var_name, var_type in free_vars.items()]
            output.write(",\n".join(params))
            if params:
                output.write("\n")
            output.write(") {\n")
            
            # Create environment struct
            output.write(f"    KontEnv *__env = makeKontEnv_{ucFirst(key)}(")
            output.write(", ".join(free_vars.keys()))
            output.write(");\n")
            output.write("    int __save = PROTECT(__env);\n")
            output.write(f"    AnfKont *__kont = newAnfKont(__env, {name}Wrapper);\n")
            output.write("    UNPROTECT(__save);\n")
            output.write("    return __kont;\n")
            output.write("}\n\n")

    def _write_trailer(self, output: TextIO) -> None:
        output.write("\n#endif /* anf_kont_impl_inc */\n")