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
        # Get configuration for struct names
        name = self.config.get('name', 'anf_kont')
        # Derive continuation struct name from config name (e.g., 'anf_kont' -> 'AnfKont')
        parts = name.split('_')
        kont_struct = ''.join(ucFirst(part) for part in parts if part != 'kont') + 'Kont'
        kont_env_union = ''.join(ucFirst(part) for part in parts if part != 'kont') + 'KontEnv'
        wrapper_type = kont_struct + 'ProcWrapper'
        
        # Build union data mapping keys to struct names
        union_data = {}
        
        for cont_name, spec in self.continuations.items():
            if not isinstance(spec, dict):
                continue
                
            struct_name = ucFirst(cont_name) + "KontEnv"
            key = spec.get('key')
            
            # Build struct fields from free_vars
            free_vars = spec.get('free_vars', {})
            struct_yaml = {
                'meta': {
                    'brief': spec.get('brief', f'Environment for {cont_name}'),
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
        catalog.add(DiscriminatedUnion(kont_env_union, union_yaml))
        
        # Add continuation struct with parameterized name
        catalog.add(SimpleStruct(kont_struct, {
            'meta': {
                'brief': f'Continuation Structure for {kont_struct} conversion',
            },
            'data': {
                'env': kont_env_union,
                'wrapper': wrapper_type
            }
        }))
        
    def generate_kont_impl_inc(self, output: TextIO, catalog: Catalog, includes) -> None:
        self._write_header(output, includes, 'inc')
        self._write_forward_decls(output, static=True)
        self._write_wrappers(output, static=True)
        self._write_constructors(output, catalog, static=True)
        self._write_trailer(output, 'inc')
    
    def _write_header(self, output: TextIO, includes, file_type='inc') -> None:
        name = self.config.get('name', 'kont')
        if file_type == 'inc':
            output.write(f"""/*
 * 
 * GENERATED FILE - DO NOT EDIT
 * 
 */

#ifndef {name}_impl_inc
#define {name}_impl_inc

#include "{name}.h"

""")
        elif file_type == 'h':
            output.write(f"""/*
 * 
 * GENERATED FILE - DO NOT EDIT
 * 
 */

#ifndef cekf_{name}_impl_h
#define cekf_{name}_impl_h

#include "{name}.h"

""")
        elif file_type == 'c':
            output.write(f"""/*
 * 
 * GENERATED FILE - DO NOT EDIT
 * 
 */

#include "{name}_impl.h"

""")
        if file_type != 'c':
            for include in includes:
                output.write(f'#include "{include}"\n')
            output.write("\n")
    
    def _write_forward_decls(self, output: TextIO, static=False) -> None:
        modifier = "static " if static else ""
        output.write("// Continuation function declarations\n")
        for name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            struct_name = ucFirst(name) + "KontEnv"
            output.write(f"{modifier}LamExp *{name}Kont(LamExp*, {struct_name}*);\n")
        output.write("\n")
    
    def _write_wrappers(self, output: TextIO, static=False) -> None:
        modifier = "static " if static else ""
        output.write("// Wrapper functions - invoke continuation with result\n\n")
        
        for name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            key = spec.get('key')
            
            # Generate wrapper function
            kont_env_union = self._get_kont_env_union_name()
            output.write(f"{modifier}struct LamExp *{name}Wrapper(")
            output.write(f"struct LamExp *exp, ")
            output.write(f"struct {kont_env_union} *env")
            output.write(") {\n")
            output.write(f"    return {name}Kont(exp, get{kont_env_union}_{ucFirst(key)}(env));\n")
            output.write("}\n\n")
    
    def _get_kont_struct_name(self, catalog: Catalog) -> str:
        """Find the main continuation struct (has 'wrapper' field)"""
        for obj_name in catalog.contents:
            obj = catalog.get(obj_name)
            if hasattr(obj, 'fields') and 'wrapper' in [f.name for f in obj.fields]:
                return obj_name
        return "AnfKont"  # fallback
    
    def _get_kont_env_union_name(self) -> str:
        """Derive the KontEnv union name from config (e.g., 'anf_kont' -> 'AnfKontEnv', 'cps_kont' -> 'CpsKontEnv')"""
        name = self.config.get('name', 'anf_kont')
        parts = name.split('_')
        return ''.join(ucFirst(part) for part in parts if part != 'kont') + 'KontEnv'
    
    def _getTypeDeclaration(self, typeName, catalog):
        obj = catalog.get(typeName)
        return obj.getTypeDeclaration(catalog)

    def _write_constructors(self, output: TextIO, catalog: Catalog, static=False) -> None:
        modifier = "static " if static else ""
        kont_struct = self._get_kont_struct_name(catalog)
        output.write("// Constructor functions - create continuation environments\n\n")
        
        for name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            key = spec.get('key', name)
            free_vars = spec.get('free_vars', {})
            
            # Generate constructor function
            output.write(f"{modifier}struct {kont_struct} *makeKont_{name}(\n")
            
            # Parameters from free_vars
            params = [f"    {self._getTypeDeclaration(var_type, catalog)} {var_name}" 
                     for var_name, var_type in free_vars.items()]
            output.write(",\n".join(params))
            if params:
                output.write("\n")
            output.write(") {\n")
            
            # Create environment struct
            kont_env_union = self._get_kont_env_union_name()
            output.write(f"    {kont_env_union} *__env = make{kont_env_union}_{ucFirst(key)}(")
            output.write(", ".join(free_vars.keys()))
            output.write(");\n")
            output.write("    int __save = PROTECT(__env);\n")
            output.write(f"    {kont_struct} *__kont = new{kont_struct}(__env, {name}Wrapper);\n")
            output.write("    UNPROTECT(__save);\n")
            output.write("    return __kont;\n")
            output.write("}\n\n")

    def _write_trailer(self, output: TextIO, file_type='inc') -> None:
        name = self.config.get('name', 'kont')
        if file_type == 'inc':
            output.write(f"\n#endif /* {name}_impl_inc */\n")
        elif file_type == 'h':
            output.write(f"\n#endif /* cekf_{name}_impl_h */\n")
    
    def generate_kont_impl_h(self, output: TextIO, catalog: Catalog, includes) -> None:
        """Generate public header file for continuation scaffolding"""
        self._write_header(output, includes, 'h')
        self._write_forward_decls(output, static=False)
        
        # Wrapper declarations
        kont_env_union = self._get_kont_env_union_name()
        output.write("// Wrapper function declarations\n")
        for cont_name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            output.write(f"struct LamExp *{cont_name}Wrapper(")
            output.write(f"struct LamExp *exp, ")
            output.write(f"struct {kont_env_union} *env);\n")
        output.write("\n")
        
        # Constructor declarations
        kont_struct = self._get_kont_struct_name(catalog)
        output.write("// Constructor function declarations\n")
        for cont_name, spec in self.continuations.items():
            if spec.get('notimplemented', False):
                continue
            free_vars = spec.get('free_vars', {})
            
            output.write(f"struct {kont_struct} *makeKont_{cont_name}(")
            params = [f"{self._getTypeDeclaration(var_type, catalog)} {var_name}" 
                     for var_name, var_type in free_vars.items()]
            output.write(", ".join(params))
            output.write(");\n")
        output.write("\n")
        
        self._write_trailer(output, 'h')
    
    def generate_kont_impl_c(self, output: TextIO, catalog: Catalog, includes) -> None:
        """Generate public C implementation file for continuation scaffolding"""
        self._write_header(output, includes, 'c')
        self._write_wrappers(output, static=False)
        self._write_constructors(output, catalog, static=False)