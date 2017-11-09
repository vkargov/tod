#-------------------------------------------------------------------------------
# DWARF2Typedes
#
# Converts DWARF type information into its own typedes format
# Can also be used to inspect type information in binaries in a readable C-like format.
#
# Requires the pyelftools library to run:
# https://bitbucket.org/eliben/pyelftools/
#
# TODO: Currently the code is dirty and inefficient. A cleanup should be in order.
#
# Author: Vladimir Kargov
# Based on an example by Eli Bendersky (eliben@gmail.com) and heavily relying on his Pyelftools library.
#-------------------------------------------------------------------------------
from __future__ import print_function
import sys
import copy
import pdb
import re
import glob
import multiprocessing

# Add path to the pyelftools library
sys.path.extend(['chksys/sbo/pyelftools'])

from elftools.dwarf.constants import *
from elftools.common.py3compat import bytes2str
from elftools.elf.elffile import ELFFile
import elftools.dwarf.die
from elftools.dwarf.descriptions import (
    describe_DWARF_expr, set_global_machine_arch)
from elftools.dwarf.dwarf_expr import GenericExprVisitor

def process_file(dwarf_name, typedes_name):

    try:
        dwarf_file = open(dwarf_name, 'rb')

        if typedes_name:
            typedes_file = open(typedes_name, 'w')
        else:
            typedes_file = sys.stdout
    except IOError:
        print("Error: can't open file.")

    elffile = ELFFile(dwarf_file)

    if not elffile.has_dwarf_info():
        print('Error: File has no DWARF info')
        return 1

    # get_dwarf_info returns a DWARFInfo context object, which is the
    # starting point for all DWARF-based processing in pyelftools.
    dwarfinfo = elffile.get_dwarf_info()        

    for CU in dwarfinfo.iter_CUs():
        # DWARFInfo allows to iterate over the compile units contained in
        # the .debug_info section. CU is a CompileUnit object, with some
        # computed attributes (such as its offset in the section) and
        # a header which conforms to the DWARF standard. The access to
        # header elements is, as usual, via item-lookup.
        # print('// Found a compile unit at offset %s, length %s' % (
            # CU.cu_offset, CU['unit_length']))

        # Start with the top DIE, the root for this CU's DIE tree
        top_DIE = CU.get_top_DIE()
        # print('// Top DIE with tag=%s' % top_DIE.tag)

        # Each DIE holds an OrderedDict of attributes, mapping names to
        # values. Values are represented by AttributeValue objects in
        # elftools/dwarf/die.py
        # We're interested in the DW_AT_name attribute. Note that its value
        # is usually a string taken from the .debug_string section. This
        # is done transparently by the library, and such a value will be
        # simply given as a string.
        name_attr = top_DIE.attributes['DW_AT_name']

        # Only C supported! C++ is more complex and currently not supported
        if die_get_attr_val(top_DIE, 'DW_AT_language') not in [DW_LANG_C, DW_LANG_C89]:

            continue

        typedes_file.write('file %s\n' % bytes2str(name_attr.value))

        # if name_attr != 'sbo/com_so/elf.c':
        #     continue

        die_build_refs(top_DIE, {})

        # die_order_children(top_DIE)

        # Display DIEs recursively starting with top_DIE
        die_info_rec(dwarfinfo, typedes_file, top_DIE, False, print_pointers = False)

        die_del_dict_rec(top_DIE)

    return 0

def die_del_dict_rec(die):
    if 'attributes_dict' in die.__dict__:
        del die.attributes_dict
    for child in die.iter_children():
        die_del_dict_rec(child)


def die_get_attr_val(die, attr_name):
    if attr_name in die.attributes:
        return die.attributes[attr_name].value
    else:
        return None

def die_get_attr_form(die, attr_name):
    if attr_name in die.attributes:
        return die.attributes[attr_name].form
    else:
        return None

def die_get_attr_offset(die, attr_name):
    if attr_name in die.attributes:
        return die.attributes[attr_name].offset
    else:
        return None

def die_get_name_repr(die):
    attr_name = 'DW_AT_name'
    val = die_get_attr_val(die, attr_name)
    if val:
        return str(val)
    else:
        return '%unnamed_' + str(die.offset) + '%'

def die_get_byte_size(die):
    attr_name = 'DW_AT_byte_size'
    val = die_get_attr_val(die, attr_name)
    return val

def die_get_bit_size(die):
    attr_name = 'DW_AT_bit_size'
    val = die_get_attr_val(die, attr_name)
    return val

def die_get_const_value(die):
    attr_name = 'DW_AT_const_value'
    val = die_get_attr_val(die, attr_name)
    return val

def die_build_ref_arr_rec(die, refs):
    refs[die.offset - die.cu.cu_offset] = die
    for child in die.iter_children():
        die_build_ref_arr_rec(child, refs)

def die_build_refs_rec(die, refs):

    die.attributes_dict = {}

    for attribute_name in die.attributes:
        attribute_val = die.attributes[attribute_name]
        die.attributes_dict[attribute_name] = attribute_val._asdict()
        if attribute_val.form in ('DW_FORM_ref1', 'DW_FORM_ref2',
                                  'DW_FORM_ref4', 'DW_FORM_ref8'):
            die.attributes_dict[attribute_name]['die_ref'] = refs[attribute_val.value]

    for child in die.iter_children():
        die_build_refs_rec(child, refs)

def die_build_refs(top_die, refs):
    refs = {}
    die_build_ref_arr_rec(top_die, refs)
    die_build_refs_rec(top_die, refs)                

class TypeInfo:
    pass

def die_get_type(die):

# {is_ptr, is_ptrptr, type_name, array_indices_list, array_indices_str, elem_len}
# If no type is present, returns None
# Currently does not parse complex expressions completely
    if 'DW_AT_type' in die.attributes_dict:
        return die.attributes_dict['DW_AT_type']['die_ref']
    else:
        return None

# Get type information of 
# Returns a structure with the following fields
# {is_ptr, is_ptrptr, type_name, array_indices_list, array_indices_str, elem_len}
# If no type is present, returns None
# Currently does not parse complex expressions completely
# But even then should always be able to get all information important for typedump
def die_get_type_info(die):
    type_die = die_get_type(die)
    if type_die:
        type_info = TypeInfo()

        if type_die.tag == 'DW_TAG_pointer_type':
            type_info.is_ptr = True
        else:
            type_info.is_ptr = False

        if type_die.tag == 'DW_TAG_const_type':
            type_info.is_const = True
            type_die = die_get_type(type_die)
            if not type_die:
                return None
        else:
            type_info.is_const = False

        if type_die.tag == 'DW_TAG_array_type':
            type_info.array_indices_list = []
            type_info.array_indices_repr = ''
            arr_die = type_die
            type_die = arr_die.attributes_dict['DW_AT_type']['die_ref']
            for subrange_die in arr_die._children:
                if 'DW_AT_upper_bound' not in subrange_die.attributes_dict:
                    # incomplete array information
                    return None
                assert(subrange_die.tag == 'DW_TAG_subrange_type')
                upper_bound = subrange_die.attributes_dict['DW_AT_upper_bound']['value']
                assert('DW_AT_lower_bound' not in subrange_die.attributes_dict)
                type_info.array_indices_list.append(upper_bound + 1)
                type_info.array_indices_repr += '[' + str(upper_bound + 1) + ']'
        else:
            type_info.array_indices_repr = ''

        if type_die.tag == 'DW_TAG_pointer_type':
            type_info.name = 'PTR('+str(die_get_byte_size( type_die))+')'
            type_info.ptrptr = True
        else:
            type_info.ptrptr = False
            if type_die.tag == 'DW_TAG_enumeration_type':
                type_info.name = 'SCALAR('+str(die_get_byte_size( type_die))+')'
            elif type_die.tag == 'DW_TAG_subroutine_type':
                type_info.name = 'FUNC()'
            else:
                type_info.name = die_get_name_repr(type_die)

        if ' ' in type_info.name:
            type_info.name = '"' + type_info.name + '"'

        # if type_info.is_ptr:
        #     type_info.name = type_info.name + ' *'

        type_info.byte_size = die_get_byte_size( type_die)

        return type_info

    else:
        return None             # No type info available for die

def die_get_modtype(die):
    if re.match('.*_ref$', die_get_name_repr(die)):
        return 'REF'
    elif die.tag == 'DW_TAG_structure_type':
        return 'STRUCT'
    elif die.tag == 'DW_TAG_union_type':
        return 'UNION'
    elif die.tag in ('DW_TAG_base_type', 'DW_TAG_enumeration_type'):
        return 'SCALAR'
    elif die.tag == 'DW_TAG_pointer_type':
        return 'PTR'
    elif die.tag in ('DW_TAG_typedef', 'DW_TAG_member', 'DW_TAG_array_type', 'DW_TAG_const_type'):
        return die_get_modtype(die_get_type(die))
    elif die.tag == 'DW_TAG_subroutine_type':
        return 'FUNC'
    else:
        assert(0)

def bytestr(size):
    if size % 100 in (1, 21, 31, 41, 51, 61, 71, 81, 91):
        return '(%d byte)' % size
    else:
        return '(%d bytes)' % size

# Get full type size in bytes. If it is an array, the size of all elements will be returned.
# If it is a bit field, return the size of a storage unit containing the bit field.
def die_get_full_type_size(die):

    elem_num = 1

    byte_size = die_get_byte_size(die)
    if byte_size:
        assert(byte_size != 0)
        return byte_size

    type_die = die_get_type(die)

    if type_die:
        parent_type_size = die_get_full_type_size(type_die)

        if not parent_type_size:
            return None

        if type_die.tag == 'DW_TAG_array_type':
            for i in die_get_type_info(die).array_indices_list:
                elem_num = elem_num * i        
                
        return parent_type_size * elem_num

    return None

# Return appropriate size string for members and types.
def die_get_type_size_str(die, is_member = False):
    full_size = die_get_full_type_size(die)
    if not full_size:
        return '%no_size%'

    bit_size = die_get_bit_size(die)

    if is_member:
        if not bit_size:
            return ''
        else:
            return ' : ' + str(bit_size)
        
    else:
        assert(not bit_size)

        return str(full_size)
    

def die_info_rec(dwarfinfo, output_file, die, in_struct, indent_level = '', print_pointers = False):
    """ A recursive function for showing information about a DIE and its
        children.
    """

    children_in_struct = False
    children_in_enum = False
    child_indent = indent_level + '  '

    # # depth_key_func = lambda die: 1 + depth_key_func(die_get_type(die) if die_get_type(die) else 0)
    # if die.tag not in ['DW_TAG_structure_type', 'DW_TAG_union_type']:
    #     die._children = sorted( die._children, key = depth_key_func)

    if die.tag == 'DW_TAG_structure_type':
        assert(not in_struct)

        if die._children:
            output_file.write((indent_level + 'struct(%d) %s\n' + indent_level + '{\n') % (die_get_byte_size(die), die_get_name_repr(die)))
            children_in_struct = True

    elif die.tag == 'DW_TAG_union_type':
        assert(not in_struct)

        output_file.write((indent_level + 'union(%d) %s\n' + indent_level + '{\n') % (die_get_byte_size(die), die_get_name_repr(die)))
        children_in_struct = True

    elif die.tag == die.tag == 'DW_TAG_typedef':
        type_name = die_get_name_repr(die)
        basetype = die_get_type_info(die)
        if basetype:
            basetype_name = basetype.name

            # No need to re-typedef structre with same name (as if struct X typedefed as X)
            if basetype.name != type_name or basetype.array_indices_repr:
                output_file.write('%stypedef %s%s %s\n' % (indent_level, basetype.name, basetype.array_indices_repr, type_name))

    elif die.tag == 'DW_TAG_member':
        member_name = die_get_name_repr(die)

        type = die_get_type_info(die)

        # This should always be the case
        assert(type)

        if type.is_ptr:
            if not print_pointers:
                return
            # ptr_str = ' *'
            ptr_str = ''
            # comment_str = '// '
            comment_str = ''
        else:
            ptr_str = ''
            comment_str = ''

        if 'DW_AT_bit_offset' in die.attributes_dict:
            bit_offset = die.attributes_dict['DW_AT_bit_offset']['value']
            # TODO
            # (Very rarely) Some structures have a HUGE offset for some reason
            # Why? Can GCC be trusted? Maybe it's in some other represenation?
            # Are DW_FORM_data1 and DW_FORM_data8 different?
            # dwarfdump -a also shows the same number
            # if bit_offset > 100:
            #     pdb.set_trace()
            bit_offset_str = '.' + str(bit_offset)
        else:
            bit_offset = 0
            bit_offset_str = ''

        if 'DW_AT_data_member_location' in die.attributes_dict:
            # I must use GenericExprVisitor instead of this
            # This handles only a specific case
            # Which GDB appears to solely rely upon
            # When generating structure member offsets
            byte_offset = re.match('\(DW_OP_plus_uconst: (\d+)\)', describe_DWARF_expr(die.attributes_dict['DW_AT_data_member_location']['value'], dwarfinfo.structs)).group(1)
            total_offset = (int(byte_offset) * 8) + bit_offset
            offset_str = str(byte_offset + bit_offset_str + ' ')
        else:
            offset_str = ''

        output_file.write('%s%s%s%s%s %s%s%s%s\n' % (indent_level, comment_str, offset_str, type.name, ptr_str, member_name, type.array_indices_repr, die_get_type_size_str(die, is_member = True), ''))

    elif die.tag == 'DW_TAG_base_type':
        type_name = die_get_name_repr(die)
        type_size = die_get_byte_size(die)
        if ' ' in type_name:
            type_name = '"' + type_name + '"'

        output_file.write('%stypedef %s(%s) %s\n' % (indent_level, die_get_modtype(die), die_get_type_size_str(die), type_name))

    for child in die.iter_children():
        die_info_rec(dwarfinfo, output_file, child, children_in_struct, child_indent, print_pointers)
        
    if children_in_struct or children_in_enum:
        output_file.write(indent_level+'}\n')
    
import pdb, sys, traceback

# Uncomment to fall right into debugger on any error
# def info(type, value, tb):
#     traceback.print_exception(type, value, tb)
#     pdb.pm()
# sys.excepthook = info

if __name__ == '__main__':
    if len(sys.argv) == 2:
        result = process_file(sys.argv[1], None)
    elif len(sys.argv) == 4 and sys.argv[2] == '-o':
        result = process_file(sys.argv[1], sys.argv[3])
    else:
        result = 1
        print("""DWARF2Typedes - a tool for converting type information from the binary DWARF format into its own textual Typedes format.

Syntax:
  dwarf2typedes dwarf_file
or
  dwarf2typedes dwarf_file -o typedes_file

Where DWARF_FILE is the ELF file with DWARF information you want to analyze (a binary or an object file) and TYPEDES_FILE is the output file. In case the output is not specified, all information is printed to STDOUT.""")
    
    sys.exit(result)






