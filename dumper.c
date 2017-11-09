#include "impl.h"

/**
 * File:
 *
 * Implementation of the typed object dumper/loader (TOD).
 */

/**
 * Module: Typed object dumper.
 *
 * Description:
 *   The type object dumper is a facility allowing the programmer to
 *   save arbitrary memory object(s) on a disk and load them back in memory.
 *
 *   There are a few examples where this facility may be especially useful:
 *
 *   - Faster debugging:
 *     Saving full compiler state during debugging and loading it back if the long
 *     compiler run fails. (Currently NOT working since not
 *     all compiler structures are saved, but saving the entire compiler state is
 *     the ultimate goal).
 *   - Compiler state comparison, "free" log files:
 *     Comparing text object dumps of the same objects on different compiler versions
 *     as a debugger facility. If some objects or their data field are different,
 *     then compilation process went differently which may suggest the place where the
 *     error takes place.
 *     Unlike log files that require time to implement and become obsolete when changes
 *     to structures are made, TOD are "free" since the user needs only to set the pool's
 *     type before being able to dump its objects.
 *   - Saving specific structures between compiler runs for further use/inspection:
 *     The first user of this package is the dynamic feedback collector which uses it
 *     to inspect IR information (CF graph, operation properties, etc.)
 *     from different compiler runs.
 *
 * Obtaining type information from the compiler:
 *   The way TOD knows all information about all type definitions inside the compiler
 *   is by analysing the debugging information put into the binary
 *   by the compiler the binary is built with. Currently the information is not analysed directly.
 *   Rather, an external python script is used (dwarf_show_types.py)
 *   to convert that binary information into a convenient text representation.
 *
 *   Currently, the place where the parsed type description file is put is the directory
 *   where the binaries reside (e.g. for bin/X the parsed file will be bin/X.typedes)
 *   TOD can later process the typedes file using the TodLoadTypeInfo() function.
 *   The type information collection script should be run as an additional build step
 *   before the program is run.
 *
 *   At this moment, only the DWARF debugginf format is supported by the script which limits
 *   usage of TOD to GCC, ICC and LLVM. The Microsoft C compiler is NOT supported.
 *   In order to support Visual Studio builds, support for Microsoft's debug interface access
 *   (DIA, http://msdn.microsoft.com/en-us/library/x93ctkx8%28v=vs.71%29.aspx) should be added.
 *   An alternative solution would be generating type information in the format of a C header
 *   file which MsC would be able to work with directly without the need of a script.
 *
 *   Note that the typedes file should be rebuilt every time a change in one of the
 *   dumped structures takes place.
 *
 * Identifying the type of a union type/field:
 *   It is impossible to identify the type of a union without additional information.
 *   In order to address this, determinant functions (MemDeterminant) were introduced.
 *   A determinant function is a user-defined function the gives the textual name of
 *   a field type based on the offset and name path depth (see below), thus resolving
 *   the uncertainty. Note that determinants are needed only for types that have
 *   union field directly or via a structure-field. In addition, if no determinant is
 *   specified for such structure, all other fields may be dumped anyway.
 *   Examples may be found by looking for functions following the ".*_.*Determinant" regex
 *   pattern. const_ConstDeterminant should be a good example for a start.
 *
 *   Since determinant functions are part of the semantical representation of the program,
 *   their information is also saved during object dump. In order to make the compiler
 *   know about the function, it should be registered in the mem package with
 *   MemRegisterDeterminant().
 *
 * Specifying type information inside memory pools:
 *   Not all pools used by the compiler contain one pre-defined type. Some pools,
 *   for example, may contain two overlapped types (HashHash with a structure as the key).
 *   Some other pools may have several instances of one type (e.g. the bunches pool used by
 *   the hash package that contains several MemEntry* instances).
 *
 *   All information about the structure of the data entry is specified during pool
 *   creation by setting appropriate values of the MemPool structure, directly or indirectly.
 *   The seven fields describe the function format:
 *   {{main/embedded}_entry_{type,determinant,quantity}} {and embedded_entry_start_field}.
 *   The layout of the general-case meta-type is as follows:
 *   A meta-type kept in a pool consists of {main_determinant_quantity} identical entries.
 *   One such entry has its main part of type {main_entry_type}
 *   and {embedded_determinant_quantity} embedded types {embedded_entry_type}
 *   starting at field {embedded_entry_start_field} of the main structure.
 *
 *   The most frequent case (1 main entry, 0 embedded entries) is pre-set during pool
 *   creation, so the only thing needed is to make one call:
 *   MemSetPoolMainSubentryType( TYPE( your_structure_type)) in order to be able to
 *   dump your pool. Other examples of usage can be found by looking up the
 *   TYPE macro: ir_New, const_Init or HashInit, to name the few.
 *
 *   In order to avoid dependence from the TOD package (memory package should not
 *   know about it), all types are specified as their ASCII names with the TYPE()
 *   macro. Example: TYPE( ir_IR) describes an IR type inside the memory manager.
 *
 * List of terms:
 *
 *   Main terms:
 *
 *   MEMORY OBJECT is data kept in a memory pool entry.
 *   DUMPING, also SAVING is the process of saving a typed memory object (referenced by
 *     MemEntry* or its derivatives) into a file.
 *   RESTORING, also LOADING - the process of restoring previously saved objects from
 *     a file into memory.
 *   TOD is a typed object dumper/loader.
 *
 *   Supplementary terms:
 *
 *   ALIAS is a type produced by the typedef keword (e.g. ir_IR* is an alias of
 *     MemEntry* which, in turn, is an alias of some other type).
 *   CTYPE is a representation of an element of a type inside TOD. All types are represented
 *     with one or several interconnected CTYPE structures.
 *   DETERMINANT is a user-defined function of type MemDeterminant giving a textual name
 *     of a field type based on the offset and name path depth.
 *     For an example see const_ConstDeterminant or other .*_.*Determinant functions.
 *   NAME PATH is a headless list of all components specifying the
 *     full path to an entry.
 *     Example: {ir}->{ids}->{8} for the eight element of the ids field of object ir (ir_IR).
 *   REPRESENTATIVE ENTRIES for pools.
 *     Currently there seems to be no way to keep pool references as hash or map values.
 *     A workaround is to allocate empty representative objects for each pool we want to
 *     remember and keep references to them. References to pools can then be obtained with
 *     MemGetEntryPool. This workaround was proposed by R. Muslinov and S. Scherbinin.
 *
 * Expected workflow for object save:
 *   TodSaveObjects( "file_name.tod", main_object);
 *
 * Expected workflow for object load:
 *   main_object = TodLoadObjects( "file_name.tod", &pools);
 *   // work with loaded objects
 *   // ...
 *   TodDeletePoolList( pools); // Optional. Pools may be deleted manually.
 *
 * TODO list:
 * - Support attributes.
 * - Support the rest of compiler structures, such as dynamic/bit arrays
 *   and strings(str_Buff).
 * - Proper auto documentation.
 * - Use the string package instead of direct string operation within the parser.
 * - Support the Microsoft C compiler bundled with MSVC.
 *
 * FAQ
 * Q: Why was an external script used to parse DWARF information and generate typedes files as opposed to making an internal framework with LIBDWARF?
 * A: Such solution seems like a better option, but it was ruled out as too time consuming to implement (working with DWARF with libdwarf is hard). In the future this should probably be done.
 *
 * Q: Why was Python chosen as the scripting language and not Perl?
 * A: Why not? Python has been used in the project before as well, currently there are 100+ Python scripts in the project.
 *    Python has a nice library providing high level interface to the DWARF structures which is currently used (pyelftools). Python also has a few alternatives, including a well-polished binding to libelf. Perl, on the contrary, does not seem to have a convenient and reliable library.
 *
 * Q: Why not analyze the sources directly to find out all the types?
 * A: This would be the best way of doing things for obvious reasons. However, the only way to ensure that structures are parsed the same way they are parsed by the compiler AND that all macro expansions are done the same way (remember that macro expansions may affect structure layout and their fields) is to use exactly the same parser and the same macro processor the compiler does. And GCC which is considered as the target compiler does not give an easy way of processing the post-processed code. However, all major compilers except can provide debug information which can then be analyzed to extraction type information from it.
 *
 * Q: Why not make program analyze itself?
 * A: This a possible improvement for later. The major reason why it hasn't been done by now is that it requires writing a relatively complex analysing procedure in C using the libelf library to access the DWARF file. This requires time.
 *
 * Q: What is the field depth level option in determinant functions? Why is it needed?
 * A: This option specifies the level of the field we want to know the type of. Consider the example:
 *   typedef union
 *   {
 *      int j;
 *      unsigned n;
 *   } emb_u_t;
 *
 *   typedef union
 *   {
 *      void * p;
 *      emb_u_t emb_u;
 *   } u_t;
 *
 * Suppose I want to call the determinant function on an object u_obj of type u_t to know whether the union u_obj.emb_u is int (j) or unsigned (n) at this moment. These two fields are located at offset 0, so we would assume that it's sufficient to provide only the field offset to the determinant function:
 * u_Determinant( object = u_obj, offset = 0)
 * in order to know the type of the field in interest. However, field p is also located at offset 0, although we are not interested in it. In order to specify that we are asking "is emb_u of type in or unsigned int" and not "is u_obj of type void * or emb_u_t?" we need an additional field specifying the depth of the structure in interest. Here the proper call answer the question "j or n?" will be:
 * u_Determinant( u_obj, offset = 0, depth = 1)
 * and the proper call go get the answer to "p or emb_u?" will be:
 * u_Determinant( u_obj, offset = 0, depth = 0)
 *
 */

/**
 * Controls whether to add additional unnecessary information (field memory offsets,
 * object types for each object) in the text dump.
 */
#define TOD_DEBUG_PRINT
/*
 * Reach private external object fields by manually type casting their references where
 * there are no access functions. This is used only for debugging.
 */
#define TOD_REACH_PRIVATE_EXTERNAL_FIELDS
/** Length of one indentation level in spaces. */
#define TOD_INDENT_LENGTH 2
/** The name of the current compiler's binary file. */
#define TOD_COMPILER_NAME_STR CONVERT_TO_STRING( COMPILER_NAME)
/** The name of the typedes file associated with the current binary */
#define TOD_TYPEDES_NAME_STR TOD_COMPILER_NAME_STR ".typedes"

/** Create a few shortcuts for frequently called functions */
#define STRCMP( str1, str2) CompareStrings( str1, str2)
#define STRLEN( str) StringLength( str)
#define ATOU( u) TodStr2Unsigned( u)

/** Different classes of TodCType */
typedef enum
{
    TOD_INVALID,
    TOD_SCALAR,
    TOD_ALIAS,
    TOD_REFERENCE,
    TOD_STRUCT,
    TOD_UNION,
    TOD_FUNCTION,
    TOD_POINTER
} TodType;

/**
 * Types of a name path entry.
 */
typedef enum
{
    TOD_INDEX,
    TOD_FIELD_NAME_REF,
    TOD_FIELD_NAME_PTR,
    TOD_POSTFIX_NUM
} TodNamePathType;

typedef enum
{
    TOD_REF_NULL = -1,
    TOD_REF_NO_TYPE_INFO = -2,
    TOD_REF_DELETED = -3,
    TOD_REF_INVALID = -4
} TodReferenceStatus;

typedef MemEntry* TodString*;

typedef struct {int byte; int bit;} TodBBPair;

typedef MemEntry* TodCType*;

/**
 * A component describing one element of a type. Basically, this is more or less a simplified
 * adaptation of the DIE structure of the DWARF format.
 */
typedef struct
{
    /** Element name */
    TodString* name;
    /** Element type */
    TodType type;
    /**
     * For an alias type either the reference to the originating type (if processed) or
     * a string identifying said type (unprocessed yet). When type loading is finished,
     * all types should be processed.
     */
    union
    {
        TodCType* originating_type;
        TodString* originating_type_name;
    } type_specific;
    /**
     * Byte size of a value (non-bitfield) or byt size of the full storage
     * element (bitfield).
     */
    int byte_size;
    /** Bit size of a value. Non-zero only for bit fields. */
    int bit_size;
    /** Byte offset starting from the beginning of the structure. Non-zero only for fields. */
    int byte_offset;
    /** Bit offset of a value. Non-zero only for bit fields. */
    int bit_offset;
    /** List of indices. For example, for a[12][20][2] it will be {12}->{20}->{2}. */
    ListUnit* indices;
    /** First subfield. Non-zero only for structure and union elements. */
    TodCType* first_subfield;
    /** Next subfield. Non-zero only for fields. */
    TodCType* next_field;
} TodCType;

/*
 * Global information. Contains type information loaded in the early stages of compilation.
 */
typedef struct
{
    /* Filename <=> Local mapx */
    MapMap* file2local;
    /* String pool */
    MemPool* str_pool;
    /* C type pool */
    MemPool* c_type_pool;
    /* List type pool */
    MemPool* Listpool;
    /* Object file name */
    char * obj_dump_name;
    /* List of allocated pools and data associated with them */
    ListList* type_pool_rel;
    /* WHAT IS THIS WHAT IS THIS????? */
    MEM_DECL_DBG_TYPE_ID
} TodGlobalInfo;

typedef MemEntry* TodGlobalInfo*;

/** Toad check type number */
#define TOD_INFO_DBG_TYPE_ID             0xDAB1670AD


/** Field access functions follow */

TodCType *
TodGetCTypePtr( TodCType* ctype)
{
    return (TodCType *)MemGetEntryPtr( ctype);
}

TodString*
TodGetCTypeName( TodCType* ctype)
{
    return TodGetCTypePtr( ctype)->name;
}

void
TodSetCTypeName( TodCType* ctype,
                  TodString* name)
{
    TodGetCTypePtr( ctype)->name = name;
}

/** Get the type (enum) of a ctype */
TodType
TodGetCTypeType( TodCType* ctype)
{
    return TodGetCTypePtr( ctype)->type;
}

void
TodSetCTypeType( TodCType* ctype,
                  TodType type)
{
    TodGetCTypePtr( ctype)->type = type;
}

TodCType*
TodGetCTypeOriginatingType( TodCType* ctype)
{
    /** Only alias elements should have access to this field. */
    assert( TodGetCTypeType( ctype) == TOD_ALIAS);
    return TodGetCTypePtr( ctype)->type_specific.originating_type;
}

void
TodSetCTypeOriginatingType( TodCType* ctype,
                             TodCType* originating_type)
{
    /** Only alias elements should have access to this field. */
    assert( TodGetCTypeType( ctype) == TOD_ALIAS);
    TodGetCTypePtr( ctype)->type_specific.originating_type = originating_type;
}

TodString*
TodGetCTypeOriginatingTypeName( TodCType* ctype)
{
    /** Only alias elements should have access to this field. */
    assert( TodGetCTypeType( ctype) == TOD_ALIAS);
    return TodGetCTypePtr( ctype)->type_specific.originating_type_name;
}

void
TodSetCTypeOriginatingTypeName( TodCType* ctype,
                                 TodString* originating_type_name)
{
    /** Only alias elements should have access to this field. */
    assert( TodGetCTypeType( ctype) == TOD_ALIAS);
    TodGetCTypePtr( ctype)->type_specific.originating_type_name = originating_type_name;
}

int
TodGetCTypeByteSize( TodCType* ctype)
{
    /**
     * The field should not be zero.
     * (This also means the function cannot be called on aliases.)
     */
    assert( TodGetCTypePtr( ctype)->byte_size > 0);

    return TodGetCTypePtr( ctype)->byte_size;
}

void
TodSetCTypeByteSize( TodCType* ctype,
                      int byte_size)
{
    TodGetCTypePtr( ctype)->byte_size = byte_size;
}

int
TodGetCTypeBitSize( TodCType* ctype)
{
    assert( TodGetCTypePtr( ctype)->bit_size >= 0);

    return TodGetCTypePtr( ctype)->bit_size;
}

void
TodSetCTypeBitSize( TodCType* ctype,
                     int bit_size)
{
    TodGetCTypePtr( ctype)->bit_size = bit_size;
}

int
TodGetCTypeByteOffset( TodCType* ctype)
{
    assert( TodGetCTypePtr( ctype)->byte_offset >= 0);

    return TodGetCTypePtr( ctype)->byte_offset;
}

void
TodSetCTypeByteOffset( TodCType* ctype,
                        int byte_offset)
{
    TodGetCTypePtr( ctype)->byte_offset = byte_offset;
}

int
TodGetCTypeBitOffset( TodCType* ctype)
{
    assert( TodGetCTypePtr( ctype)->bit_offset >= 0);

    return TodGetCTypePtr( ctype)->bit_offset;
}

void
TodSetCTypeBitOffset( TodCType* ctype,
                       int bit_offset)
{
    TodGetCTypePtr( ctype)->bit_offset = bit_offset;
}

ListUnit*
TodGetCTypeIndices( TodCType* ctype)
{
    return TodGetCTypePtr( ctype)->indices;
}

void
TodSetCTypeIndices( TodCType* ctype,
                     ListUnit* indices)
{
    TodGetCTypePtr( ctype)->indices = indices;
}

TodCType*
TodGetCTypeFirstSubfield( TodCType* ctype)
{
    assert( TodGetCTypeType( ctype) != TOD_ALIAS);
    return TodGetCTypePtr( ctype)->first_subfield;
}

void
TodSetCTypeFirstSubfield( TodCType* ctype,
                           TodCType* first_subfield)
{
    assert( TodGetCTypeType( ctype) != TOD_ALIAS);
    TodGetCTypePtr( ctype)->first_subfield = first_subfield;
}

TodCType*
TodGetCTypeNextField( TodCType* ctype)
{
    return TodGetCTypePtr( ctype)->next_field;
}

void
TodSetCTypeNextField( TodCType* ctype,
                       TodCType* next_field)
{
    TodGetCTypePtr( ctype)->next_field = next_field;
}

/**
 * Find the last field in a structure by its arbitrary field
 */
TodCType*
TodFindCTypeLastField( TodCType* ctype)
{
    while ( MemIsNotRefNull( TodGetCTypeNextField( ctype)) )
        ctype = TodGetCTypeNextField( ctype);
    return ctype;
}

/**
 * Access to package-local string representation. Probably should be nuked in favour of
 * the STR package. TODO.
 */
char *
TodGetString( MemEntry* str)
{
    return (char *)MemGetEntryPtr( str);
}

/**
 * Print to file. Local for package, open for tweaks.
 */
Size
TodFilePrintF( File file, /** file */
                 const char *format, /** message */
                 ...) /** arguments */
{
    Size size;
    VAList va;
    VAStart( va, format);
    if ( file == FileNull )
    {
        StmPrintFUseVA( STREAM_STDERR, format, va);
        size = 0;
    }
    else
        size = FilePrintFUseVA( file, format, va);
    VAEnd( va);
    return size;
}

/**
 * Print to file with indentation.
 */
Size
TodFilePrintFIndented( File file, /** file */
                        unsigned indent_level, /** indentation level */
                        const char *format, /** message */
                        ...) /** arguments */
{
    Size size;
    VAList va;
    VAStart( va, format);
    size = TodFilePrintF( file, "%*s", indent_level * TOD_INDENT_LENGTH, "");
    size += FilePrintFUseVA( file, format, va);
    VAEnd( va);
    return size;
}

/**
 * Print type name to a file.
 */
void
TodPrintTypeNameStr( File file,     /** file name */
                      TodCType* c_type) /** ctype */
{
    TodType type = TodGetCTypeType( c_type);

    if ( type == TOD_SCALAR )
        TodFilePrintF( file, " SCALAR(%d)", TodGetCTypeByteSize( c_type));
    else if ( type == TOD_ALIAS)
    {
        char * alias = TodGetString( TodGetCTypeName( TodGetCTypeOriginatingType( c_type)));
        TodFilePrintF( file, " %s", alias);
    }
    else if ( type == TOD_POINTER )
        TodFilePrintF( file, " PTR(%d)", TodGetCTypeByteSize( c_type));
    else if ( type == TOD_FUNCTION )
        TodFilePrintF( file, " FUNC()");
    else if ( type == TOD_STRUCT || type == TOD_UNION )
        ;
    else
    {
        assert( 0);
    }
}

/** Print indices list to a file. */
void
TodPrintIndices( File file,     /** file name */
                  ListUnit* member) /** indices */
{
    while ( MemIsNotRefNull( member) )
    {
        TodFilePrintF( file, "[%d]", (int) ListGetClientData( member));
        member = ListNext( member);
    }
}

/**
 * Find the number of elements in a multidimensional array specified by a list
 * of its indices.
 */
int
TodFindElemNum( ListUnit* indices)
{
    int elem_num = 1;

    while ( MemIsNotRefNull( indices) )
    {
        elem_num *= ListGetClientData( indices);
        indices = ListNext( indices);
    }

    return elem_num;
}

/**
 * Get info type pointer without type checking
 */
#define TOD_GET_GLOBALINFO_PTR( name) ( TodGlobalInfo*)MemGetEntryPtr(name)

static TodGlobalInfo global_info_static;

/**
 * Returns: pointer to TodGlobalInfo
 */
inline TodGlobalInfo *
TodGetGlobalInfoPtr( void )
{
    return & global_info_static;
}

/** Duplicate string in non-manageable memory */
/*
 * TODO: All call to this function should be eliminated in favour of allocating memory
 * directly by in memory manager upon pool creation. Or something.
 */
char *
TodDuplicateString( char * s)
{
    MemPool* str_pool;
    char * new_s;

    context_Switch2Shared();
    str_pool = TodGetGlobalInfoPtr()->str_pool;
    MemLockPool( str_pool);
    new_s = MemGetEntryPtr( MemNewFloatEntry( str_pool, StringLength( s) + 1));
    MemUnlockPool( str_pool);
    context_Switch2Local();
    return CopyString( new_s, s);
}

/**
 * Create a string resulting from concatenation of several parts in the global
 * string pool.
 */
MemEntry*
TodNewConcatString( char ** str_arr,
                     int str_num)
{
    int i;
    int total_str_len = 0;
    MemEntry* entry;
    char * new_str;

    if ( str_num == -1 )
    {
        str_num = 0;
        for ( i = 0; str_arr[i] != NULL; i++, str_num++ );
    }

    for ( i = 0; i < str_num; i++ )
    {
        total_str_len += STRLEN( str_arr[i]);
    }

    /* Include whitespace + \0 */
    total_str_len += str_num;

    MemLockPool( TodGetGlobalInfoPtr()->str_pool);
    entry = MemNewFloatEntry( TodGetGlobalInfoPtr()->str_pool, total_str_len);
    MemUnlockPool( TodGetGlobalInfoPtr()->str_pool);

    new_str = MemGetEntryPtr( entry);

    for ( i = 0; ; )
    {
        CopyMemory( new_str, str_arr[i], STRLEN( str_arr[i]));
        new_str += STRLEN( str_arr[i]);
        i++;
        if ( i < str_num )
            *new_str++ = ' ';
        else
        {
            *new_str = '\0';
            break;
        }
    }

    return entry;
}

/** Create new string in the global string pool. */
MemEntry*
TodNewString( char * str)
{
    return TodNewConcatString( &str, 1);
}

char *
TodGetLine( File f)
{
    int bufsize;
    int pos;
    char * line;
    int c;

    if ( IsFileEOF( f) )
        return NULL;

    for ( pos = 0, bufsize = BUFSIZ, line = MemMalloc( bufsize), c = FileGetChar( f);
          c != '\n' && c != '\0' && c != EOF;
          c = FileGetChar( f), pos++)
    {

        if ( pos == bufsize )
        {
            bufsize *= 2;
            line = MemRealloc( line, bufsize);
        }

        line[pos] = c;
    }

    line[pos] = '\0';

    return line;
}

/**
 * Create a null-terminated array of pointers to words constituting a given line.
 */
char **
TodSplitLineToWords( char * line,              /** Line to split. */
                      Bool split_on_dot) /** Consider the dot character a word splitter. */
{
    int line_len = STRLEN(line) + 1;
    char * word = NULL, * line_copy;
    char ** words;
    int line_pos, line_copy_pos, word_len, word_num;
    enum { OUTSIDE, IN_PLAINSTR, IN_QUOTE, IN_BRACKETS, AFTER_BRACKETS } state;

    if ( line_len <= 1 )
        return NULL;

    /* satisfying the compiler's complaints  */
    word_len = -1;
    state = OUTSIDE;

    words = (char **) MemMalloc( (line_len + 1) * sizeof( char *));
    line_copy = (char *) MemMalloc( 2 * (line_len + 1) * sizeof( char));

    for ( line_pos = 0, line_copy_pos = 0, word_num = 0; line[line_pos] != '\0'; line_pos++ )
    {
        char c = line[line_pos];

        if ( c == '[' || c == '(' )
        {
            switch ( state )
            {
            case IN_PLAINSTR:
                word[word_len++] = '\0';
                line_copy_pos++;
                /* word = MemRealloc( word, word_len); */
                /* fall through */
            case AFTER_BRACKETS:
                words[word_num++] = word;
                /* fall through */
            case OUTSIDE:
                /* word = MemMalloc(line_len); */
                word = &line_copy[line_copy_pos];
                word_len = 0;
                state = IN_BRACKETS;
                /* fall through */
            case IN_QUOTE:
                word[word_len++] = c;
                line_copy_pos++;
                break;
            default:
                {
                    assert( 0);
                }
            }
        }
        else if ( c == '"' )
        {
            if ( state == OUTSIDE )
            {
                state = IN_QUOTE;
                /* word = MemMalloc( line_len); */
                word = &line_copy[line_copy_pos];
                word_len = 0;
                word[word_len++] = c;
                line_copy_pos++;
            }
            else if ( state == IN_QUOTE )
            {
                state = IN_PLAINSTR;
                word[word_len++] = c;
                line_copy_pos++;
                state = IN_PLAINSTR;
            }
            else if ( state == IN_PLAINSTR )
            {
                state = IN_QUOTE;
                word[word_len++] = c;
                line_copy_pos++;
            }
            else
            {
                assert( 0);
            }
        }
        else if ( c == ']' || c == ')' )
        {
            if ( state == IN_BRACKETS )
            {
                word[word_len++] = c;
                line_copy_pos++;
                state = AFTER_BRACKETS;
            }
            else if ( state == IN_QUOTE )
            {
                word[word_len++] = c;
                line_copy_pos++;
            }
            else
            {
                assert( 0);
            }
        }
        else if ( split_on_dot && c == '.' )
        {
            if ( state == IN_PLAINSTR || state == AFTER_BRACKETS )
            {
                word[word_len++] = '\0';
                line_copy_pos++;
                /* word = MemRealloc( word, word_len); */
                words[word_num++] = word;
                state = OUTSIDE;

            }
            else
            {
                assert( state == OUTSIDE || state == IN_QUOTE);
            }
        }
        else if ( IsCharGraph( c) )
        {
            if ( state == OUTSIDE )
            {
                /* word = MemMalloc(line_len); */
                word = &line_copy[line_copy_pos];
                word_len = 0;
                word[word_len++] = c;
                line_copy_pos++;
                state = IN_PLAINSTR;
            }
            else if ( state == IN_BRACKETS )
            {
                /* assert( IsCharNumeric( c)); */
                word[word_len++] = c;
                line_copy_pos++;
            }
            else if ( state == AFTER_BRACKETS )
            {
                assert( 0);
            }
            else
            {
                word[word_len++] = c;
                line_copy_pos++;
            }
        }
        else      /* non-printable */
        {
            if ( state == IN_QUOTE )
            {
                word[word_len++] = c;
                line_copy_pos++;
            }
            else if ( state == IN_BRACKETS )
                assert( 0);
            else if ( state == IN_PLAINSTR || state == AFTER_BRACKETS )
            {
                word[word_len++] = '\0';
                line_copy_pos++;
                /* word = MemRealloc( word, word_len); */
                words[word_num++] = word;
                state = OUTSIDE;
            }
        }
    }

    if ( state != OUTSIDE )
    {
        word[word_len++] = '\0';
        line_copy_pos++;
        /* word = MemRealloc( word, word_len); */
        words[word_num++] = word;
    }

    if ( word_num == 0 )
        MemFree( line_copy);

    words[word_num++] = NULL;
    /* words = MemRealloc( words, sizeof( char *) * word_num); */

    return words;
}

void
TodDeleteParsedString( char ** str)
{
    if ( str == NULL )
        return;

    /*
     * for ( i = 0; str[i] != NULL; i++ )
     * {
     *     MemFree( str[i]);
     * }
     */

    if ( str[0] )
        MemFree( str[0]);
    MemFree( str);
}

/*
 * Provide an atoi()-like interface for ParseUInt64, because the default one is too
 * bulky to be used frequently which is the case with the TOD parser.
 */
UInt64
TodStr2Unsigned( const char * str)
{
    UInt64 result;
    Bool success;
    unsigned str_len;
    const char * str_end;

    for ( str_len = 0; IsCharNumeric( str[str_len]); str_len++ );
    assert( str_len > 0);
    str_end = &str[str_len];

    success = ParseUInt64( str, 10, &str_end, &result);
    /* 0.18446744073709551615 uint32 R3 : 11 */
    /* TODO: enable assertion, see error, think about the problem */
    /* assert( success); */

    UNUSED_PARAM(success);
    return result;
}

TodBBPair
TodGetBBPairFromString( const char * str)
{
    TodBBPair bbpair;

    if ( !IsCharNumeric( *str) )
    {
        str = ScanStringForFirstChar( str, '(');
        str++;
    }
    bbpair.byte = ATOU( str);
    while ( IsCharNumeric( *str) )
        str++;
    if ( *str == '.' )
    {
        /* Bit part */
        str++;
        bbpair.bit = ATOU( str);
        while ( IsCharNumeric( *str) )
            str++;
    }
    else
    {
        bbpair.bit = 0;
    }
    return bbpair;
}

TodType
TodGetBaseTypeFromString( char * str)
{
#define TOD_SCALAR_STR "SCALAR"
#define TOD_REF_STR "REF"
#define TOD_FUNC_STR "FUNC" /* What can I do with this? */
#define TOD_PTR_STR "PTR" /* Useless */
    if ( !CompareStringsN( TOD_SCALAR_STR, str, STRLEN( TOD_SCALAR_STR)) )
        return TOD_SCALAR;
    else if ( !CompareStringsN( TOD_REF_STR, str, STRLEN( TOD_REF_STR)) )
        return TOD_REFERENCE;
    else if ( !CompareStringsN( TOD_FUNC_STR, str, STRLEN( TOD_FUNC_STR)) )
        return TOD_FUNCTION;
    else if ( !CompareStringsN( TOD_PTR_STR, str, STRLEN( TOD_PTR_STR)) )
        return TOD_POINTER;

    return TOD_ALIAS;
}

char *
TodRemoveComments( char * str)
{
    unsigned pos = 0;

    while ( !((str[pos] == '\0') || (str[pos] == '/' && str[pos + 1] == '/')) )
        pos++;

    str[pos] = '\0';

    return str;
}

TodCType*
TodFindOriginatingType( MapMap* name_type_map,
                         char * str)
{
    MapPair* name_type_pair = MapGetFirstPair( name_type_map);

    while ( MemIsNotRefNull( name_type_pair) )
    {
        TodString* name;

        name = MapGetPairKeyRef( name_type_pair);

        if ( !STRCMP( TodGetString( name), str) )
            return MapGetPairValueRef( name_type_pair);

        name_type_pair = MapGetNextPair( name_type_pair);
    }

    /* Type could not be found, maybe it is further ahead in the file? */
    return NULL;
}

int
TodFindStr( char ** words,
             char * str)
{
    unsigned i;

    for ( i = 0; words[i] != NULL; i++ )
    {
        if ( !STRCMP( words[i], str) )
            return i;
    }

    return -1;
}

int
TodFindLastWord( char ** words)
{
    unsigned i;

    for ( i = 0; words[i] != NULL; i++ );

    return i - 1;
}

void
TodResolveAliases( MapMap* name_type_map,
                    ListUnit* forward_type_refs,
                    ListUnit* forward_member_refs)
{
    TodGlobalInfo * global_info = TodGetGlobalInfoPtr();

    while ( MemIsNotRefNull( forward_type_refs) )
    {
        TodCType* referencing_type = ListGetClientRef( forward_type_refs);
        TodString* referenced_type_name = TodGetCTypeOriginatingTypeName( referencing_type);
        char * referenced_type_name_str = TodGetString( referenced_type_name);
        TodCType* referenced_type = TodFindOriginatingType( name_type_map,
                                                                 referenced_type_name_str);

        assert( MemIsNotRefNull( referenced_type));

        TodSetCTypeOriginatingType( referencing_type, referenced_type);

        MemLockPool( global_info->str_pool);
        MemDeleteEntry( referenced_type_name);
        MemUnlockPool( global_info->str_pool);
        forward_type_refs = ListRemoveShiftInPool( forward_type_refs);
    }

    while ( MemIsNotRefNull( forward_member_refs) )
    {
        TodCType* referencing_member = ListGetClientRef( forward_member_refs);
        TodString* referenced_type_name;
        TodCType* referenced_type;

        referenced_type_name = TodGetCTypeOriginatingTypeName( referencing_member);
        referenced_type = TodFindOriginatingType( name_type_map,
                                                   TodGetString( referenced_type_name));
        assert( MemIsNotRefNull( referenced_type));


        TodSetCTypeOriginatingType( referencing_member, referenced_type);

        MemLockPool( global_info->str_pool);
        MemDeleteEntry( referenced_type_name);
        MemUnlockPool( global_info->str_pool);
        forward_member_refs = ListRemoveShiftInPool( forward_member_refs);
    }

}

TodCType*
TodNewCTypeInPool( MemPool* pool)
{
    TodCType* new_ctype;

    MemLockPool( pool);
    new_ctype = MemNewEntry( pool);
    MemUnlockPool( pool);

    TodSetCTypeByteSize( new_ctype, -1);
    TodSetCTypeBitSize( new_ctype, -1);
    TodSetCTypeByteOffset( new_ctype, -1);
    TodSetCTypeBitOffset( new_ctype, -1);

    return new_ctype;
}

/**
 * Load type info (architecture-specific) from file.
 */
Bool
TodLoadTypeInfo( const char *file_name)
{
    /** This enum specifies where we are at. */
    enum {TOD_TYPEPARSER_STRUCT, TOD_TYPEPARSER_UNION, TOD_TYPEPARSER_FILE,
          TOD_TYPEPARSER_OPENING_BRACE, TOD_TYPEPARSER_CLOSING_BRACE, TOD_TYPEPARSER_START,
          TOD_TYPEPARSER_MEMBER} prev_parser_state = TOD_TYPEPARSER_START;
    char * line;
    File file = FileOpen( file_name, FILE_OMODE_READ);
    TodGlobalInfo * global_info = TodGetGlobalInfoPtr();
    MapMap* name_type_map = NULL;
    TodCType* complex_type = NULL;
    unsigned line_no = 0;
    ListUnit* forward_type_refs = NULL;
    ListUnit* forward_member_refs = NULL;

    assert( MemIsNotRefNull( global_info->file2local));

    /*
     * File not found. Ensure you have the
     * *.typedes file (or link) in the current directory.
     */
    if ( !IsFileOk( file) )
    {
        StmPrintF( STREAM_STDERR,
                        "Error: Can't find a typedes file \"%s\" in current directory.\n"
                        "Ensure you have built a typedes file and made a link to it "
                        "before using TOD.",
                        file_name);
        Crash();
    }

    /** You must enable TOD with the -tod parameter in order to use it. */
    assert( conf_GetOptns( rma_tools.enable_tod));

    for ( line = TodGetLine( file); line; line = TodGetLine( file) )
    {
        char ** words = TodSplitLineToWords( TodRemoveComments( line), FALSE);
        MemEntry* file_name = NULL;

        /* Empty string */
        if ( words == NULL || !STRCMP (words[0], "//") )
        {
            /* Empty line */
        }
        else if ( !STRCMP( words[0], "file") )
        {
            TodResolveAliases( name_type_map, forward_type_refs, forward_member_refs);
            forward_type_refs = NULL;
            forward_member_refs = NULL;

            file_name = TodNewString( words[1]);
            name_type_map = MapNew( MAP_REF_REF);

            MapAddRefRef( global_info->file2local, file_name, name_type_map, FALSE);
        }
        else if ( !STRCMP( words[0], "typedef") )
        {
            TodCType* base_type;
            TodCType * base_type_p;
            TodType type;
            int i, first_index_pos = 2;
            ListUnit* indices = NULL;

            /* TODO */
            /* Skip typedefing structure with the same name. This is a workaround
               until I implement proper distiction between namings of
               "struct X", "union X" and simple "X". Right now they are all
               called "X" which will cause fails in cases of unusual name reuse. */
            /* To start fixing the problem, just delete 2 following lines */
            if ( !STRCMP( words[1], words[2]) )
                goto finish;

            /** functions are not supported (and not fully represented in DWARF anyway) */
            if ( !STRCMP( words[1], "FUNC" ) )
                goto finish;

            base_type = TodNewCTypeInPool( global_info->c_type_pool);
            base_type_p = TodGetCTypePtr( base_type);
            type = TodGetBaseTypeFromString( words[1]);

            TodSetCTypeType( base_type, type);

            if ( type == TOD_SCALAR || type == TOD_POINTER )
            {
                first_index_pos = 3;
                TodSetCTypeByteSize( base_type, TodGetBBPairFromString( words[2]).byte);
            }
            else if ( type == TOD_ALIAS )
            {
                TodCType* originating_type = TodFindOriginatingType( name_type_map,
                                                                          words[1]);

                if ( MemIsNotRefNull( originating_type) )
                    TodSetCTypeOriginatingType( base_type, originating_type);
                else
                {
                    forward_type_refs = ListConsRefInPool( forward_type_refs, base_type,
                                                            global_info->Listpool);
                    TodSetCTypeOriginatingTypeName( base_type, TodNewString( words[1]));
                }
                TodSetCTypeByteSize( base_type, 0);
                TodSetCTypeName( base_type, TodNewString( words[2]));
            }
            else if ( type == TOD_FUNCTION || type == TOD_POINTER )
            {
                /* Something _very_ clever */
            }
            else
            {
                assert( 0);
            }

            base_type_p->name = TodNewString( words[TodFindLastWord( words)]);

            /* Always zero for non-members */
            TodSetCTypeBitSize( base_type, 0);
            TodSetCTypeByteOffset( base_type, 0);
            TodSetCTypeBitOffset( base_type, 0);

            for ( i = first_index_pos; words[i + 1] != 0; i++)
            {
                indices = ListInsertInPool( indices, global_info->Listpool);
                ListSetClientData( indices, ATOU( &words[i][1]));
            }

            TodSetCTypeIndices( base_type, ListReverse( indices));

            MapAddRefRef( name_type_map, base_type_p->name, base_type, FALSE);
        }
        else if ( !STRCMP( words[0], "struct") || !STRCMP( words[0], "union") )
        {
            TodCType* struct_type = TodNewCTypeInPool( global_info->c_type_pool);
            TodCType * struct_type_p = TodGetCTypePtr( struct_type);
            MemEntry* struct_name = TodNewString( words[2]);

            complex_type = struct_type;
            struct_type_p->name = struct_name;
            if ( !STRCMP( words[0], "struct") )
            {
                struct_type_p->type = TOD_STRUCT;
                prev_parser_state = TOD_TYPEPARSER_STRUCT;
            }
            else
            {
                struct_type_p->type = TOD_UNION;
                prev_parser_state = TOD_TYPEPARSER_UNION;
            }
            TodSetCTypeFirstSubfield( struct_type, NULL);
            TodSetCTypeByteSize( struct_type, ATOU( &words[1][1]));
            /** Unions elements cannot be bitfields (?) */
            TodSetCTypeBitOffset( struct_type, 0);
            TodSetCTypeBitSize( struct_type, 0);
            TodSetCTypeIndices( struct_type, NULL);

            MapAddRefRef( name_type_map, struct_name, struct_type, FALSE);
        }
        else if ( !STRCMP( words[0], "union") )
        {
            /* printf("Union definition\n"); */
        }
        else if ( !STRCMP( words[0], "{") )
        {
            assert( prev_parser_state == TOD_TYPEPARSER_STRUCT
                        || prev_parser_state == TOD_TYPEPARSER_UNION);

            prev_parser_state = TOD_TYPEPARSER_OPENING_BRACE;
        }
        else if ( !STRCMP( words[0], "}") )
        {
            assert( prev_parser_state == TOD_TYPEPARSER_MEMBER
                        || prev_parser_state == TOD_TYPEPARSER_OPENING_BRACE);

            complex_type = NULL;

            prev_parser_state = TOD_TYPEPARSER_CLOSING_BRACE;
        }
        else if ( prev_parser_state == TOD_TYPEPARSER_OPENING_BRACE
                  || prev_parser_state == TOD_TYPEPARSER_MEMBER )
        {
            TodCType* prev_member = TodGetCTypeFirstSubfield( complex_type);
            TodCType* new_member = TodNewCTypeInPool( global_info->c_type_pool);
            TodType type;
            int type_str_pos, current_pos, name_pos;
            ListUnit* indices = NULL;

            assert( MemIsNotRefNull( complex_type));

            if ( TodGetCTypeType( complex_type) == TOD_STRUCT )
            {
                TodBBPair offset = TodGetBBPairFromString( words[0]);

                /* size = TodGetBBPairFromString( words[1]); */
                /* First element = offset, second element = type */

                name_pos = 2;
                TodSetCTypeByteOffset( new_member, offset.byte);
                TodSetCTypeBitOffset( new_member, offset.bit);
                type_str_pos = 1;
            }
            else
            {
                /* size = TodGetBBPairFromString( words[0]); */
                /* First element = type */
                name_pos = 1;
                TodSetCTypeByteOffset( new_member, 0);
                TodSetCTypeBitOffset( new_member, 0);
                type_str_pos = 0;
            }

            type = TodGetBaseTypeFromString( words[type_str_pos]);

            if ( type == TOD_SCALAR || type == TOD_POINTER )
                name_pos++;

            TodSetCTypeType( new_member, TodGetBaseTypeFromString( words[type_str_pos]));
            TodSetCTypeName( new_member, TodNewString( words[name_pos]));

            if ( type == TOD_SCALAR || type == TOD_POINTER )
            {
                /* defining base type directly */
                TodBBPair size = TodGetBBPairFromString( words[type_str_pos + 1]);
                TodSetCTypeByteSize( new_member, size.byte);
            }
            else if ( type == TOD_ALIAS )
            {
                /* referencing (aliasing) an already defined type */
                TodCType* originating_type = TodFindOriginatingType( name_type_map,
                                                                          words[type_str_pos]);
                if ( MemIsNotRefNull( originating_type) )
                    TodSetCTypeOriginatingType( new_member, originating_type);
                else
                {
                    TodSetCTypeOriginatingTypeName( new_member,
                                                     TodNewString( words[type_str_pos]));
                    forward_member_refs = ListConsRefInPool( forward_member_refs, new_member,
                                                              global_info->Listpool);
                }
                TodSetCTypeByteSize( new_member, 0);
            }
            else
            {
                assert( 0);
            }

            for ( current_pos = name_pos + 1;
                  words[current_pos] != 0 && STRCMP( words[current_pos], ":");
                  current_pos++ )
            {
                indices = ListInsertInPool( indices, global_info->Listpool);
                ListSetClientData( indices, ATOU( &words[current_pos][1]));
            }

            TodSetCTypeIndices( new_member, ListReverse( indices));

            if ( words[current_pos] != 0 )
            {
                /* : BIT_LENGTH may follow */
                TodSetCTypeBitSize( new_member, ATOU( words[current_pos + 1]));
            }
            else
                TodSetCTypeBitSize( new_member, 0);

            TodSetCTypeNextField( new_member, NULL);

            if ( MemIsRefNull( prev_member) )
                TodSetCTypeFirstSubfield( complex_type, new_member);
            else
            {
                prev_member = TodFindCTypeLastField( prev_member);
                TodSetCTypeNextField( prev_member, new_member);
            }

            prev_parser_state = TOD_TYPEPARSER_MEMBER;
        }
        else
        {
            /* printf("%%something completely different%%\n"); */
        }

    finish:
        line_no++;
        TodDeleteParsedString( words);
        MemFree( line);
    }

    TodResolveAliases( name_type_map, forward_type_refs, forward_member_refs);

    return TRUE;
}

/*
 * Save type info in a file. Currently unmaintained.
 */
Bool
TodSaveTypeInfo( const char * file_name)
{
    MapPair* file2local_pair;
    TodGlobalInfo * global_info = TodGetGlobalInfoPtr();
    File file = FileOpen( file_name, FILE_OMODE_WRITE);

    for ( file2local_pair = MapGetFirstPair( global_info->file2local);
          MemIsNotRefNull( file2local_pair);
          file2local_pair = MapGetNextPair( file2local_pair) )
    {
        MapMap* name_type_map = MapGetPairValueRef( file2local_pair);
        MapPair* name_type_pair;

        TodFilePrintF( file, "file %s\n", TodGetString( MapGetPairKeyRef( file2local_pair)));

        for ( name_type_pair = MapGetFirstPair( name_type_map);
              MemIsNotRefNull( name_type_pair);
              name_type_pair = MapGetNextPair( name_type_pair) )
        {
            TodCType* struct_type = MapGetPairValueRef( name_type_pair);
            TodCType * struct_type_p = TodGetCTypePtr( struct_type);
            char * c_type_name = TodGetString( MapGetPairKeyRef( name_type_pair));
            char * decl_type = NULL;

            if ( struct_type_p->type == TOD_SCALAR )
                decl_type = "typedef";
            else if ( struct_type_p->type == TOD_POINTER )
                decl_type = "typedef";
            else if ( struct_type_p->type == TOD_ALIAS )
                decl_type = "typedef";
            else if ( struct_type_p->type == TOD_REFERENCE )
                decl_type = "typedef";
            else if ( struct_type_p->type == TOD_STRUCT )
                decl_type = "struct";
            else if ( struct_type_p->type == TOD_UNION )
                decl_type = "union";
            else if ( struct_type_p->type == TOD_FUNCTION )
                decl_type = "typedef";
            else
            {
                assert( 0);
            }

            TodFilePrintF( file, "  %s", decl_type);
            TodPrintTypeNameStr( file, struct_type);
            if ( (struct_type_p->type == TOD_ALIAS || struct_type_p->type == TOD_REFERENCE)
                 && MemIsNotRefNull( TodGetCTypeIndices( struct_type)) )
            {
                TodPrintIndices( file, TodGetCTypeIndices( struct_type));
            }
            else if ( struct_type_p->type == TOD_STRUCT || struct_type_p->type == TOD_UNION )
            {
                TodFilePrintF( file, "(%d)", TodGetCTypeByteSize( struct_type));
            }
            TodFilePrintF( file, " %s", c_type_name);
            TodFilePrintF( file, "\n");

            if ( struct_type_p->type == TOD_STRUCT || struct_type_p->type == TOD_UNION )
            {
                TodCType* member = TodGetCTypeFirstSubfield( struct_type);

                TodFilePrintF( file, "  {\n");

                while ( MemIsNotRefNull( member) )
                {
                    TodFilePrintF( file, "    ");
                    if ( struct_type_p->type == TOD_STRUCT )
                    {
                        if ( TodGetCTypeBitOffset( member) != 0 )
                            TodFilePrintF( file, "%d.%d ", TodGetCTypeByteOffset( member),
                                     TodGetCTypeBitOffset( member));
                        else
                            TodFilePrintF( file, "%d ", TodGetCTypeByteOffset( member));
                    }

                    TodPrintTypeNameStr( file, member);

                    TodFilePrintF( file, "%s", TodGetString( TodGetCTypeName( member)));

                    TodPrintIndices( file, TodGetCTypeIndices( member));

                    if ( TodGetCTypeBitSize( member) != 0 )
                        TodFilePrintF( file, " : %d", TodGetCTypeBitSize( member));

                    TodFilePrintF( file, "\n");

                    member = TodGetCTypeNextField( member);
                }

                TodFilePrintF( file, "  }\n");
            }
        }
    }

    FileClose( file);

    return TRUE;
}

MapMap*
TodFindLocalMapByTypeName( MapMap* file2local_map,
                  MemType entry_type)
{
    MapPair* file2local_pair;
    unsigned filename_len = ScanStringForFirstChar( entry_type, ':') - entry_type;
    for ( file2local_pair = MapGetFirstPair( file2local_map);
          MemIsNotRefNull( file2local_pair);
          file2local_pair = MapGetNextPair( file2local_pair) )
    {
        char * pair_filename = MemGetEntryPtr( MapGetPairKeyRef( file2local_pair));
        if ( !CompareStringsN( entry_type, pair_filename, filename_len) )
        {
            return MapGetPairValueRef( file2local_pair);
        }
    }

    return NULL;
}

TodCType*
TodFindCTypeByTypeNameInLocalMap( MapMap* localmap,
                                   char * entry_type)
{
    MapPair* localmap_pair;
    const char * typename = ScanStringForFirstChar( entry_type, ':') + 1;
    for ( localmap_pair = MapGetFirstPair( localmap);
          MemIsNotRefNull( localmap_pair);
          localmap_pair = MapGetNextPair( localmap_pair) )
    {
        char * pair_typename = MemGetEntryPtr( MapGetPairKeyRef( localmap_pair));
        if ( !STRCMP( typename, pair_typename) )
        {
            return MapGetPairValueRef( localmap_pair);
        }
    }

    return NULL;
}

/**
 * Find object type description (ctype) by its full ("file:object") name.
 */
TodCType*
TodFindCTypeByTypeName( char * type_name)
{
    MapMap* file2local = TodGetGlobalInfoPtr()->file2local;
    MapMap* local_map = TodFindLocalMapByTypeName( file2local, type_name);
    return TodFindCTypeByTypeNameInLocalMap( local_map, type_name);
}

/**
 * Switch bit offset value format from DWARF representation to the one used in host
 * and vice versa.
 */
unsigned
TodSwitchBitOffsetFormat( unsigned byte_size,
                           unsigned bit_size,
                           unsigned bit_offset)
{
    /** Count for endianness. */
#ifdef ENDIAN_LITTLE
    assert( byte_size * 8 >= bit_size + bit_offset);
    bit_offset = byte_size * 8 - bit_size - bit_offset;
#elif ENDIAN_BIG
    /** We're good. */
#else
#error Unknown endianness.
#endif

    return bit_offset;
}

void
TodPrintScalar( File file,
                 void * object_p,
                 unsigned byte_size,
                 unsigned bit_size,
                 unsigned bit_offset,
                 ListUnit* indices)
{
    vmask_Mask val_vmask;
    /** The size of a chunk we are working with. */
    Size chunk_size = sizeof( UInt64) * 8;

    if ( bit_size == 0 )
    {
        /** Unspecified bit size */
        bit_size = byte_size * 8;
    }

    /*
     * Artifitial limitation allowing us to use only one chunk. Easily expendable
     * when a use case arises. TODO.
     */
    assert( byte_size <= chunk_size);

    if ( MemIsNotRefNull( indices) )
    {
        unsigned elem_num = ListGetClientData( indices);
        unsigned i;
        ListUnit* subindices = ListNext( indices);

        for ( i = 0; i < elem_num; i++ )
        {
            /** Bit field arrays should not be possible. */
            assert( bit_offset == 0 && bit_size == 0);

            TodFilePrintF( file, "{");
            TodPrintScalar( file, object_p, bit_size / elem_num, 0, 0,
                             subindices);
            TodFilePrintF( file, "}");
        }
    }
    else
    {
        long long unsigned int val = * (UInt64 *) object_p;

        bit_offset = TodSwitchBitOffsetFormat( byte_size, bit_size, bit_offset);

        val_vmask = vmask_Shr( vmask_GetByIntVal( val), bit_offset);
        val_vmask = vmask_CutByBits( val_vmask, bit_size);
        val = vmask_GetIntVal( val_vmask);

        TodFilePrintF( file, "0x%llX", (unsigned long long)val);
    }
    UNUSED_PARAM( chunk_size);
}

TodCType*
TodFindBaseType( TodCType* ctype)
{
    while ( MemIsNotRefNull( ctype) && TodGetCTypeType( ctype) == TOD_ALIAS )
        ctype = TodGetCTypeOriginatingType( ctype);

    return ctype;
}

int
TodFindCTypeSize( TodCType* ctype)
{
    TodType type = TodGetCTypeType( ctype);

    if ( type == TOD_ALIAS )
        return TodFindCTypeSize( TodGetCTypeOriginatingType( ctype));
    else
        return TodGetCTypeByteSize( ctype);
}

/** Print name path (e.g. {ir}->{ids}->{8}) to file */
void
TodPrintNamePath( File file, /** file */
                   ListUnit* name_path) /** name path */
{
    ListUnit* rev_name_path = ListReverse( ListCopy( name_path));
    Bool first_time = TRUE;

    while ( MemIsNotRefNull( rev_name_path) )
    {
        if ( !first_time && ListGetClientData2( rev_name_path) != TOD_INDEX )
            TodFilePrintF( file, ".");
        else
            first_time = FALSE;

        /** Print array index. */
        if ( ListGetClientData2( rev_name_path) == TOD_INDEX )
            TodFilePrintF( file, "[%i]", (int)ListGetClientData( rev_name_path));
        /** Print name kept inside manageable (ref) or non-manageable (ptr) memory */
        else if ( ListGetClientData2( rev_name_path) == TOD_FIELD_NAME_REF )
            TodFilePrintF( file, "%s", TodGetString( ListGetClientRef( rev_name_path)));
        else if ( ListGetClientData2( rev_name_path) == TOD_FIELD_NAME_PTR )
            TodFilePrintF( file, "%s", (char *)ListGetClientPtrData( rev_name_path));
        /** Print psotfix number */
        else if ( ListGetClientData2( rev_name_path) == TOD_POSTFIX_NUM )
            TodFilePrintF( file, "$%d", (int)ListGetClientData( rev_name_path));
        else
        {
            assert( 0);
        }

        rev_name_path = ListRemoveShift( rev_name_path);
        first_time = FALSE;
    }
}

/**
 * Get object ID by its reference and register it in the dump queue if it's not there yet.
 */
static int
TodGetObjIdByRef( MapMap* ref2id,  /** reference <=> ID map to use */
                   MemEntry* entry, /** reference to object */
                   ListUnit* *due_objects, /** Pointer to due object reference.  */
                   int * object_num,
                   MemEntry* name_path, /** Element name path used in warnings. */
                   void * field_p) /** Field address used in warnings */
{
    unsigned id;
    void * entry_ptr;
    Bool ref_deb = conf_GetOptns( rma_tools.enable_Todref_deb);

    /* Check for null without validity check used in MemIsRefNull(). */
    if ( MemGetEntryPtrImpl( entry) == MemGetEntryPtrImpl( NULL) )
        return TOD_REF_NULL;

    entry_ptr = MemGetEntryPtrImpl( entry);

#if OS_TYPE == OS_TYPE_UNIX
    if ( ref_deb )
    {
        /**
         * This code is intended for finding invalid pointers to unmapped or unreadable
         * addresses in Linux (and other POSIX-compatible systems).
         * It is used only for debugging and, consequently, exploits platform-dependent
         * features.
         */
        #include <errno.h>

        int fildes[2];
        Bool is_entry_invalid;

        pipe( fildes);

        /* write(2): EFAULT: buf is outside your accessible address space. */
        is_entry_invalid = write( fildes[1], entry_ptr, 1) == -1;

        close( fildes[0]);
        close( fildes[1]);

        if ( is_entry_invalid )
        {
            StmPrintF( STREAM_STDERR, "Warning: Reference at %p ", field_p);
            TodPrintNamePath( FileNull, name_path);
            StmPrintF( STREAM_STDERR, " = %p is invalid!\n", entry_ptr);

            return TOD_REF_INVALID;
        }
    }
#endif

    /* TODO: make this condition an assertion. If something is deleted, all references to it
       should be set to zero by the programmer. */
    if ( MemIsEntryFree( entry) )
    {
        if ( ref_deb )
        {
            StmPrintF( STREAM_STDERR, "Warning: Reference at %p ", field_p);
            TodPrintNamePath( FileNull, name_path);
            StmPrintF( STREAM_STDERR, " = %p was freed!\n", entry_ptr);
        }
        return TOD_REF_DELETED;
    }

    if ( ref_deb )
    {
        /* Perform additional pool validity checks for the reference debug mode. */
        MemPool* pool = MemGetEntryPoolImpl( entry);

        if ( (MemGetPoolPtrImpl( pool) == MemGetPoolPtrImpl( MemPoolNull))
#if defined TOD_REACH_PRIVATE_EXTERNAL_FIELDS && defined MEM_CHECK_POOLS
             || (((MemPool *)pool)->__debug_type_id__ != MEM_POOL_TYPE_ID)
#endif
             || MemIsPoolFree( pool) )
        {
            StmPrintF( STREAM_STDERR, "Warning: Reference at %p ", field_p);
            TodPrintNamePath( FileNull, name_path);
            StmPrintF( STREAM_STDERR, " = %p has an invalid pool! \n", entry_ptr);
            return TOD_REF_INVALID;
        }
    }

    /* TODO get rid of Hash */
    if ( MemIsRefNull( HashFindRef( ref2id, entry)) )
    {
        /* If the code fails here, the reference must be invalid. */

        if ( MemGetPoolMainSubentryType( MemGetEntryPool( entry)) == NULL )
            return TOD_REF_NO_TYPE_INFO;

        id = *object_num;
        /* MemLockPool( HashGetTableEntryPool( ref2id)); */
        MapAddRefData( ref2id, entry, id, FALSE);
        /* MemUnlockPool( HashGetTableEntryPool( ref2id)); */
        *object_num = *object_num + 1;
#if 0
        {
            /* Depth-first walkthrough. More machine-intuitive. */

            *due_objects = ListConsRefIn( *due_objects, entry);
        }
#else
        {
            ListUnit* new_unit;

            /* Breadth-first walkthrough. Perhaps a little more human-intuitive. */
            new_unit = ListConsRefDirected( ListFindEnd( *due_objects, LIST_DIR_DEFAULT_INSERT),
                                             LIST_DIR_DEFAULT, entry);
            if ( MemIsRefNull( *due_objects) )
                *due_objects = new_unit;
        }
#endif
    }
    else
    {
        id = MapGetRefData( ref2id, entry);
    }

    return id;
}

int TodGetPoolIdByRef( HashTable* pool2id,
                        MemPool* pool,
                        int * pool_num,
                        Bool * is_new)
{
    HashEntry* Hashentry;
    int pool_id;

    Hashentry = HashCreateStruct( pool2id, &pool, is_new);

    if ( *is_new )
    {
        pool_id = (*pool_num)++;
        HashSetEntryClientData( Hashentry, pool_id);
    }
    else
    {
        pool_id = HashGetEntryClientData( Hashentry);
    }

    return pool_id;
}

/**
 * Internal recursive procedure for saving object information in a file.
 */
Bool
TodSaveObjectInfoRec( File file,
                       TodCType* field_des,     /* Field description */
                       void * elem_p,            /* Pointer to the element dumped. */
                       MapMap* ref2id,       /* Object reference <=> Dump object ID mapping. */
                       ListUnit* * due_objects, /* List of objects due for saving. */
                       int * object_num,            /* Pointer to total object number. */
                       MemEntry* object,        /* Reference to the object being dumped. */
                       ListUnit* name_path,     /* Name path. */
                       ListUnit* indices,       /* List of indices */
                       unsigned depth,
                       MapMap* file2local,
                       void * max_offset,
                       MemDeterminant determinant,
                       Bool * is_embedded_needed)
{
    /* I'm working on making ctype always an actual type and NOT an alias. Aliases should be dealiased as soon as they are met. Thus assertion in TOD_ALIAS */

    void * member_p;
    TodCType* member;
    TodCType* type_des;

    /** Derive type description from the field description. */
    if ( MemIsRefNull( field_des) )
        type_des = NULL;
    else
        type_des = TodFindBaseType( field_des);

    if ( MemIsNotRefNull( indices) )
    {
        /* Iterate through indices & call oneself recursively */
        int subelem_num = TodFindElemNum( ListNext( indices));
        int n = ListGetClientData( indices), i;

        for ( i = 0; i < n; i++ )
        {
            name_path = ListConsData2( name_path, i, TOD_INDEX);
            TodSaveObjectInfoRec( file, type_des,
                                   (char *)elem_p +
                                   i * subelem_num * TodFindCTypeSize( type_des),
                                   ref2id, due_objects, object_num, object, name_path,
                                   ListNext( indices), depth, file2local, max_offset,
                                   determinant, is_embedded_needed);
            name_path = ListRemoveShift( name_path);
        }

        return TRUE;
    }
    else
    {
        /* Only one element left, print it */

        switch ( TodGetCTypeType( type_des) )
        {
        case TOD_SCALAR:

            if ( max_offset != NULL && elem_p >= max_offset )
                /** Reached set offset limit */
                return TRUE;

            TodFilePrintFIndented( file, 1, "");
            TodPrintNamePath( file, name_path);
            TodFilePrintF( file, " = ");
            TodPrintScalar( file, elem_p, TodGetCTypeByteSize( type_des),
                             TodGetCTypeBitSize( field_des),
                             TodGetCTypeBitOffset( field_des), indices);
#ifdef TOD_DEBUG_PRINT
            TodFilePrintF( file, " // %p", elem_p);
#endif

            TodFilePrintF( file, "\n");
            break;
        case TOD_POINTER:
            if ( max_offset != NULL && elem_p >= max_offset )
                /** Reached set offset limit */
                return TRUE;

            TodFilePrintFIndented( file, 1, "");
            TodPrintNamePath( file, name_path);
            TodFilePrintF( file, " = ");
            if ( !STRCMP( TodGetString( TodGetCTypeName( type_des)), "MemEntry*") )
            {
                MemEntry* entry = *((MemEntry* *)elem_p);
                int id = TodGetObjIdByRef( ref2id, entry, due_objects, object_num, name_path, elem_p);

                if ( id == TOD_REF_NULL )
                    TodFilePrintF( file, "NULL");
                else if ( id == TOD_REF_NO_TYPE_INFO )
                    TodFilePrintF( file, "%%no_type_info%%");
                else if ( id == TOD_REF_DELETED )
                    TodFilePrintF( file, "%%deleted%%");
                else if ( id == TOD_REF_INVALID )
                    TodFilePrintF( file, "%%invalid%%");
                else
                    TodFilePrintF( file, "*%u", id);
            }
            else
                TodFilePrintF( file, "%%pointer%%");
#ifdef TOD_DEBUG_PRINT
            TodFilePrintF( file, " // %p", elem_p);
#endif
            TodFilePrintF( file, "\n");
            break;
        case TOD_STRUCT:
            for ( member = TodGetCTypeFirstSubfield( type_des);
                  MemIsNotRefNull( member);
                  member = TodGetCTypeNextField( member) )
            {
                name_path = ListConsRefData( name_path, TodGetCTypeName( member),
                                              TOD_FIELD_NAME_REF);
                member_p = (char *)elem_p + TodGetCTypeByteOffset( member);
                TodSaveObjectInfoRec( file, member, member_p,
                                       ref2id, due_objects, object_num, object, name_path,
                                       TodGetCTypeIndices( member), depth + 1, file2local,
                                       max_offset, determinant, is_embedded_needed);
                name_path = ListRemoveShift( name_path);
            }
            break;
        case TOD_ALIAS:
            assert( 0);
            break;
        case TOD_UNION:
            {
                if ( determinant == NULL )
                {
                    if ( max_offset != NULL && elem_p >= max_offset )
                    {
                        /** Reached set offset limit */
                        *is_embedded_needed = TRUE;
                        return TRUE;
                    }

                    TodFilePrintFIndented( file, 1, "");
                    TodPrintNamePath( file, name_path);
                    TodFilePrintF( file, " = 0x0 // No determinant function present\n");
                }
                else
                {
                    char * field_name;
                    PointerDiff offset;

                    offset = (char *)elem_p - (char *)MemGetEntryPtr( object);
                    field_name = determinant( object, offset, depth);

                    if ( field_name == NULL )
                    {
                        if ( max_offset != NULL && elem_p >= max_offset )
                            /** Reached set offset limit */
                            return TRUE;

                        TodFilePrintFIndented( file, 1, "");
                        TodPrintNamePath( file, name_path);
                        TodFilePrintF( file, " = 0x0 // Union set to \"empty\"\n");
                    }
                    else
                    {
                        TodString* current_field_name = NULL;

                        for ( member = TodGetCTypeFirstSubfield( type_des);
                              MemIsNotRefNull( member);
                              member = TodGetCTypeNextField( member) )
                        {
                            current_field_name = TodGetCTypeName( member);

                            if ( !STRCMP( TodGetString( current_field_name), field_name) )
                            {
                                /**
                                 * Determinant explicitly states that a pre-defined type
                                 * is in place. Override max_offset.
                                 */

                                max_offset = NULL;
                                *is_embedded_needed = FALSE;

                                break;
                            }
                        }

                        if ( MemIsRefNull( member) )
                        {
                            if ( !STRCMP( field_name, "$") )
                            {
                                /*
                                 * Determinant explicitly states that the embedded type
                                 * should not be placed.
                                 */

                                *is_embedded_needed = FALSE;
                                return TRUE;
                            }
                            else
                            {
                                /**
                                 * Unrecognised field name. Check returned values of the
                                 * determinant function.
                                 */

                                assert( 0);
                            }
                        }

                        name_path = ListConsPtrData( name_path, field_name);

                        ListSetClientData2( name_path, TOD_FIELD_NAME_PTR);

                        TodSaveObjectInfoRec( file, member, elem_p,
                                               ref2id, due_objects, object_num, object,
                                               name_path,
                                               TodGetCTypeIndices( member), depth + 1,
                                               file2local, max_offset, determinant, is_embedded_needed);

                        name_path = ListRemoveShift( name_path);
                    }
                }
                break;
            }
        default:
            /* many unsupported cases */
            TodFilePrintF( file, "// unhandled type %d\n", TodGetCTypeType( type_des));
        }

        return TRUE;
    }
}

TodCType*
TodGetCTypeByTypeStr( MapMap* file2local,
                       MemType type_str)
{
    MapMap* localmap;

    if ( type_str == NULL )
        return NULL;

    localmap = TodFindLocalMapByTypeName( file2local, type_str);

    if ( MemIsRefNull( localmap) )
        return NULL;

    return TodFindCTypeByTypeNameInLocalMap( localmap, type_str);
}

/** Find field by its name in a CType structure or union element. */
TodCType*
TodFindField( TodCType* cstruct, /** structure or union where the search is performed */
               const char * name)     /** field name */
{
    TodCType* field = TodGetCTypeFirstSubfield( TodFindBaseType( cstruct));

    while ( MemIsNotRefNull( field) && STRCMP( (char *)TodGetCTypeName( field), name) )
        field = TodGetCTypeNextField( field);

    return field;
}

/*
 * Save information about memory entry OBJECT and all other objects (memory entries) it
 * references into FILE_NAME.
 */
Bool
TodSaveObjects( const char * file_name, /** Name of the file the objects will be saved to. */
                 MemEntry* object)   /** The main (initial) object which will be saved
                                             along with other objects it references. */
{
    File file = FileOpen( file_name, FILE_OMODE_WRITE);
    /* Object reference <=> object ID mapping. */
    MapMap* ref2id = MapNew( MAP_REF_DATA);
    /** Pool reference <=> pool ID mapping. */
    /* TODO replace hashes with maps by adding proper calls to the Map package. */
    HashTable* pool2id = HashCreateTable( HASH_STRUCT_KEYS( MemPool*));
    /* Objects due for saving. */
    ListUnit* due_objects = NULL;
    /* Number of saved objects. */
    int object_num = 0;
    /* Number of saved pools */
    int pool_num = 0;
    TodGlobalInfo * global_info = TodGetGlobalInfoPtr();

    /** You must enable TOD with the -tod parameter in order to use it. */
    assert( conf_GetOptns( rma_tools.enable_tod));

    /* Register initial object */
    TodGetObjIdByRef( ref2id, object, &due_objects, &object_num, NULL,
                       MemGetEntryPtr( object));

    while ( MemIsNotRefNull( due_objects) )
    {
        /** Reference to the current object we are working with */
        MemEntry* curr_obj = ListGetClientRef( due_objects);
        /** Pointer to the current object */
        void * curr_obj_p = MemGetEntryPtr( curr_obj);
        /** Pool of the current object */
        MemPool* pool = MemGetEntryPool( curr_obj);

        /** Create shortcuts for some pool fields we want to dump. */
        char * main_type_str = MemGetPoolMainSubentryType( pool);
        char * embedded_type_str = MemGetPoolEmbeddedSubentryType( pool);
        char * embedded_entry_start_field = MemGetPoolEmbeddedSubentryStartField( pool);
        MemDeterminant main_determinant = MemGetPoolMainSubentryDeterminant( pool);
        MemDeterminant embedded_determinant = MemGetPoolEmbeddedSubentryDeterminant( pool);
        unsigned main_entry_quantity = MemGetPoolMainSubentryQuantity( pool);
        unsigned embedded_entry_quantity = MemGetPoolEmbeddedSubentryQuantity( pool);
        int main_determinant_id = MemGetDeterminantIDByPtr( main_determinant);
        int embedded_determinant_id = MemGetDeterminantIDByPtr( embedded_determinant);
        unsigned max_attr_num = MemGetPoolMaxAttrNum( pool);
        unsigned last_alloc_attr_num = MemGetPoolLastAllocAttrNumImpl( pool);

        /** Descriptions of the main and embedded types. */
        TodCType* main_ctype, embedded_ctype;
        /** Indicates whether the pool has already been met or not. */
        Bool new_pool;
        /** Current object and pool IDs */
        unsigned object_id = TodGetObjIdByRef( ref2id, curr_obj, &due_objects,
                                                &object_num, NULL, curr_obj_p);
        unsigned pool_id = TodGetPoolIdByRef( pool2id, pool, &pool_num, &new_pool);
        /** main_size = size of the main part of the object */
        unsigned main_size;
        /** embedded_size = size of the embedded part of the object */
        unsigned embedded_size = 0;
        /** Size of the whole object in memory */
        unsigned full_size;
        /** Offset from the main part from which the embedded part starts. */
        unsigned embedded_offset = 0;

        unsigned i, j;

        /** Remove current object from queue. */
        due_objects = ListRemoveShift( due_objects);

        /** If the pool has never been met, dump information about it. */
        if ( new_pool )
        {
            TodFilePrintF( file, "pool %d // 0x%x\n{\n", pool_id, MemGetPoolPtr( pool));

            /** Only non-default values are dumped */
            if ( main_type_str != NULL )
                TodFilePrintFIndented( file, 1, "main_type = %s\n",
                                        main_type_str);

            if ( embedded_type_str != NULL )
                TodFilePrintFIndented( file, 1, "embedded_type = %s\n",
                                        embedded_type_str);

            if ( main_determinant_id >= 0 )
                TodFilePrintFIndented( file, 1, "main_determinant = %d\n",
                                        main_determinant_id);

            if ( embedded_determinant_id >= 0 )
                TodFilePrintFIndented( file, 1, "embedded_determinant = %d\n",
                                        embedded_determinant_id);

            if ( embedded_entry_start_field != NULL )
                TodFilePrintFIndented( file, 1, "embedded_start = %s\n",
                                        embedded_entry_start_field);

            if ( main_entry_quantity != 1 )
                TodFilePrintFIndented( file, 1, "main_quantity = %d\n",
                                        main_entry_quantity);

            if ( embedded_entry_quantity != 0 )
                TodFilePrintFIndented( file, 1, "embedded_quantity = %d\n",
                                        embedded_entry_quantity);

            if ( max_attr_num != 0 )
            {
                TodFilePrintFIndented( file, 1, "max_attr_num = %u\n",
                                        max_attr_num);

                if ( last_alloc_attr_num != 0 )
                {
                    TodFilePrintFIndented( file, 1, "last_alloc_attr_num = %u\n",
                                            last_alloc_attr_num);
                }
            }

            TodFilePrintF( file, "}\n\n");
        }

        /** Begin object dump */
        TodFilePrintF( file, "object %d pool %d", object_id, pool_id);

        /**
         * Get main & embedded type describing structures that will be used in the dump process
         */
        if ( main_type_str != NULL )
            main_ctype = TodFindBaseType( TodFindCTypeByTypeName( main_type_str));
        else
            main_ctype = NULL;

        if ( embedded_type_str != NULL )
            embedded_ctype = TodFindBaseType( TodFindCTypeByTypeName( embedded_type_str));
        else
            embedded_ctype = NULL;

#ifdef TOD_DEBUG_PRINT
        /**
         * Print address (useful for debugging of the program)
         */
        TodFilePrintF( file, " // ");

        if ( main_type_str )
            TodFilePrintF( file, "%s ", main_type_str);

        if ( embedded_type_str )
            TodFilePrintF( file, "+ %s ", embedded_type_str);

        TodFilePrintF( file, "%p", curr_obj);
#endif
        TodFilePrintF( file, "\n{\n");

        if ( MemIsNotRefNull( main_ctype) )
            main_size = TodGetCTypeByteSize( main_ctype);
        else
            main_size = 0;

        /* Determine embedded type offset, embedded type size and full type size */
        if ( MemIsNotRefNull( embedded_ctype) )
        {
            int i;
            char ** field_path = TodSplitLineToWords( embedded_entry_start_field, TRUE);
            TodCType* embedded_field = main_ctype;

            /* Start embedded field may be multi-layered (e.g. data1.cdt for
               lists with a user type), so we need to parse it. */
            for ( i = 0; field_path[i] != NULL; i++)
            {
                embedded_field = TodFindField( embedded_field, field_path[i]);
                embedded_offset += TodGetCTypeByteOffset( embedded_field);
                embedded_field = TodFindBaseType( embedded_field);
            }

            TodDeleteParsedString( field_path);

            embedded_size = TodGetCTypeByteSize( embedded_ctype);
            full_size = embedded_offset + embedded_size * embedded_entry_quantity;
        }
        else
        {
            embedded_offset = 0;
            full_size = main_size;
        }

        for ( i = 0; i < main_entry_quantity; i++ )
        {
            void * main_entry_p = (char *)curr_obj_p + full_size * i;
            void * embedded_entry_p = MemIsRefNull( embedded_ctype) ?
                0 : (char *)main_entry_p + embedded_offset;
            ListUnit* name_path = NULL;
            Bool distinguish_main = FALSE;
            Bool is_embedded_needed = TRUE;

            if ( MemIsNotRefNull( main_ctype) )
            {

                if ( main_entry_quantity > 1 )
                {
                    /**
                     * More than one instance of the main type is present in one
                     * dumped element. Put an index array in order to distinguish fields.
                     */
                    name_path = ListConsData2( name_path, i, TOD_INDEX);
                    distinguish_main = TRUE;
                }
                TodSaveObjectInfoRec( file, main_ctype, main_entry_p,
                                       ref2id, &due_objects, &object_num,
                                       curr_obj,
                                       name_path, TodGetCTypeIndices( main_ctype), 0,
                                       global_info->file2local,
                                       (char *)embedded_entry_p, main_determinant, &is_embedded_needed);
            }

            /* Dump embedded object also if present */
            if ( MemIsNotRefNull( embedded_ctype) && is_embedded_needed )
            {
                for ( j = 0; j < embedded_entry_quantity; j++ )
                {
                    /**
                     * Put an index array in order to distinguish embedded fields.
                     */
                    name_path = ListConsData2( name_path, j, TOD_INDEX);

                    TodSaveObjectInfoRec( file, embedded_ctype,
                                           (char *)embedded_entry_p + embedded_size * j,
                                           ref2id, &due_objects, &object_num,
                                           curr_obj,
                                           name_path, TodGetCTypeIndices( embedded_ctype), 0,
                                           global_info->file2local, 0,
                                           embedded_determinant, NULL);

                    name_path = ListRemoveShift( name_path);
                }
            }

            if ( distinguish_main )
                name_path = ListRemoveShift( name_path);
        }

        TodFilePrintF( file, "}\n\n");
    }

    FileClose( file);

    /** Delete reference & pool mappings. */
    MapDelete( ref2id);
    HashDeleteTable( pool2id);

    return TRUE;
}

/**
 * Find the number of elements if an array whose dimensions are specified as
 * list elements.
 */
static int
TodFindIndexVolume( ListUnit* indices) /** array dimentsions in a list */
{
    int vol = 1;

    while ( MemIsNotRefNull( indices) )
    {
        vol *= ListGetClientData( indices);
        indices = ListNext( indices);
    }

    return vol;
}

/*
 * Load object(s) from a TOD file into the local memory context and return
 * the main (first) object. Additionally, save references to all created pools in
 * a list and make pool_Listp reference the list if it is not initially NULL.
 */
MemEntry*
TodLoadObjects( const char * file_name, /** TOD file name */
                 ListUnit* * pool_Listp)      /** If pool_list is not NULL, set it to the
                                                       first element of the unordered list of
                                                       the created pools (useful for automated
                                                       deletion).  */
{
    File file = FileOpen( file_name, FILE_OMODE_READ);
    /** Read line. */
    char * line;
    /** State showing where we currently are at. */
    enum {OUTSIDE, BEFORE_POOL_DES, BEFORE_OBJECT_DES, INSIDE_POOL_DES,
          INSIDE_OBJECT_DES} state = OUTSIDE;
    /** Object ID <=> Object reference map. */
    MapMap* objid2ref = MapNew( MAP_DATA_REF);
    /** Pool ID <=> Pool representative map. */
    MapMap* poolid2repr = MapNew( MAP_DATA_REF);
    /** Current object, first object */
    MemEntry* object = NULL, first_object = NULL;
    /** Main and embedded types of the current object. */
    TodCType* main_ctype = NULL, embedded_ctype = NULL;
    /** Current object and pool IDs (as seen in the dump file). */
    int pool_id = -1;
    /**
     * List of yet unresolved references in the form
     * "(pointer to object reference, object ID)"
     */
    ListUnit* unresolved_refs = NULL;
    /** Line number. Needed for debugging. */
    unsigned line_no = 0;
    /** Determines whether we should keep track of create pools or not */
    Bool track_created_pools = pool_Listp != NULL;

    /** The following variables will be filled during pool and/or obj initialization. */
    MemType main_type_name = NULL, embedded_type_name = NULL;
    unsigned main_entry_quantity = 1, embedded_entry_quantity = 0;
    MemDeterminant main_determinant = NULL, embedded_determinant = NULL;
    MemFieldName embedded_type_start_field = NULL;
    MemPool* pool_for_obj = MemPoolNull;
    unsigned max_attr_num = 0, last_alloc_attr_num = 0;

    if ( track_created_pools )
        *pool_Listp = NULL;

    /** Process all lines in the file. */
    for ( line = TodGetLine( file); line; line = TodGetLine( file) )
    {
        char ** words = TodSplitLineToWords( TodRemoveComments( line),
                                              state == OUTSIDE
                                              || state == INSIDE_POOL_DES ? 0 : 1 );
        if ( !words )
        {
            /** Empty line. */
        }
        else if ( state == OUTSIDE && !STRCMP( words[0], "pool") )
        {
            pool_id = ATOU( words[1]);

            main_type_name = embedded_type_name = NULL;
            main_ctype = embedded_ctype = NULL;
            main_determinant = embedded_determinant = NULL;
            main_entry_quantity = 1;
            embedded_entry_quantity = 0;
            embedded_type_start_field = NULL;
            max_attr_num = 0;
            last_alloc_attr_num = 0;

            assert( !words[2]);

            state = BEFORE_POOL_DES;
        }
        else if ( state == OUTSIDE && !STRCMP( words[0], "object") )
        {
            /** Encountered the beginning of an object description. */

            /** Pool to which the described object belongs to. */
            int object_id = ATOU( words[1]);
            int pool_id = ATOU( words[3]);

            assert( !STRCMP( words[2], "pool"));

            /* TodFilePrintF( stderr, "\nfound object %s pool %s\n", words[1], words[3]); */

            /** Find the pool to which the object belongs to by the specified ID. */
            pool_for_obj = MemGetEntryPool( MapGetDataRef( poolid2repr, pool_id));

            assert( !MemIsPoolRefNull( pool_for_obj) );

            /** Create a new entry in the pool which we will be filling. */
            object = MemNewEntry( pool_for_obj);

            /**
             * Zero fill the entry. Ideally, this is not necessary, but is a good
             * precaution since it helps mitigate misleading garbage in fields.
             */
            SetMemoryByByte( MemGetEntryPtr( object), 0,
                                  MemGetPoolEntrySize( pool_for_obj));

            /** Remember the object id <=> object reference connection. */
            MapAddDataRef( objid2ref, object_id, object, FALSE);

            /** Object 0 is the root object and will be returned by the function. */
            if ( object_id == 0 )
                first_object = object;

            /** Initialize shortcuts. */
            main_type_name = MemGetPoolMainSubentryType( pool_for_obj);
            embedded_type_name = MemGetPoolEmbeddedSubentryType( pool_for_obj);
            main_ctype = TodFindBaseType( TodFindCTypeByTypeName( main_type_name));
            if ( embedded_type_name )
                embedded_ctype = TodFindBaseType( TodFindCTypeByTypeName( embedded_type_name));
            else
                embedded_ctype = NULL;
            main_entry_quantity = MemGetPoolMainSubentryQuantity( pool_for_obj);
            embedded_entry_quantity = MemGetPoolEmbeddedSubentryQuantity( pool_for_obj);
            embedded_type_start_field = MemGetPoolEmbeddedSubentryStartField( pool_for_obj);

            state = BEFORE_OBJECT_DES;
        }
        else if ( !STRCMP( words[0], "{") )
        {
            if ( state == BEFORE_POOL_DES )
                state = INSIDE_POOL_DES;
            else if ( state == BEFORE_OBJECT_DES )
                state = INSIDE_OBJECT_DES;
            else
            {
                assert( 0);
            }
        }
        else if ( state == INSIDE_POOL_DES && !STRCMP( words[0], "}" ) )
        {
            /** Pool description has ended. Create the pool. */

            MemPool* new_pool;
            MemSize entry_size;
            unsigned i;

            /** Pool ID must have been allotted first */
            assert( pool_id >= 0);

            /* Determine the full size of an entry kept in the pool */
            if ( MemIsNotRefNull( embedded_ctype) )
            {
                char ** field_path = TodSplitLineToWords( embedded_type_start_field, TRUE);
                TodCType* embedded_field = main_ctype;
                unsigned embedded_offset = 0;
                MemSize embedded_size = TodGetCTypeByteSize( embedded_ctype);

                /* Start embedded field may be multi-layered (e.g. data1.cdt for
                   lists with a user type), so we need to parse it. */
                for ( i = 0; field_path[i] != NULL; i++)
                {
                    embedded_field = TodFindField( embedded_field, field_path[i]);
                    embedded_offset += TodGetCTypeByteOffset( embedded_field);
                    embedded_field = TodFindBaseType( embedded_field);
                }

                assert( embedded_entry_quantity > 0);
                entry_size = embedded_offset + embedded_size * embedded_entry_quantity;

                TodDeleteParsedString( field_path);
            }
            else
            {
                assert( main_entry_quantity > 0);
                entry_size = TodGetCTypeByteSize( main_ctype);
            }

            entry_size *= main_entry_quantity;

            /** Re-create pool and set(restore) its dumped properties */
            new_pool = MemNewFixedPool( MEM_UNKNOWN_DATA_AREA, MEM_POOL_DEFAULT,
                                         entry_size, MEM_POOL_DEFAULT, max_attr_num, TRUE);

            /* TODO typecast */
            if ( track_created_pools )
                *pool_Listp = ListConsRef( *pool_Listp, (MemEntry*)new_pool);

            MemSetPoolMainSubentryType( new_pool, main_type_name);
            MemSetPoolEmbeddedSubentryType( new_pool, embedded_type_name);
            MemSetPoolMainSubentryQuantity( new_pool, main_entry_quantity);
            MemSetPoolEmbeddedSubentryQuantity( new_pool, embedded_entry_quantity);
            MemSetPoolMainSubentryDeterminant( new_pool, main_determinant);
            MemSetPoolEmbeddedSubentryDeterminant( new_pool, embedded_determinant);
            MemSetPoolEmbeddedSubentryStartField( new_pool, embedded_type_start_field);

            for ( i = 0; i < last_alloc_attr_num; i++ )
                MemAllocPoolNewAttr( new_pool, MEM_ATTR_AUTO_ENTRY, sizeof( MemEntry*));

            /** Bind the pool ID with the pool reference to an empty representative object. */
            MapAddDataRef( poolid2repr, pool_id, MemNewEntry( new_pool), FALSE);

            state = OUTSIDE;
        }
        else if ( state == INSIDE_OBJECT_DES && !STRCMP( words[0], "}" ) )
        {
            state = OUTSIDE;
        }
        else if ( state == INSIDE_POOL_DES )
        {
            /** Read a pool parameter. */

            char * param_name, * param_val;

            param_name = words[0];
            assert( !STRCMP( words[1], "="));
            param_val = words[2];

            if ( !STRCMP( param_name, "main_type" ) )
            {
                main_type_name = TodDuplicateString( param_val);
                main_ctype = TodFindBaseType( TodFindCTypeByTypeName( param_val));
            }
            else if ( !STRCMP( param_name, "embedded_type" ) )
            {
                embedded_type_name = TodDuplicateString( param_val);
                embedded_ctype = TodFindBaseType( TodFindCTypeByTypeName( param_val));
            }
            else if ( !STRCMP( param_name, "main_determinant") )
                main_determinant = MemGetDeterminantPtrByID( ATOU( param_val));
            else if ( !STRCMP( param_name, "embedded_determinant") )
                embedded_determinant = MemGetDeterminantPtrByID( ATOU( param_val));
            else if ( !STRCMP( param_name, "main_quantity") )
                main_entry_quantity = ATOU( param_val);
            else if ( !STRCMP( param_name, "embedded_quantity") )
                embedded_entry_quantity = ATOU( param_val);
            else if ( !STRCMP( param_name, "embedded_start") )
                embedded_type_start_field = TodDuplicateString( param_val);
            else if ( !STRCMP( param_name, "max_attr_num") )
                max_attr_num = ATOU( param_val);
            else if ( !STRCMP( param_name, "last_alloc_attr_num") )
                last_alloc_attr_num = ATOU( param_val);
            else
            {
                /* Unknown argument */
                assert( 0);
            }
        }
        else if ( state == INSIDE_OBJECT_DES )
        {
            /** Read and initialize an object field. */

            unsigned field_offset = 0, subentry_offset;
            ListUnit* indices = NULL;
            void * entry_p = MemGetEntryPtr( object), * field_p;
            int i, main_index = 0, embedded_index = 0;
            TodCType* field;

            assert( main_entry_quantity > 0);

            /** First we must deduce what part ... */
            if ( main_entry_quantity == 1 )
            {
                if ( words[0][0] == '[' )
                {
                    field = embedded_ctype;
                    i = 1;
                    embedded_index = ATOU( &words[0][1]);
;
                }
                else
                {
                    field = main_ctype;
                    i = 0;
                }
            }
            else
            {
                assert( main_entry_quantity > 1 );
                assert( words[0][0] == '[');

                main_index = ATOU( &words[0][1]);

                if ( words[1][0] == '[' )
                {
                    field = embedded_ctype;
                    i = 2;
                    embedded_index = ATOU( &words[1][1]);
                }
                else
                {
                    field = main_ctype;
                    i = 1;
                }
            }

            /* Calculate the subentry offset */
            assert( MemGetPoolEntrySize( pool_for_obj) % main_entry_quantity == 0);
            subentry_offset = MemGetPoolEntrySize( pool_for_obj) / main_entry_quantity;
            subentry_offset *= main_index;

            if ( MemIsNotRefNull( embedded_ctype) )
            {
                subentry_offset += TodGetCTypeByteSize( embedded_ctype) * embedded_index;

                if ( MemAreRefsEQ( field, embedded_ctype ) )
                {
                    char ** words = TodSplitLineToWords( embedded_type_start_field, TRUE);
                    int i;
                    TodCType* f = main_ctype;

                    for ( i = 0; words[i] != NULL; i++ )
                    {
                        f = TodFindField( f, words[i]);
                        subentry_offset += TodGetCTypeByteOffset( f);
                    }

                    TodDeleteParsedString( words);
                }
            }

            /* Detect element offset */
            for ( ; words[i] && STRCMP( words[i], "="); i++ )
            {
                if ( words[i][0] == '[' )
                {
                    int index = ATOU( &words[i][1]);

                    /* TodFilePrintF( stderr, "[%d]", index); */

                    field_offset += ( index * TodFindIndexVolume( ListNext( indices))
                                * TodGetCTypeByteSize( TodFindBaseType( field)) );

                    indices = ListNext( indices);
                }
                else
                {
                    /* TodFilePrintF( stderr, "<dot>%s", words[i]); */
                    if ( !STRCMP( words[i], "$") )
                        field = TodFindCTypeByTypeName( embedded_type_name);
                    else
                        field = TodFindField( field, words[i]);
                    field_offset += TodGetCTypeByteOffset( field);
                    indices = TodGetCTypeIndices( field);
                }
            }

            if ( !words[i] || !words[++i] )
                goto saveobj_end;

            /* TodFilePrintF( stderr, "<eq>"); */

            field_p = (char *)entry_p + subentry_offset + field_offset;

            /* Handle element value */
            if ( !CompareStringsN( words[i], "0x", 2) )
            {
                unsigned byte_size = TodGetCTypeByteSize( TodFindBaseType( field));
                unsigned bit_size = TodGetCTypeBitSize( field);
                unsigned bit_offset = TodGetCTypeBitOffset( field);
                UInt64 new_val, old_val;
                vmask_Mask new_val_mask, old_val_mask;

                /** The new value... */
                ParseUInt64( &words[i][0], 16, NULL, &new_val);
                new_val_mask = vmask_GetByIntVal( new_val);
                /** ...will be etched in the old one. */
                old_val = * (UInt64 *) field_p;
                old_val_mask = vmask_GetByIntVal( old_val);


                if ( bit_size == 0 )
                {
                    /** Unspecified bit size */
                    bit_size = byte_size * 8;
                }

                bit_offset = TodSwitchBitOffsetFormat( byte_size, bit_size, bit_offset);

                new_val_mask = vmask_Shl( new_val_mask, bit_offset);
                old_val_mask = vmask_SetBits( old_val_mask, bit_offset, bit_size, 0);
                new_val_mask = vmask_Or( new_val_mask, old_val_mask);

                * (UInt64 *) field_p = vmask_GetLoIntVal( new_val_mask);
            }
            else if ( !CompareStringsN( words[i], "*", 1) )
            {
                int object_id = ATOU( &words[i][1]);
                MemEntry* referenced_obj = MapGetDataRef( objid2ref, object_id);

                /* TodFilePrintF( stderr, "(ptr)*%d ", object_id); */

                if ( MemIsNotRefNull( referenced_obj) )
                    * ( MemEntry* * )field_p = referenced_obj;
                else
                    unresolved_refs = ListConsRefData( unresolved_refs, (MemEntry*)field_p,
                                                        object_id);
            }
            else if ( !STRCMP( words[i], "NULL")
                      || !STRCMP( words[i], "%no_type_info%")
                      || !STRCMP( words[i], "%pointer%")
                      || !STRCMP( words[i], "%deleted%")
                      || !STRCMP( words[i], "%invalid%") )
            {
                /* TodFilePrintF( stderr, "(emptyptr)0 "); */
                * ( MemEntry* * )field_p = NULL;
            }
            else
            {
                assert( 0);
            }

            /* TodFilePrintF( stderr, "\n"); */
        }

    saveobj_end:
        TodDeleteParsedString( words);
        MemFree( line);

        line_no++;
    }

    /** Resolve all unresolved object references */
    while ( MemIsNotRefNull( unresolved_refs) )
    {
        MemEntry* * ref_ptr = (MemEntry* *)ListGetClientRef( unresolved_refs);
        *ref_ptr = MapGetDataRef( objid2ref, ListGetClientData2( unresolved_refs));
        unresolved_refs = ListRemoveShift( unresolved_refs);
    }

    FileClose( file);

    /** Remove pool and object mappings. */
    MapDelete( objid2ref);
    HashDeleteTable( poolid2repr);

    /** The initially requested object is returned */
    return first_object;
}

/**
 * Initialize the core TOD structures and load type info from argv[0].
 * This makes TOD functionality available.
 */
void
TodInit( void)
{
#ifdef COMPILER_NAME

    global_info = TodGetGlobalInfoPtr();

    global_info->file2local = MapNew( MAP_REF_REF);

    global_info->str_pool = MemNewFloatPool( MEM_UNKNOWN_DATA_AREA, 16, MEM_POOL_SMALL,
                                              FALSE);
    MemSetPoolLock( global_info->str_pool, LOCK_E_T_TOD);
    global_info->c_type_pool = MemNewFixedPool( MEM_UNKNOWN_DATA_AREA, 9001,
                                                 sizeof( TodCType), MEM_POOL_SMALL, 0,
                                                 FALSE);
    MemSetPoolLock( global_info->c_type_pool, LOCK_E_T_TOD);

    global_info->Listpool = ListNewPool( 0, LOCK_INVALID_ID);

    /** Load type info. */
    if ( !TodLoadTypeInfo( TOD_TYPEDES_NAME_STR) )
    {
        assert( 0);
    }

#endif
}

/*
 * Remove the core TOD structures and unload type info from the compiler.
 * This makes TOD functionality unavailable.
 */
void
TodDone( void)
{
    TodGlobalInfo * global_info = TodGetGlobalInfoPtr();
    MapPair* pair;

    if ( context_IsLocal() || !conf_GetOptns( rma_tools.enable_tod) )
        return;

    if ( !MemIsPoolRefNull( global_info->c_type_pool) )
    {
        /* Remove C type descriptionsa */
        MemLockPool( global_info->c_type_pool);
        MemDeletePool( global_info->c_type_pool, FALSE);
        global_info->c_type_pool = MemPoolNull;
    }

    if ( !MemIsPoolRefNull( global_info->str_pool) )
    {
        /* Remove global string pool */
        MemLockPool( global_info->str_pool);
        MemDeletePool( global_info->str_pool, FALSE);
        global_info->str_pool = MemPoolNull;
    }

    if ( !MemIsRefNull( global_info->file2local) )
    {
        /* Remove all local type=>des maps */
        for ( pair = MapGetFirstPair( global_info->file2local);
              MemIsNotRefNull( pair);
              pair = MapGetNextPair( pair) )
        {
            MapDelete( MapGetPairValueRef( pair));
        }

        MapDelete( global_info->file2local);
        global_info->file2local = NULL;
    }

    if ( !MemIsPoolRefNull( global_info->Listpool) )
    {
        /* Destroy list pool */
        ListDestroyPool( global_info->Listpool);
        global_info->Listpool = MemPoolNull;
    }
}

/**
 * Delete all pools in a list. Useful when there is need to remove all objects and pools
 * created by the Load function.
 */
void
TodDeletePoolList( ListUnit* pool_list)
{
    while ( MemIsNotRefNull( pool_list) )
    {
        MemDeletePool( (MemPool*)ListGetClientRef( pool_list), FALSE);
        pool_list = ListRemoveShift( pool_list);
    }
}
