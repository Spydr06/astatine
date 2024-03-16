{-# LANGUAGE ForeignFunctionInterface #-}

{-
    Bindings for the pure C API to enable client code to embed GCC as a JIT-compiler

    libgccjit is owned by the Free Software Foundation, Inc. and is licensed under the GNU General Public License V3

    libgccjit is part of GCC.

    GCC is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3, or (at your option)
    any later version.

    GCC is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with GCC; see the file COPYING3.  If not see
    <http://www.gnu.org/licenses/>.
-}

#include <libgccjit.h>

-- module exports
module GccJit.Foreign (
    -- enums
    StrOption(..),
    IntOption(..),
    BoolOption(..),
    OutputKind(..),
    Types(..),
    FunctionKind(..),
    TLSModel(..),
    GlobalKind(..),
    UnaryOp(..),
    BinaryOp(..),
    Comparison(..),
    -- functions
    contextAcquire,
    contextRelease,
    setStrOption,
    setIntOption,
    setBoolOption,
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
    contextSetBoolAllowUnreachableBlocks,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_print_errors_to_stderr
    contextSetBoolPrintErrorsToStderr,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
    contextSetBoolUseExternalDriver,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
    contextAddCommandLineOption,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option
    contextAddDriverOption,
#endif
    contextCompile,
    contextCompileToFile,
    contextDumpToFile,
    contextSetLogfile,
    contextGetFirstError,
    contextGetLastError,
    resultGetCode,
    resultGetGlobal,
    resultRelease,
    objectGetContext,
    objectGetDebugString,
    contextNewLocation,
    locationAsObject,
    typeAsObject,
    contextGetType,
    contextGetIntType,
    typeGetPointer,
    typeGetConst,
    typeGetVolatile,
#ifdef LIBGCCJIT_HAVE_SIZED_INTEGERS
    compatibleTypes,
    typeGetSize,
#endif
    contextNewArrayType,
    contextNewField,
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitfield
    contextNewBitfield,
#endif
    fieldAsObject,
    contextNewStructType,
    contextNewOpaqueStruct,
    structAsType,
    structSetFields,
    structGetField,
    structGetFieldCount,
    contextNewUnionType,
    contextNewFunctionPtrType,
    contextNewParam,
    paramAsObject,
    paramAsLValue,
    paramAsRValue,
    contextNewFunction,
    contextGetBuiltinFunction,
    functionAsObject,
    functionGetParam,
    functionDumpToDot,
    functionNewBlock,
    blockAsObject,
    blockGetFunction,
    contextNewGlobal,
#ifdef LIBGCCJIT_HAVE_CTORS
    contextNewStructConstructor,
    contextNewUnionConstructor,
    contextNewArrayConstructor,
    globalSetInitializerRValue,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_global_set_initializer
    globalSetInitializer,
#endif
    lValueAsObject,
    lValueAsRValue,
    rValueAsObject,
    rValueGetType,
    contextZero,
    contextOne,
    contextNewRValueFromInt,
    contextNewRValueFromLong,
    contextNewRValueFromDouble,
    contextNewRValueFromPtr,
    contextNull,
    contextNewStringLiteral,
    contextNewUnaryOp,
    contextNewBinaryOp,
    contextNewComparison,
    contextNewCall,
    contextNewCallThroughPtr,
    contextNewCast,
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
    contextNewBitcast,
#endif
#ifdef LIBGCCJIT_HAVE_ALIGNMENT
    lValueGetAlignment,
    lValueSetAlignment,
#endif
    contextNewArrayAccess,
    lValueAccessField,
    rValueAccessField,
    rValueDereferenceField,
    rValueDereference,
    lValueGetAddress,
#ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_link_section
    lValueSetLinkSection,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_tls_model
    lValueSetTLSModel,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_register_name
    lValueSetRegisterName,
#endif
    functionNewLocal,
    blockAddEval,
    blockAddAssignment,
    blockAddAssignmentOp,
    blockAddComment,
    blockEndWithConditional,
    blockEndWithJump,
    blockEndWithReturn,
    blockEndWithVoidReturn,
#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
    contextNewCase,
    caseAsObject,
    blockEndWithSwitch,
#endif
    contextNewChildContext,
    contextDumpReproducerToFile,
    contextEnableDump,
#ifdef LIBGCCJIT_HAVE_TIMING_API
    timerNew,
    timerRelease,
    contextSetTimer,
    contextGetTimer,
    timerPush,
    timerPop,
    timerPrint,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call
    rValueSetBoolRequireTailCall,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
    typeGetAligned,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_vector
    typeGetVector,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_function_get_address
    functionGetAddress,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_rvalue_from_vector
    contextNewRValueFromVector,
#endif
#ifdef LIBGCCJIT_HAVE_gcc_jit_version
    versionMajor,
    versionMinor,
    versionPatchlevel,
#endif
#ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS
    blockAddExtendedAsm,
    blockEndWithExtendedAsmGoto,
    extendedAsmAsObject,
    extendedAsmSetVolatileFlag,
    extendedAsmSetInlineFlag,
    extendedAsmAddOutputOperand,
    extendedAsmAddInputOperand,
    extendedAsmAddClobber,
    contextAddTopLevelAsm,
#endif
#ifdef LIBGCCJIT_HAVE_REFLECTION
    functionGetReturnType,
    functionGetParamCount,
    typeDyncastArray,
    typeIsBool,
    typeDyncastFunctionPtrType,
    functionTypeGetReturnType,
    functionTypeGetParamCount,
    functionTypeGetParamType,
    typeIsIntegral,
    typeIsPointer,
    typeDyncastVector,
    typeIsStruct,
    vectorTypeGetNumUnits,
    vectorTypeGetElementType,
    typeUnqualified
#endif
) where

-- Imported modules for C interoperability.
import Foreign
import Foreign.C.Types
import Foreign.C (CString, newCString)
import System.Posix.Types (CSsize)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Applicative (Applicative(liftA2))

-- Import data types
import GccJit.Types

-- Utility functions for the Haskell wrappers.

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe ptr | ptr == nullPtr = Nothing
               | otherwise = Just ptr

maybeToPtr :: Maybe (Ptr a) -> Ptr a
maybeToPtr Nothing = nullPtr
maybeToPtr (Just p) = p

listToPtr :: Storable a => [a] -> IO (Ptr a)
listToPtr xs = withArray xs return

unzipPairs :: [(a, b)] -> ([a], [b])
unzipPairs xs = (fst <$> xs, snd <$> xs)

-- Acquire a JIT-compilation context.
foreign import ccall "gcc_jit_context_acquire" gcc_jit_context_acquire :: IO (Ptr Context)
contextAcquire :: IO (Maybe (Ptr Context))
contextAcquire = ptrToMaybe <$> gcc_jit_context_acquire

-- Release the context.  After this call, it's no longer valid to use the ctxt.
foreign import ccall "gcc_jit_context_release" contextRelease :: Ptr Context -> IO ()

{-
   Options present in the initial release of libgccjit.
   These were handled using enums.
-}

-- Options taking string values.
data StrOption = Progname 
    {- The name of the program, for use as a prefix when printing error
       messages to stderr.  If Nothing, or default, "libgccjit.so" is used. -}
    | NumStrOptions
    deriving (Enum)

-- Options taking int values.
data IntOption = OptimizationLevel
    {- How much to optimize the code.
       Valid values are 0-3, corresponding to GCC's command-line options
       -O0 through -O3.

       The default value is 0 (unoptimized). -}
    | NumIntOptions
    deriving (Enum)

-- Options taking boolean values.
-- These all default to "false".
data BoolOption =
    {- If true, gcc_jit_context_compile will attempt to do the right
       thing so that if you attach a debugger to the process, it will
       be able to inspect variables and step through your code.

       Note that you can't step through code unless you set up source
       location information for the code (by creating and passing in
       gcc_jit_location instances). -}
    DebugInfo
    
    {- If true, gcc_jit_context_compile will dump its initial "tree"
       representation of your code to stderr (before any
       optimizations). -}
    | DumpInitialTree

    {- If true, gcc_jit_context_compile will dump the "gimple"
       representation of your code to stderr, before any optimizations
       are performed.  The dump resembles C code. -}
    | DumpInitialGimple

    {- If true, gcc_jit_context_compile will dump the final
       generated code to stderr, in the form of assembly language. -}
    | DumpGeneratedCode
    
    {- If true, gcc_jit_context_compile will print information to stderr
       on the actions it is performing, followed by a profile showing
       the time taken and memory usage of each phase. -}
    | DumpSummary

    {- If true, gcc_jit_context_compile will dump copious
       amount of information on what it's doing to various
       files within a temporary directory.  Use
       GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES (see below) to
       see the results.  The files are intended to be human-readable,
       but the exact files and their formats are subject to change. -}
    | DumpEverything

    {- If true, libgccjit will aggressively run its garbage collector, to
       shake out bugs (greatly slowing down the compile).  This is likely
       to only be of interest to developers *of* the library.  It is
       used when running the selftest suite. -}
    | SelfcheckGC

    {- If true, gcc_jit_context_release will not clean up
       intermediate files written to the filesystem, and will display
       their location on stderr. -}
    | KeepIntermediate
    | NumBoolOptions
    deriving (Enum)

{-
   Set a string option on the given context.

   The context takes a copy of the string, so the
   (const char *) buffer is not needed anymore after the call
   returns.
-}
foreign import ccall "gcc_jit_context_set_str_option" gcc_jit_set_str_option :: Ptr Context -> CInt -> CString -> IO ()
setStrOption :: Ptr Context -> StrOption -> String -> IO ()
setStrOption ctxt opt value = do
    c_value <- newCString value
    gcc_jit_set_str_option ctxt (fromIntegral $ fromEnum opt) c_value

-- Set an int option on the given context.
foreign import ccall "gcc_jit_context_set_int_option" gcc_jit_set_int_option :: Ptr Context -> CInt -> CInt -> IO ()
setIntOption :: Ptr Context -> IntOption -> Int -> IO ()
setIntOption ctxt opt = gcc_jit_set_int_option ctxt (fromIntegral $ fromEnum opt) . fromIntegral


{-
   Set a boolean option on the given context.

   Zero is "false" (the default), non-zero is "true".
-}
foreign import ccall "gcc_jit_context_set_bool_option" gcc_jit_set_bool_option :: Ptr Context -> CInt -> CInt -> IO ()
setBoolOption :: Ptr Context -> BoolOption -> Bool -> IO ()
setBoolOption ctxt opt = gcc_jit_set_bool_option ctxt (fromIntegral $ fromEnum opt) . fromIntegral . fromEnum

{- 
   Options added after the initial release of libgccjit.
   These are handled by providing an entrypoint per option,
   rather than by extending the enum gcc_jit_*_option,
   so that client code that use these new options can be identified
   from binary metadata.
-}

{- 
   By default, libgccjit will issue an error about unreachable blocks
   within a function.

   This option can be used to disable that error.

   This entrypoint was added in LIBGCCJIT_ABI_2; you can test for
   its presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks
foreign import ccall "gcc_jit_context_set_bool_allow_unreachable_blocks" gcc_jit_context_set_bool_allow_unreachable_blocks :: Ptr Context -> CInt -> IO ()
contextSetBoolAllowUnreachableBlocks :: Ptr Context -> Bool -> IO ()
contextSetBoolAllowUnreachableBlocks ctxt = gcc_jit_context_set_bool_allow_unreachable_blocks ctxt . fromIntegral . fromEnum
#endif

{- 
   By default, libgccjit will print errors to stderr.

   This option can be used to disable the printing.

   This entrypoint was added in LIBGCCJIT_ABI_23; you can test for
   its presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_print_errors_to_stderr
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_print_errors_to_stderr
foreign import ccall "gcc_jit_context_set_bool_print_errors_to_stderr" gcc_jit_context_set_bool_print_errors_to_stderr :: Ptr Context -> CInt -> IO ()
contextSetBoolPrintErrorsToStderr :: Ptr Context -> Bool -> IO ()
contextSetBoolPrintErrorsToStderr ctxt = gcc_jit_context_set_bool_print_errors_to_stderr ctxt . fromIntegral . fromEnum
#endif

{-
   Implementation detail:
   libgccjit internally generates assembler, and uses "driver" code
   for converting it to other formats (e.g. shared libraries).

   By default, libgccjit will use an embedded copy of the driver
   code.

   This option can be used to instead invoke an external driver executable
   as a subprocess.

   This entrypoint was added in LIBGCCJIT_ABI_5; you can test for
   its presence using
     #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver
foreign import ccall "gcc_jit_context_set_bool_use_external_driver" gcc_jit_context_set_bool_use_external_driver :: Ptr Context -> CInt -> IO ()
contextSetBoolUseExternalDriver :: Ptr Context -> Bool -> IO ()
contextSetBoolUseExternalDriver ctxt = gcc_jit_context_set_bool_use_external_driver ctxt . fromIntegral . fromEnum
#endif

{-
   Add an arbitrary gcc command-line option to the context.
   The context takes a copy of the string, so the
   (const char *) optname is not needed anymore after the call
   returns.

   Note that only some options are likely to be meaningful; there is no
   "frontend" within libgccjit, so typically only those affecting
   optimization and code-generation are likely to be useful.

   This entrypoint was added in LIBGCCJIT_ABI_1; you can test for
   its presence using
   #ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
foreign import ccall "gcc_jit_context_add_command_line_option" gcc_jit_context_add_command_line_option :: Ptr Context -> CString -> IO ()
contextAddCommandLineOption :: Ptr Context -> String -> IO ()
contextAddCommandLineOption ctxt optname = do
    c_optname <- newCString optname
    gcc_jit_context_add_command_line_option ctxt c_optname
#endif

{-
   Add an arbitrary gcc driver option to the context.
   The context takes a copy of the string, so the
   (const char *) optname is not needed anymore after the call
   returns.

   Note that only some options are likely to be meaningful; there is no
   "frontend" within libgccjit, so typically only those affecting
   assembler and linker are likely to be useful.

   This entrypoint was added in LIBGCCJIT_ABI_11; you can test for
   its presence using
   #ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option
foreign import ccall "gcc_jit_context_add_driver_option" gcc_jit_context_add_driver_option :: Ptr Context -> CString -> IO ()
contextAddDriverOption :: Ptr Context -> String -> IO ()
contextAddDriverOption ctxt optname = do
    c_optname <- newCString optname
    gcc_jit_context_add_driver_option ctxt c_optname
#endif

{-
   Compile the context to in-memory machine code.

   This can be called more that once on a given context,
   although any errors that occur will block further compilation. 
-}

foreign import ccall "gcc_jit_context_compile" gcc_jit_context_compile :: Ptr Context -> IO (Ptr Result)
contextCompile :: Ptr Context -> IO (Maybe (Ptr Result))
contextCompile = fmap ptrToMaybe . gcc_jit_context_compile

{-
   Kinds of ahead-of-time compilation, for use with
   gcc_jit_context_compile_to_file.
-}

data OutputKind = 
    -- Compile the context to an assembler file.
    Assembler

    -- Compile the context to an object file.
    | ObjectFile

    -- Compile the context to a dynamic library.
    | DynamicLibrary

    -- Compile the context to an executable.
    | Executable
    deriving (Enum)

{-
   Compile the context to a file of the given kind.

   This can be called more that once on a given context,
   although any errors that occur will block further compilation.
-}

foreign import ccall "gcc_jit_context_compile_to_file" gcc_jit_context_compile_to_file :: Ptr Context -> CInt -> CString -> IO ()
contextCompileToFile :: Ptr Context -> OutputKind -> String -> IO ()
contextCompileToFile ctxt output_kind output_path = do
    c_output_path <- newCString output_path
    gcc_jit_context_compile_to_file ctxt (fromIntegral $ fromEnum output_kind) c_output_path

{-
   To help with debugging: dump a C-like representation to the given path,
   describing what's been set up on the context.

   If "update_locations" is true, then also set up gcc_jit_location
   information throughout the context, pointing at the dump file as if it
   were a source file.  This may be of use in conjunction with
   GCC_JIT_BOOL_OPTION_DEBUGINFO to allow stepping through the code in a
   debugger.
-}

foreign import ccall "gcc_jit_context_dump_to_file" gcc_jit_context_dump_to_file :: Ptr Context -> CString -> CInt -> IO ()
contextDumpToFile :: Ptr Context -> String -> Int -> IO ()
contextDumpToFile ctxt path update_locations = do
    c_path <- newCString path
    gcc_jit_context_dump_to_file ctxt c_path $ fromIntegral $ fromEnum update_locations

{-
   To help with debugging; enable ongoing logging of the context's
   activity to the given FILE *.

   The caller remains responsible for closing "logfile".

   Params "flags" and "verbosity" are reserved for future use, and
   must both be 0 for now.
-}

foreign import ccall "gcc_jit_context_set_logfile" gcc_jit_context_set_logfile :: Ptr Context -> Ptr CFile -> CInt -> CInt -> IO ()
contextSetLogfile :: Ptr Context -> Ptr CFile -> Int -> Int -> IO ()
contextSetLogfile ctxt logfile flags = gcc_jit_context_set_logfile ctxt logfile (fromIntegral flags) . fromIntegral

{-
   To be called after any API call, this gives the first error message
   that occurred on the context.

   The returned string is valid for the rest of the lifetime of the
   context.

   If no errors occurred, this will be Nothing.
-}

foreign import ccall "gcc_jit_context_get_first_error" gcc_jit_context_get_first_error :: Ptr Context -> IO CString
contextGetFirstError :: Ptr Context -> IO (Maybe String)
contextGetFirstError ctxt = do
    c_error <- gcc_jit_context_get_first_error ctxt
    return $ if c_error == nullPtr 
        then Nothing
        else Just $ show c_error

{-
  To be called after any API call, this gives the last error message
   that occurred on the context.

   If no errors occurred, this will be Nothing.

   If non-Nothing, the returned string is only guaranteed to be valid until
   the next call to libgccjit relating to this context.
-}

foreign import ccall "gcc_jit_context_get_last_error" gcc_jit_context_get_last_error :: Ptr Context -> IO CString
contextGetLastError :: Ptr Context -> IO (Maybe String)
contextGetLastError ctxt = do
    c_error <- gcc_jit_context_get_last_error ctxt
    return $ if c_error == nullPtr
        then Nothing
        else Just $ show c_error

{-
   Locate a given function within the built machine code.
   This will need to be cast to a function pointer of the
   correct type before it can be called.
-}

foreign import ccall "gcc_jit_result_get_code" gcc_jit_result_get_code :: Ptr Result -> CString -> IO (Ptr ())
resultGetCode :: Ptr Result -> String -> IO (Maybe (FunPtr a))
resultGetCode result funcname = do
    c_funcname <- newCString funcname
    c_funptr <- gcc_jit_result_get_code result c_funcname
    return $ if c_funptr == nullPtr
        then Nothing
        else Just $ castPtrToFunPtr c_funptr

{-
   Locate a given global within the built machine code.
   It must have been created using GCC_JIT_GLOBAL_EXPORTED.
   This is a ptr to the global, so e.g. for an int this is an int *.
-}

foreign import ccall "gcc_jit_result_get_global" gcc_jit_result_get_global :: Ptr Result -> CString -> IO (Ptr ())
resultGetGlobal :: Ptr Result -> String -> IO (Maybe (Ptr a))
resultGetGlobal result name = do
    c_name <- newCString name
    c_ptr <- gcc_jit_result_get_global result c_name
    return $ if c_ptr == nullPtr
        then Nothing
        else Just $ castPtr c_ptr

{-
   Once we're done with the code, this unloads the built .so file.
   This cleans up the result; after calling this, it's no longer
   valid to use the result.
-}

foreign import ccall "gcc_jit_result_release" resultRelease :: Ptr Result -> IO ()

{-*********************************************************************
 Functions for creating "contextual" objects.

 All objects created by these functions share the lifetime of the context
 they are created within, and are automatically cleaned up for you when
 you call gcc_jit_context_release on the context.

 Note that this means you can't use references to them after you've
 released their context.

 All (const char *) string arguments passed to these functions are
 copied, so you don't need to keep them around.

 You create code by adding a sequence of statements to blocks.
*********************************************************************-}

{-*********************************************************************
 The base class of "contextual" object.
 *********************************************************************-}

-- Which context is "obj" within?
foreign import ccall "gcc_jit_object_get_context" objectGetContext :: IO (Ptr Object)

{-
   Get a human-readable description of this object.
   The string buffer is created the first time this is called on a given
   object, and persists until the object's context is released.
-}

foreign import ccall "gcc_jit_object_get_debug_string" gcc_jit_object_get_debug_string :: Ptr Object -> IO CString
objectGetDebugString :: Ptr Object -> IO String
objectGetDebugString = fmap show . gcc_jit_object_get_debug_string

{-*********************************************************************
 Debugging information.
 *********************************************************************-}

{-
   Creating source code locations for use by the debugger.
   Line and column numbers are 1-based.
-}

foreign import ccall "gcc_jit_context_new_location" gcc_jit_context_new_location :: Ptr Context -> CString -> CInt -> CInt -> IO (Ptr Location)
contextNewLocation :: Ptr Context -> String -> Int -> Int -> IO (Ptr Location)
contextNewLocation ctxt filename line column = do
    c_filename <- newCString filename
    gcc_jit_context_new_location ctxt c_filename (fromIntegral line) $ fromIntegral column

-- Upcasting from location to object.
foreign import ccall "gcc_jit_location_as_object" locationAsObject :: Ptr Location -> IO (Ptr Object)

{-*********************************************************************
 Types.
 *********************************************************************-}

-- Upcasting from type to object.
foreign import ccall "gcc_jit_type_as_object" typeAsObject :: Ptr Type -> IO (Ptr Object)

-- Access to specific types.
data Types = Void -- C's "void" type.
    | VoidPtr -- "void *".
    | Bool -- C++'s bool type; also C99's "_Bool" type, aka "bool" if using stdbool.h.
    
    -- Various integer types.
    -- C's "char" (of some signedness) and the variants where the
    -- signedness is specified.
    | Char
    | SignedChar
    | UnsignedChar

    -- C's "short" and "unsigned short".
    | Short -- signed
    | UnsignedShort

    -- C's "int" and "unsigned int".
    | Int -- signed
    | UnsignedInt

    -- C's "long" and "unsigned long".
    | Long -- signed
    | UnsignedLong

    -- C99's "long long" and "unsigned long long".
    | LongLong -- signed
    | UnsignedLongLong

    -- Floating-point types
    | Float
    | Double
    | LongDouble

    
    | ConstCharPtr -- C type: (const char *).
    | SizeT -- The C "size_t" type.
    | FilePtr -- C type: (FILE *)

    -- Complex numbers.
    | ComplexFloat
    | ComplexDouble
    | ComplexLongDouble

#ifdef LIBGCCJIT_HAVE_SIZED_INTEGERS
    -- Sized integer types.
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | UInt128
    | Int8
    | Int16
    | Int32
    | Int64
    | Int128
#endif
    deriving (Enum)

foreign import ccall "gcc_jit_context_get_type" gcc_jit_context_get_type :: Ptr Context -> CInt -> IO (Ptr Type)
contextGetType :: Ptr Context -> Types -> IO (Ptr Type)
contextGetType ctxt = gcc_jit_context_get_type ctxt . fromIntegral . fromEnum

-- Get the integer type of the given size and signedness.
foreign import ccall "gcc_jit_context_get_int_type" gcc_jit_context_get_int_type :: Ptr Context -> CInt -> CInt -> IO (Ptr Type)
contextGetIntType :: Ptr Context -> Int -> Bool -> IO (Ptr Type)
contextGetIntType ctxt numBytes = gcc_jit_context_get_int_type ctxt (fromIntegral numBytes) . fromIntegral . fromEnum

-- Constructing new types.

-- Given type "T", get type "T*".
foreign import ccall "gcc_jit_type_get_pointer" typeGetPointer :: Ptr Type -> IO (Ptr Type)

-- Given type "T", get type "const T".
foreign import ccall "gcc_jit_type_get_const" typeGetConst :: Ptr Type -> IO (Ptr Type)

-- Given type "T", get type "volatile T".
foreign import ccall "gcc_jit_type_get_volatile" typeGetVolatile :: Ptr Type -> IO (Ptr Type)

#ifdef LIBGCCJIT_HAVE_SIZED_INTEGERS

-- Given types LTYPE and RTYPE, return non-zero if they are compatible.
foreign import ccall "gcc_jit_compatible_types" gcc_jit_compatible_types :: Ptr Type -> Ptr Type -> IO CInt
compatibleTypes :: Ptr Type -> Ptr Type -> IO Bool
compatibleTypes ltype rtype = (fromIntegral (0 :: Integer) /=) <$> gcc_jit_compatible_types ltype rtype

-- Given type "T", get its size.
foreign import ccall "gcc_jit_type_get_size" gcc_jit_type_get_size :: Ptr Type -> IO CSize
typeGetSize :: Ptr Type -> IO CSsize
typeGetSize = fmap fromIntegral . gcc_jit_type_get_size

#endif -- LIBGCCJIT_HAVE_SIZED_INTEGERS

-- Given type "T", get type "T[N]" (for a constant N).
foreign import ccall "gcc_jit_context_new_array_type" gcc_jit_context_new_array_type :: Ptr Context -> Ptr Location -> Ptr Type -> CInt -> IO (Ptr Type)
contextNewArrayType :: Ptr Context -> Ptr Location -> Ptr Type -> Int -> IO (Ptr Type)
contextNewArrayType ctxt loc elementType = gcc_jit_context_new_array_type ctxt loc elementType . fromIntegral

-- Struct-handling.

-- Create a field, for use within a struct or union.
foreign import ccall "gcc_jit_context_new_field" gcc_jit_context_new_field :: Ptr Context -> Ptr Location -> Ptr Type -> CString -> IO (Ptr Field)
contextNewField :: Ptr Context -> Ptr Location -> Ptr Type -> String -> IO (Ptr Field)
contextNewField ctxt loc type' name = do
    c_name <- newCString name
    gcc_jit_context_new_field ctxt loc type' c_name

-- Create a bit field, for use within a struct or union.
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitfield
foreign import ccall "gcc_jit_context_new_bitfield" gcc_jit_context_new_bitfield :: Ptr Context -> Ptr Location -> Ptr Type -> CInt -> CString -> IO (Ptr Field)
contextNewBitfield :: Ptr Context -> Ptr Location -> Ptr Type -> Int -> String -> IO (Ptr Field)
contextNewBitfield ctxt loc type' width name = do
    c_name <- newCString name
    gcc_jit_context_new_bitfield ctxt loc type' (fromIntegral width) c_name
#endif

-- Upcasting from field to object.
foreign import ccall "gcc_jit_field_as_object" fieldAsObject :: Ptr Field -> IO (Ptr Object)

-- Create a struct type from an array of fields.
foreign import ccall "gcc_jit_context_new_struct_type" gcc_jit_context_new_struct_type :: Ptr Context -> Ptr Location -> CString -> CInt -> Ptr (Ptr Field) -> IO (Ptr Struct)
contextNewStructType :: Ptr Context -> Ptr Location -> String -> [Ptr Field] -> IO (Ptr Struct)
contextNewStructType ctxt loc name fields = do
    c_name <- newCString name
    c_fields <- listToPtr fields
    gcc_jit_context_new_struct_type ctxt loc c_name (fromIntegral $ length fields) c_fields 

-- Create an opaque struct type.
foreign import ccall "gcc_jit_context_new_opaque_struct" gcc_jit_context_new_opaque_struct :: Ptr Context -> Ptr Location -> CString -> IO (Ptr Struct)
contextNewOpaqueStruct :: Ptr Context -> Ptr Location -> String -> IO (Ptr Struct)
contextNewOpaqueStruct ctxt loc name = do
    c_name <- newCString name
    gcc_jit_context_new_opaque_struct ctxt loc c_name

-- Upcast a struct to a type.
foreign import ccall "gcc_jit_struct_as_type" structAsType :: Ptr Struct -> IO (Ptr Type)

{-
   Populating the fields of a formerly-opaque struct type.
   This can only be called once on a given struct type.
-}
foreign import ccall "gcc_jit_struct_set_fields" gcc_jit_struct_set_fields :: Ptr Struct -> Ptr Location -> CInt -> Ptr (Ptr Field) -> IO ()
structSetFields :: Ptr Struct -> Ptr Location -> [Ptr Field] -> IO ()
structSetFields struct loc fields = do
    c_fields <- listToPtr fields
    gcc_jit_struct_set_fields struct loc (fromIntegral $ length fields) c_fields

-- Get a field by index.
foreign import ccall "gcc_jit_struct_get_field" gcc_jit_struct_get_field :: Ptr Struct -> CSize -> IO (Ptr Field)
structGetField :: Ptr Struct -> Int -> IO (Maybe (Ptr Field))
structGetField struct index = fmap ptrToMaybe . gcc_jit_struct_get_field struct $ fromIntegral index

-- Get the number of fields.
foreign import ccall "gcc_jit_struct_get_field_count" gcc_jit_struct_get_field_count :: Ptr Struct -> IO CSize
structGetFieldCount :: Ptr Struct -> IO Int
structGetFieldCount = fmap fromIntegral . gcc_jit_struct_get_field_count

-- Unions work similarly to structs.
foreign import ccall "gcc_jit_context_new_union_type" gcc_jit_context_new_union_type :: Ptr Context -> Ptr Location -> CString -> CInt -> Ptr (Ptr Field) -> IO (Ptr Type)
contextNewUnionType :: Ptr Context -> Ptr Location -> String -> [Ptr Field] -> IO (Ptr Type)
contextNewUnionType ctxt loc name fields = do
    c_name <- newCString name
    c_fields <- listToPtr fields
    gcc_jit_context_new_union_type ctxt loc c_name (fromIntegral $ length fields) c_fields

-- Function pointers.
foreign import ccall "gcc_jit_context_new_function_ptr_type" gcc_jit_context_new_function_ptr_type :: Ptr Context -> Ptr Location -> Ptr Type -> CInt -> Ptr (Ptr Type) -> CInt -> IO (Ptr Type)
contextNewFunctionPtrType :: Ptr Context -> Ptr Location -> Ptr Type -> [Ptr Type] -> Bool -> IO (Ptr Type)
contextNewFunctionPtrType ctxt loc returnType paramTypes isVariadic = do
    c_paramTypes <- listToPtr paramTypes
    gcc_jit_context_new_function_ptr_type ctxt loc returnType (fromIntegral $ length paramTypes) c_paramTypes $ fromIntegral $ fromEnum isVariadic

{-*********************************************************************
 Constructing functions.
 *********************************************************************-}

-- Create a function param.
foreign import ccall "gcc_jit_context_new_param" gcc_jit_context_new_param :: Ptr Context -> Ptr Location -> Ptr Type -> CString -> IO (Ptr Param)
contextNewParam :: Ptr Context -> Ptr Location -> Ptr Type -> String -> IO (Ptr Param)
contextNewParam ctxt loc type' name = do
    c_name <- newCString name
    gcc_jit_context_new_param ctxt loc type' c_name

-- Upcasting from param to object.
foreign import ccall "gcc_jit_param_as_object" paramAsObject :: Ptr Param -> IO (Ptr Object)

-- Upcasting from param to lvalue.
foreign import ccall "gcc_jit_param_as_lvalue" paramAsLValue :: Ptr Param -> IO (Ptr LValue)

-- Upcasting from param to rvalue.
foreign import ccall "gcc_jit_param_as_rvalue" paramAsRValue :: Ptr Param -> IO (Ptr RValue)

-- Kinds of function.
data FunctionKind = 
    {- Function is defined by the client code and visible
       by name outside of the JIT. -}
    FunctionExported

    {- Function is defined by the client code, but is invisible
       outside of the JIT.  Analogous to a "static" function. -}
    | FunctionInternal

    {- Function is not defined by the client code; we're merely
       referring to it.  Analogous to using an "extern" function from a
       header file. -}
    | FunctionImported

    {- Function is only ever inlined into other functions, and is
       invisible outside of the JIT.

       Analogous to prefixing with "inline" and adding
       __attribute__((always_inline)).

       Inlining will only occur when the optimization level is
       above 0; when optimization is off, this is essentially the
       same as GCC_JIT_FUNCTION_INTERNAL. -}
    | FunctionAlwaysInline
    deriving (Enum)

-- Thread local storage model.
data TLSModel = TSLNone
    | TLSGlobalDynamic
    | TLSLocalDynamic
    | TLSInitialExec
    | TLSLocalExec
    deriving (Enum)

-- Create a function.
foreign import ccall "gcc_jit_context_new_function" gcc_jit_context_new_function :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> CString -> CInt -> Ptr (Ptr Param) -> CInt -> IO (Ptr Function)
contextNewFunction :: Ptr Context -> Ptr Location -> FunctionKind -> Ptr Type -> String -> [Ptr Param] -> Bool -> IO (Ptr Function)
contextNewFunction ctxt loc kind returnType name params isVariadic = do
    c_name <- newCString name
    c_params <- listToPtr params
    gcc_jit_context_new_function ctxt loc (fromIntegral $ fromEnum kind) returnType c_name (fromIntegral $ length params) c_params $ fromIntegral $ fromEnum isVariadic

-- Create a reference to a builtin function (sometimes called intrinsic functions).
foreign import ccall "gcc_jit_context_get_builtin_function" gcc_jit_context_get_builtin_function :: Ptr Context -> CString -> IO (Ptr Function)
contextGetBuiltinFunction :: Ptr Context -> String -> IO (Maybe (Ptr Function))
contextGetBuiltinFunction ctxt name = do
    c_name <- newCString name
    ptrToMaybe <$> gcc_jit_context_get_builtin_function ctxt c_name

-- Upcasting from function to object.
foreign import ccall "gcc_jit_function_as_object" functionAsObject :: Ptr Function -> IO (Ptr Object)

-- Get a specific param of a function by index.
foreign import ccall "gcc_jit_function_get_param" gcc_jit_function_get_param :: Ptr Function -> CInt -> IO (Ptr Object)
functionGetParam :: Ptr Function -> Int -> IO (Maybe (Ptr Object))
functionGetParam func = fmap ptrToMaybe . gcc_jit_function_get_param func . fromIntegral

-- Emit the function in graphviz format.
foreign import ccall "gcc_jit_function_dump_to_dot" gcc_jit_function_dump_to_dot :: Ptr Function -> CString -> IO ()
functionDumpToDot :: Ptr Function -> String -> IO ()
functionDumpToDot func path = do
    c_path <- newCString path
    gcc_jit_function_dump_to_dot func c_path

{-
   Create a block.

   The name can be Nothing, or you can give it a meaningful name, which
   may show up in dumps of the internal representation, and in error
   messages.
-}

foreign import ccall "gcc_jit_function_new_block" gcc_jit_function_new_block :: Ptr Function -> CString -> IO (Ptr Block)
functionNewBlock :: Ptr Function -> Maybe String -> IO (Maybe (Ptr Block))
functionNewBlock func (Just name) = do
    c_name <- newCString name
    ptrToMaybe <$> gcc_jit_function_new_block func c_name
functionNewBlock func Nothing = ptrToMaybe <$> gcc_jit_function_new_block func nullPtr

-- Upcasting from block to object.
foreign import ccall "gcc_jit_block_as_object" blockAsObject :: Ptr Block -> IO (Ptr Object)

-- Which function is this block within?
foreign import ccall "gcc_jit_block_get_function" blockGetFunction :: Ptr Block -> IO (Ptr Function)

{-*********************************************************************
 lvalues, rvalues and expressions.
 *********************************************************************-}
data GlobalKind = 
    {- Global is defined by the client code and visible
       by name outside of this JIT context via gcc_jit_result_get_global. -}
    GlobalExported

    {- Global is defined by the client code, but is invisible
       outside of this JIT context.  Analogous to a "static" global. -}
    | GlobalInternal

    {- Global is not defined by the client code; we're merely
       referring to it.  Analogous to using an "extern" global from a
       header file. -}
    | GlobalImported
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_global" gcc_jit_context_new_global :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> CString -> IO (Ptr LValue)
contextNewGlobal :: Ptr Context -> Ptr Location -> GlobalKind -> Ptr Type -> String -> IO (Ptr LValue)
contextNewGlobal ctxt loc kind type' name = do
    c_name <- newCString name
    gcc_jit_context_new_global ctxt loc (fromIntegral $ fromEnum kind) type' c_name

#ifdef LIBGCCJIT_HAVE_CTORS

{-
   Create a constructor for a struct as an rvalue.

   Returns Nothing on error.  The two parameter arrays are copied and
   do not have to outlive the context.

   `type` specifies what the constructor will build and has to be
   a struct.

   `num_values` specifies the number of elements in `values`.

   `fields` need to have the same length as `values`, or be Nothing.

   If `fields` is null, the values are applied in definition order.

   Otherwise, each field in `fields` specifies which field in the struct to
   set to the corresponding value in `values`.  `fields` and `values`
   are paired by index.

   Each value has to have the same unqualified type as the field
   it is applied to.

   A Nothing value element  in `values` is a shorthand for zero initialization
   of the corresponding field.

   The fields in `fields` have to be in definition order, but there
   can be gaps.  Any field in the struct that is not specified in
   `fields` will be zeroed.

   The fields in `fields` need to be the same objects that were used
   to create the struct.

   If `num_values` is 0, the array parameters will be
   ignored and zero initialization will be used.

   The constructor rvalue can be used for assignment to locals.
   It can be used to initialize global variables with
   gcc_jit_global_set_initializer_rvalue.  It can also be used as a
   temporary value for function calls and return values.

   The constructor can contain nested constructors.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
   #ifdef LIBGCCJIT_HAVE_CTORS
-}

foreign import ccall "gcc_jit_context_new_struct_constructor" gcc_jit_context_new_struct_constructor :: Ptr Context -> Ptr Location -> Ptr Type -> CSize -> Ptr (Ptr Field) -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewStructConstructor :: Ptr Context -> Ptr Location -> Ptr Type -> Either [Ptr RValue] [(Ptr Field, Maybe (Ptr RValue))] -> IO (Maybe (Ptr RValue))
contextNewStructConstructor ctxt loc type' (Left values) = do
    c_values <- listToPtr values
    ptrToMaybe <$> gcc_jit_context_new_struct_constructor ctxt loc type' (fromIntegral $ length values) nullPtr c_values
contextNewStructConstructor ctxt loc type' (Right fieldValues) = do
    (c_fields, c_values) <- uncurry (liftA2 (,)) $ bimap listToPtr (listToPtr . map maybeToPtr) $ unzipPairs fieldValues 
    ptrToMaybe <$> gcc_jit_context_new_struct_constructor ctxt loc type' (fromIntegral $ length fieldValues) c_fields c_values

{- 
   Create a constructor for a union as an rvalue.

   Returns Nothing on error.

   `type` specifies what the constructor will build and has to be
   an union.

   `field` specifies which field to set.  If it is Nothing, the first
   field in the union will be set.  `field` need to be the same
   object that were used to create the union.

   `value` specifies what value to set the corresponding field to.
   If `value` is Nothing, zero initialization will be used.

   Each value has to have the same unqualified type as the field
   it is applied to.

   `field` need to be the same objects that were used
   to create the union.

   The constructor rvalue can be used for assignment to locals.
   It can be used to initialize global variables with
   gcc_jit_global_set_initializer_rvalue.  It can also be used as a
   temporary value for function calls and return values.

   The constructor can contain nested constructors.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
   #ifdef LIBGCCJIT_HAVE_CTORS
-}

foreign import ccall "gcc_jit_context_new_union_constructor" gcc_jit_context_new_union_constructor :: Ptr Context -> Ptr Location -> Ptr Type -> Ptr Field -> Ptr RValue -> IO (Ptr RValue)
contextNewUnionConstructor :: Ptr Context -> Ptr Location -> Ptr Type -> Maybe (Ptr Field) -> Maybe (Ptr RValue) -> IO (Maybe (Ptr RValue))
contextNewUnionConstructor ctxt loc type' field value = 
    ptrToMaybe <$> gcc_jit_context_new_union_constructor ctxt loc type' (maybeToPtr field) (maybeToPtr value)

{-
   Create a constructor for an array as an rvalue.

   Returns Nothing on error.  `values` are copied and
   do not have to outlive the context.

   `type` specifies what the constructor will build and has to be
   an array.

   `num_values` specifies the number of elements in `values` and
   it can't have more elements than the array type.

   Each value in `values` sets the corresponding value in the array.
   If the array type itself has more elements than `values`, the
   left-over elements will be zeroed.

   Each value in `values` need to be the same unqualified type as the
   array type's element type.

   If `num_values` is 0, the `values` parameter will be
   ignored and zero initialization will be used.

   Note that a string literal rvalue can't be used to construct a char
   array.  It needs one rvalue for each char.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
   #ifdef LIBGCCJIT_HAVE_CTORS
-}

foreign import ccall "gcc_jit_context_new_array_constructor" gcc_jit_context_new_array_constructor :: Ptr Context -> Ptr Location -> Ptr Type -> CSize -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewArrayConstructor :: Ptr Context -> Ptr Location -> Ptr Type -> [Ptr RValue] -> IO (Maybe (Ptr RValue))
contextNewArrayConstructor ctxt loc type' values = do
    c_values <- listToPtr values
    ptrToMaybe <$> gcc_jit_context_new_array_constructor ctxt loc type' (fromIntegral $ length values) c_values

{-
   Set the initial value of a global of any type with an rvalue.

   The rvalue needs to be a constant expression, e.g. no function calls.

   The global can't have the 'kind' GCC_JIT_GLOBAL_IMPORTED.

   Use together with gcc_jit_context_new_constructor () to
   initialize structs, unions and arrays.

   On success, returns the 'global' parameter unchanged.  Otherwise, Nothing.

   'values' is copied and does not have to outlive the context.

   This entrypoint was added in LIBGCCJIT_ABI_19; you can test for its
   presence using:
     #ifdef LIBGCCJIT_HAVE_CTORS
-}

foreign import ccall "gcc_jit_global_set_initializer_rvalue" gcc_jit_global_set_initializer_rvalue :: Ptr LValue -> Ptr RValue -> IO (Ptr LValue)
globalSetInitializerRValue :: Ptr LValue -> Ptr RValue -> IO (Maybe (Ptr LValue))
globalSetInitializerRValue global = fmap ptrToMaybe . gcc_jit_global_set_initializer_rvalue global

#endif -- LIBGCCJIT_HAVE_CTORS

{-
   Set an initial value for a global, which must be an array of
   integral type.  Return the global itself.
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_global_set_initializer
foreign import ccall "gcc_jit_global_set_initializer" gcc_jit_global_set_initializer :: Ptr LValue -> Ptr a -> CSize -> IO (Ptr LValue)
globalSetInitializer :: Storable a => Ptr LValue -> Ptr a -> Int -> IO (Ptr LValue)
globalSetInitializer global ptr = gcc_jit_global_set_initializer global ptr . fromIntegral
#endif

-- Upcasting.
foreign import ccall "gcc_jit_lvalue_as_object" lValueAsObject :: Ptr LValue -> IO (Ptr Object)
foreign import ccall "gcc_jit_rvalue_as_object" rValueAsObject :: Ptr RValue -> IO (Ptr Object)
foreign import ccall "gcc_jit_lvalue_as_rvalue" lValueAsRValue :: Ptr LValue -> IO (Ptr RValue)
foreign import ccall "gcc_jit_rvalue_get_type" rValueGetType :: Ptr RValue -> IO (Ptr Type)

-- Integer constants.
foreign import ccall "gcc_jit_context_zero" contextZero :: Ptr Context -> Ptr Type -> IO (Ptr RValue)
foreign import ccall "gcc_jit_context_one" contextOne :: Ptr Context -> Ptr Type -> IO (Ptr RValue)

foreign import ccall "gcc_jit_context_new_rvalue_from_int" gcc_jit_context_new_rvalue_from_int :: Ptr Context -> Ptr Type -> CInt -> IO (Ptr RValue)
contextNewRValueFromInt :: Ptr Context -> Ptr Type -> Int -> IO (Ptr RValue)
contextNewRValueFromInt ctxt numericType = gcc_jit_context_new_rvalue_from_int ctxt numericType . fromIntegral

foreign import ccall "gcc_jit_context_new_rvalue_from_long" gcc_jit_context_new_rvalue_from_long :: Ptr Context -> Ptr Type -> CLong -> IO (Ptr RValue)
contextNewRValueFromLong :: Ptr Context -> Ptr Type -> Int64 -> IO (Ptr RValue)
contextNewRValueFromLong ctxt numericType = gcc_jit_context_new_rvalue_from_long ctxt numericType . fromIntegral

-- Floating-point constants.
foreign import ccall "gcc_jit_context_new_rvalue_from_double" gcc_jit_context_new_rvalue_from_double :: Ptr Context -> Ptr Type -> CDouble -> IO (Ptr RValue)
contextNewRValueFromDouble :: Ptr Context -> Ptr Type -> Double -> IO (Ptr RValue)
contextNewRValueFromDouble ctxt numericType = gcc_jit_context_new_rvalue_from_double ctxt numericType . realToFrac

-- Pointers.
foreign import ccall "gcc_jit_context_new_rvalue_from_ptr" contextNewRValueFromPtr :: Ptr Context -> Ptr Type -> Ptr a -> IO (Ptr RValue)
foreign import ccall "gcc_jit_context_null" contextNull :: Ptr Context -> Ptr Type -> IO (Ptr RValue)

-- String literals.
foreign import ccall "gcc_jit_context_new_string_literal" gcc_jit_context_new_string_literal :: Ptr Context -> CString -> IO (Ptr RValue)
contextNewStringLiteral :: Ptr Context -> String -> IO (Ptr RValue)
contextNewStringLiteral ctxt value = do
    c_value <- newCString value
    gcc_jit_context_new_string_literal ctxt c_value

data UnaryOp = 
    {- Negate an arithmetic value; analogous to:
       -(EXPR)
       in C. -}
    UnaryMinus

    {- Bitwise negation of an integer value (one's complement); analogous
       to:
         ~(EXPR)
       in C. -}
    | UnaryBitwiseNegate

    {- Logical negation of an arithmetic or pointer value; analogous to:
         !(EXPR)
       in C. -}
    | UnaryLogicalNegate

    {- Absolute value of an arithmetic expression; analogous to:
         abs (EXPR)
       in C. -}
    | UnaryAbs
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_unary_op" gcc_jit_context_new_unary_op :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> Ptr RValue -> IO (Ptr RValue)
contextNewUnaryOp :: Ptr Context -> Ptr Location -> UnaryOp -> Ptr Type -> Ptr RValue -> IO (Ptr RValue)
contextNewUnaryOp ctxt loc op = gcc_jit_context_new_unary_op ctxt loc $ fromIntegral $ fromEnum op

data BinaryOp = 
    {- Addition of arithmetic values; analogous to:
         (EXPR_A) + (EXPR_B)
       in C.
       For pointer addition, use gcc_jit_context_new_array_access. -}
    BinaryPlus

    {- Subtraction of arithmetic values; analogous to:
         (EXPR_A) - (EXPR_B)
       in C. -}
    | BinaryMinus

    {- Multiplication of a pair of arithmetic values; analogous to:
         (EXPR_A) * (EXPR_B)
       in C. -}
    | BinaryMult

    {- Quotient of division of arithmetic values; analogous to:
         (EXPR_A) / (EXPR_B)
       in C.
       The result type affects the kind of division: if the result type is
       integer-based, then the result is truncated towards zero, whereas
       a floating-point result type indicates floating-point division. -}
    | BinaryDivide

    {- Remainder of division of arithmetic values; analogous to:
         (EXPR_A) % (EXPR_B)
       in C. -}
    | BinaryModulo

    {- Bitwise AND; analogous to:
         (EXPR_A) & (EXPR_B)
       in C. -}
    | BinaryBitwiseAnd

    {- Bitwise exclusive OR; analogous to:
         (EXPR_A) ^ (EXPR_B)
       in C. -}
    | BinaryBitwiseXor

    {- Bitwise inclusive OR; analogous to:
         (EXPR_A) | (EXPR_B)
       in C. -}
    | BinaryBitwiseOr

    {- Logical AND; analogous to:
         (EXPR_A) && (EXPR_B)
       in C. -}
    | BinaryLogicalAnd

    {- Logical OR; analogous to:
         (EXPR_A) || (EXPR_B)
       in C. -}
    | BinaryLogicalOr

    {- Left shift; analogous to:
         (EXPR_A) << (EXPR_B)
       in C. -}
    | BinaryLShift

    {- Right shift; analogous to:
         (EXPR_A) >> (EXPR_B)
       in C. -}
    | BinaryRShift
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_binary_op" gcc_jit_context_new_binary_op :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewBinaryOp :: Ptr Context -> Ptr Location -> BinaryOp -> Ptr Type -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewBinaryOp ctxt loc op = gcc_jit_context_new_binary_op ctxt loc $ fromIntegral $ fromEnum op

-- (Comparisons are treated as separate from "binary_op" to save you having to specify the result_type).
data Comparison = 
    -- (EXPR_A) == (EXPR_B).
    ComparisonEq
    
    -- (EXPR_A) != (EXPR_B).
    | ComparisonNe

    -- (EXPR_A) < (EXPR_B).
    | ComparisonLt

    -- (EXPR_A) <=(EXPR_B).
    | ComparisonLe

    -- (EXPR_A) > (EXPR_B).
    | ComparisonGt

    -- (EXPR_A) >= (EXPR_B).
    | ComparisonGe
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_comparison" gcc_jit_context_new_comparison :: Ptr Context -> Ptr Location -> CInt -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewComparison :: Ptr Context -> Ptr Location -> Comparison -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewComparison ctxt  loc op = gcc_jit_context_new_comparison ctxt loc $ fromIntegral $ fromEnum op

-- Function calls.

-- Call of a specific function.
foreign import ccall "gcc_jit_context_new_call" gcc_jit_context_new_call :: Ptr Context -> Ptr Location -> Ptr Function -> CInt -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewCall :: Ptr Context -> Ptr Location -> Ptr Function -> [Ptr RValue] -> IO (Ptr RValue)
contextNewCall ctxt loc func args = do
    c_args <- listToPtr args
    gcc_jit_context_new_call ctxt loc func (fromIntegral $ length args) c_args

-- Call through a function pointer.
foreign import ccall "gcc_jit_context_new_call_through_ptr" gcc_jit_context_new_call_through_ptr :: Ptr Context -> Ptr Location -> Ptr RValue -> CInt -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewCallThroughPtr :: Ptr Context -> Ptr Location -> Ptr RValue -> [Ptr RValue] -> IO (Ptr RValue)
contextNewCallThroughPtr ctxt loc fnPtr args = do
    c_args <- listToPtr args
    gcc_jit_context_new_call_through_ptr ctxt loc fnPtr (fromIntegral $ length args) c_args

{-
   Type-coercion.

   Currently only a limited set of conversions are possible:
     int <-> float
     int <-> bool
-}

foreign import ccall "gcc_jit_context_new_cast" contextNewCast :: Ptr Context -> Ptr Location -> Ptr RValue -> Ptr Type -> IO (Ptr RValue)

{-
   Reinterpret a value as another type.

   The types must be of the same size.
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast
foreign import ccall "gcc_jit_context_new_bitcast" contextNewBitcast :: Ptr Context -> Ptr Location -> Ptr RValue -> Ptr Type -> IO (Ptr RValue)
#endif

#ifdef LIBGCCJIT_HAVE_ALIGNMENT

-- Set the alignment of a variable.
foreign import ccall "gcc_jit_lvalue_set_alignment" gcc_jit_lvalue_set_alignment :: Ptr LValue -> CUInt -> IO ()
lValueSetAlignment :: Ptr LValue -> Int -> IO ()
lValueSetAlignment lvalue = gcc_jit_lvalue_set_alignment lvalue . fromIntegral


--Get the alignment of a variable.
foreign import ccall "gcc_jit_lvalue_get_alignment" gcc_jit_lvalue_get_alignment :: Ptr LValue -> IO CUInt
lValueGetAlignment :: Ptr LValue -> IO Int
lValueGetAlignment = fmap fromIntegral . gcc_jit_lvalue_get_alignment

#endif

foreign import ccall "gcc_jit_context_new_array_access" contextNewArrayAccess :: Ptr Context -> Ptr Location -> Ptr RValue -> Ptr RValue -> IO (Ptr LValue)

-- Field access is provided separately for both lvalues and rvalues.

{-
   Accessing a field of an lvalue of struct type, analogous to:
      (EXPR).field = ...;
   in C.
-}

foreign import ccall "gcc_jit_lvalue_access_field" lValueAccessField :: Ptr LValue -> Ptr Location -> Ptr Field -> IO (Ptr LValue)

{-
   Accessing a field of an rvalue of struct type, analogous to:
      (EXPR).field
   in C.
-}

foreign import ccall "gcc_jit_rvalue_access_field" rValueAccessField :: Ptr RValue -> Ptr Location -> Ptr Field -> IO (Ptr RValue)

{-
   Accessing a field of an rvalue of pointer type, analogous to:
      (EXPR)->field
   in C, itself equivalent to (*EXPR).FIELD
-}

foreign import ccall "gcc_jit_rvalue_dereference_field" rValueDereferenceField :: Ptr RValue -> Ptr Location -> Ptr Field -> IO (Ptr LValue)

{-
   Dereferencing a pointer; analogous to:
     *(EXPR)
-}

foreign import ccall "gcc_jit_rvalue_dereference" rValueDereference :: Ptr RValue -> Ptr Location -> IO (Ptr LValue)

{-
   Taking the address of an lvalue; analogous to:
     &(EXPR)
   in C.
-}

foreign import ccall "gcc_jit_lvalue_get_address" lValueGetAddress :: Ptr LValue -> Ptr Location -> IO (Ptr RValue)

-- Set the thread-local storage model of a global variable
#ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_tls_model
foreign import ccall "gcc_jit_lvalue_set_tls_model" gcc_jit_lvalue_set_tls_model :: Ptr LValue -> CInt -> IO ()
lValueSetTLSModel :: Ptr LValue -> TLSModel -> IO ()
lValueSetTLSModel lvalue = gcc_jit_lvalue_set_tls_model lvalue . fromIntegral . fromEnum
#endif

{-
   Set the link section of a global variable; analogous to:
     __attribute__((section(".section_name")))
   in C.
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_link_section
foreign import ccall "gcc_jit_lvalue_set_link_section" gcc_jit_lvalue_set_link_section :: Ptr LValue -> CString -> IO ()
lValueSetLinkSection :: Ptr LValue -> String -> IO ()
lValueSetLinkSection lvalue sectionName = do
    c_sectionName <- newCString sectionName
    gcc_jit_lvalue_set_link_section lvalue c_sectionName
#endif

-- Make this variable a register variable and set its register name.

#ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_register_name
foreign import ccall "gcc_jit_lvalue_set_register_name" gcc_jit_lvalue_set_register_name :: Ptr LValue -> CString -> IO ()
lValueSetRegisterName :: Ptr LValue -> String -> IO ()
lValueSetRegisterName lvalue registerName = do
    c_registerName <- newCString registerName
    gcc_jit_lvalue_set_register_name lvalue c_registerName
#endif

foreign import ccall "gcc_jit_function_new_local" gcc_jit_function_new_local :: Ptr Function -> Ptr Location -> Ptr Type -> CString -> IO (Ptr LValue)
functionNewLocal :: Ptr Function -> Ptr Location -> Ptr Type -> String -> IO (Ptr LValue)
functionNewLocal func loc type' name = do
    c_name <- newCString name
    gcc_jit_function_new_local func loc type' c_name

{-*********************************************************************
 Statement-creation.
 *********************************************************************-}

{-
   Add evaluation of an rvalue, discarding the result
   (e.g. a function call that "returns" void).

   This is equivalent to this C code:

     (void)expression;
-}

foreign import ccall "gcc_jit_block_add_eval" blockAddEval :: Ptr Block -> Ptr Location -> Ptr RValue -> IO ()

{-
   Add evaluation of an rvalue, assigning the result to the given
   lvalue.

   This is roughly equivalent to this C code:

     lvalue = rvalue;
-}

foreign import ccall "gcc_jit_block_add_assignment" blockAddAssignment :: Ptr Block -> Ptr Location -> Ptr LValue -> Ptr RValue -> IO ()

{-
   Add evaluation of an rvalue, using the result to modify an
   lvalue.

   This is analogous to "+=" and friends:

     lvalue += rvalue;
     lvalue *= rvalue;
     lvalue /= rvalue;
   etc
-}

foreign import ccall "gcc_jit_block_add_assignment_op" gcc_jit_block_add_assignment_op :: Ptr Block -> Ptr Location -> Ptr LValue -> CInt -> Ptr RValue -> IO ()
blockAddAssignmentOp :: Ptr Block -> Ptr Location -> Ptr LValue -> BinaryOp -> Ptr RValue -> IO ()
blockAddAssignmentOp block loc lvalue = gcc_jit_block_add_assignment_op block loc lvalue . fromIntegral . fromEnum

{-
   Add a no-op textual comment to the internal representation of the
   code.  It will be optimized away, but will be visible in the dumps
   seen via
     GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE
   and
     GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE,
   and thus may be of use when debugging how your project's internal
   representation gets converted to the libgccjit IR.
-}

foreign import ccall "gcc_jit_block_add_comment" gcc_jit_block_add_comment :: Ptr Block -> Ptr Location -> CString -> IO ()
blockAddComment :: Ptr Block -> Ptr Location -> String -> IO ()
blockAddComment block loc text = do
    c_text <- newCString text
    gcc_jit_block_add_comment block loc c_text

{-
   Terminate a block by adding evaluation of an rvalue, branching on the
   result to the appropriate successor block.

   This is roughly equivalent to this C code:

     if (boolval)
       goto on_true;
     else
       goto on_false;

   block, boolval, on_true, and on_false must be non-Nothing.
-}

foreign import ccall "gcc_jit_block_end_with_conditional" blockEndWithConditional :: Ptr Block -> Ptr Location -> Ptr RValue -> Ptr Block -> Ptr Block -> IO ()

{-
   Terminate a block by adding a jump to the given target block.

   This is roughly equivalent to this C code:

      goto target;
-}

foreign import ccall "gcc_jit_block_end_with_jump" blockEndWithJump :: Ptr Block -> Ptr Location -> Ptr Block -> IO ()

{-
   Terminate a block by adding evaluation of an rvalue, returning the value.

   This is roughly equivalent to this C code:

      return expression;
-}

foreign import ccall "gcc_jit_block_end_with_return" blockEndWithReturn :: Ptr Block -> Ptr Location -> Ptr RValue -> IO ()

{-
   Terminate a block by adding a valueless return, for use within a function
   with "void" return type.

   This is equivalent to this C code:

      return;
-}

foreign import ccall "gcc_jit_block_end_with_void_return" blockEndWithVoidReturn :: Ptr Block -> Ptr Location -> IO ()

{-
   Create a new gcc_jit_case instance for use in a switch statement.
   min_value and max_value must be constants of integer type.
-}

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS

foreign import ccall "gcc_jit_context_new_case" contextNewCase :: Ptr Context -> Ptr RValue -> Ptr RValue -> Ptr Block -> IO (Ptr Case)

-- Upcasting from case to object.
foreign import ccall "gcc_jit_case_as_object" caseAsObject :: Ptr Case -> IO (Ptr Object)

{-
   Terminate a block by adding evalation of an rvalue, then performing
   a multiway branch.

   This is roughly equivalent to this C code:

     switch (expr)
       {
       default:
	 goto default_block;

       case C0.min_value ... C0.max_value:
	 goto C0.dest_block;

       case C1.min_value ... C1.max_value:
	 goto C1.dest_block;

       ...etc...

       case C[N - 1].min_value ... C[N - 1].max_value:
	 goto C[N - 1].dest_block;
     }

   block, expr, default_block and cases must all be non-Nothing.

   expr must be of the same integer type as all of the min_value
   and max_value within the cases.

   num_cases must be >= 0.

   The ranges of the cases must not overlap (or have duplicate
   values).
-}

foreign import ccall "gcc_jit_block_end_with_switch" gcc_jit_block_end_with_switch :: Ptr Block -> Ptr Location -> Ptr RValue -> Ptr Block -> CInt -> Ptr (Ptr Case) -> IO ()
blockEndWithSwitch :: Ptr Block -> Ptr Location -> Ptr RValue -> Ptr Block -> [Ptr Case] -> IO ()
blockEndWithSwitch block loc expr defaultBlock cases = do
    c_cases <- listToPtr cases
    gcc_jit_block_end_with_switch block loc expr defaultBlock (fromIntegral $ length cases) c_cases

#endif

{-*********************************************************************
 Nested contexts.
 *********************************************************************-}

{-
   Given an existing JIT context, create a child context.

   The child inherits a copy of all option-settings from the parent.

   The child can reference objects created within the parent, but not
   vice-versa.

   The lifetime of the child context must be bounded by that of the
   parent: you should release a child context before releasing the parent
   context.

   If you use a function from a parent context within a child context,
   you have to compile the parent context before you can compile the
   child context, and the gcc_jit_result of the parent context must
   outlive the gcc_jit_result of the child context.

   This allows caching of shared initializations.  For example, you could
   create types and declarations of global functions in a parent context
   once within a process, and then create child contexts whenever a
   function or loop becomes hot. Each such child context can be used for
   JIT-compiling just one function or loop, but can reference types
   and helper functions created within the parent context.

   Contexts can be arbitrarily nested, provided the above rules are
   followed, but it's probably not worth going above 2 or 3 levels, and
   there will likely be a performance hit for such nesting.
-}

foreign import ccall "gcc_jit_context_new_child_context" gcc_jit_context_new_child_context :: Ptr Context -> IO (Ptr Context)
contextNewChildContext :: Ptr Context -> IO (Maybe (Ptr Context))
contextNewChildContext = fmap ptrToMaybe . gcc_jit_context_new_child_context

{-*********************************************************************
 Implementation support.
 *********************************************************************-}

{-
   Write C source code into "path" that can be compiled into a
   self-contained executable (i.e. with libgccjit as the only dependency).
   The generated code will attempt to replay the API calls that have been
   made into the given context.

   This may be useful when debugging the library or client code, for
   reducing a complicated recipe for reproducing a bug into a simpler
   form.

   Typically you need to supply the option "-Wno-unused-variable" when
   compiling the generated file (since the result of each API call is
   assigned to a unique variable within the generated C source, and not
   all are necessarily then used).
-}

foreign import ccall "gcc_jit_context_dump_reproducer_to_file" gcc_jit_context_dump_reproducer_to_file :: Ptr Context -> CString -> IO ()
contextDumpReproducerToFile :: Ptr Context -> String -> IO ()
contextDumpReproducerToFile ctxt path = do
    c_path <- newCString path
    gcc_jit_context_dump_reproducer_to_file ctxt c_path

{-
   Enable the dumping of a specific set of internal state from the
   compilation, capturing the result in-memory as a buffer.

   Parameter "dumpname" corresponds to the equivalent gcc command-line
   option, without the "-fdump-" prefix.
   For example, to get the equivalent of "-fdump-tree-vrp1", supply
   "tree-vrp1".
   The context directly stores the dumpname as a (const char *), so the
   passed string must outlive the context.

   gcc_jit_context_compile and gcc_jit_context_to_file
   will capture the dump as a dynamically-allocated buffer, writing
   it to ``*out_ptr``.

   The caller becomes responsible for calling
      free (*out_ptr)
   each time that gcc_jit_context_compile or gcc_jit_context_to_file
   are called.  *out_ptr will be written to, either with the address of a
   buffer, or with Nothing if an error occurred.

   This API entrypoint is likely to be less stable than the others.
   In particular, both the precise dumpnames, and the format and content
   of the dumps are subject to change.

   It exists primarily for writing the library's own test suite.
-}

foreign import ccall "gcc_jit_context_enable_dump" gcc_jit_context_enable_dump :: Ptr Context -> CString -> Ptr CString -> IO ()
contextEnableDump :: Ptr Context -> String -> Ptr CString -> IO ()
contextEnableDump ctxt dumpName outPtr = do
    c_dumpName <- newCString dumpName
    gcc_jit_context_enable_dump ctxt c_dumpName outPtr

{-*********************************************************************
 Timing support.
 *********************************************************************-}

#ifdef LIBGCCJIT_HAVE_TIMING_API

-- Create a gcc_jit_timer instance, and start timing.
foreign import ccall "gcc_jit_timer_new" timerNew :: IO (Ptr Timer)

-- Release a gcc_jit_timer instance.
foreign import ccall "gcc_jit_timer_release" timerRelease :: Ptr Timer -> IO ()

-- Associate a gcc_jit_timer instance with a context.
foreign import ccall "gcc_jit_context_set_timer" contextSetTimer :: Ptr Context -> Ptr Timer -> IO ()

-- Get the timer associated with a context (if any).
foreign import ccall "gcc_jit_context_get_timer" gcc_jit_context_get_timer :: Ptr Context -> IO (Ptr Timer)
contextGetTimer :: Ptr Context -> IO (Maybe (Ptr Timer))
contextGetTimer = fmap ptrToMaybe . gcc_jit_context_get_timer

-- Push the given item onto the timing stack.
foreign import ccall "gcc_jit_timer_push" gcc_jit_timer_push :: Ptr Timer -> CString -> IO ()
timerPush :: Ptr Timer -> String -> IO ()
timerPush timer itemName = do
    c_itemName <- newCString itemName
    gcc_jit_timer_push timer c_itemName

-- Pop the top item from the timing stack.
foreign import ccall "gcc_jit_timer_pop" gcc_jit_timer_pop :: Ptr Timer -> CString -> IO ()
timerPop :: Ptr Timer -> String -> IO ()
timerPop timer itemName = do
    c_itemName <- newCString itemName
    gcc_jit_timer_pop timer c_itemName

-- Print timing information to the given stream about activity since
-- the timer was started.
foreign import ccall "gcc_jit_timer_print" timerPrint :: Ptr Timer -> Ptr CFile -> IO ()

#endif -- LIBGCCJIT_HAVE_TIMING_API

-- Mark/clear a call as needing tail-call optimization.
#ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call
foreign import ccall "gcc_jit_rvalue_set_bool_require_tail_call" gcc_jit_rvalue_set_bool_require_tail_call :: Ptr RValue -> CInt -> IO ()
rValueSetBoolRequireTailCall :: Ptr RValue -> Bool -> IO ()
rValueSetBoolRequireTailCall call = gcc_jit_rvalue_set_bool_require_tail_call call . fromIntegral . fromEnum
#endif

{-
   Given type "T", get type:

     T __attribute__ ((aligned (ALIGNMENT_IN_BYTES)))

   The alignment must be a power of two.
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned
foreign import ccall "gcc_jit_type_get_aligned" gcc_jit_type_get_aligned :: Ptr Type -> CSize -> IO (Ptr Type)
typeGetAligned :: Ptr Type -> Int64 -> IO (Ptr Type)
typeGetAligned type' = gcc_jit_type_get_aligned type' . fromIntegral
#endif

{-
   Given type "T", get type:

     T  __attribute__ ((vector_size (sizeof(T) * num_units))

   T must be integral/floating point; num_units must be a power of two.
-}

#ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_vector
foreign import ccall "gcc_jit_type_get_vector" gcc_jit_type_get_vector :: Ptr Type -> CSize -> IO (Ptr Type)
typeGetVector :: Ptr Type -> Int64 -> IO (Ptr Type)
typeGetVector type' = gcc_jit_type_get_vector type' . fromIntegral
#endif

-- Get the address of a function as an rvalue, of function pointer type.
#ifdef LIBGCCJIT_HAVE_gcc_jit_function_get_address
foreign import ccall "gcc_jit_function_get_address" functionGetAddress :: Ptr Function -> Ptr Location -> Ptr RValue
#endif

-- Build a vector rvalue from an array of elements.
-- "vec_type" should be a vector type, created using gcc_jit_type_get_vector.
#ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_rvalue_from_vector
foreign import ccall "gcc_jit_context_new_rvalue_from_vector" gcc_jit_context_new_rvalue_from_vector :: Ptr Context -> Ptr Location -> Ptr Type -> CSize -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewRValueFromVector :: Ptr Context -> Ptr Location -> Ptr Type -> [Ptr RValue] -> IO (Ptr RValue)
contextNewRValueFromVector ctxt loc vecType elements = do
    c_elements <- listToPtr elements
    gcc_jit_context_new_rvalue_from_vector ctxt loc vecType (fromIntegral $ length elements) c_elements
#endif

-- Functions to retrieve libgccjit version.
-- Analogous to __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__ in C code.

#ifdef LIBGCCJIT_HAVE_gcc_jit_version
foreign import ccall "gcc_jit_version_major" gcc_jit_version_major :: IO CInt
versionMajor :: IO Int
versionMajor = fromIntegral <$> gcc_jit_version_major

foreign import ccall "gcc_jit_version_minor" gcc_jit_version_minor :: IO CInt
versionMinor :: IO Int
versionMinor = fromIntegral <$> gcc_jit_version_minor

foreign import ccall "gcc_jit_version_patchlevel" gcc_jit_version_patchlevel :: IO CInt
versionPatchlevel :: IO Int
versionPatchlevel = fromIntegral <$> gcc_jit_version_patchlevel

#endif -- LIBGCCJIT_HAVE_gcc_jit_version

{-*********************************************************************
 Asm support.
 *********************************************************************-}

#ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS

-- Create a gcc_jit_extended_asm for an extended asm statement
-- with no control flow (i.e. without the goto qualifier).
foreign import ccall "gcc_jit_block_add_extended_asm" gcc_jit_block_add_extended_asm :: Ptr Block -> Ptr Location -> CString -> IO (Ptr ExtendedAsm)
blockAddExtendedAsm :: Ptr Block -> Ptr Location -> String -> IO (Ptr ExtendedAsm)
blockAddExtendedAsm block loc asmTemplate = do
    c_asmTemplate <- newCString asmTemplate
    gcc_jit_block_add_extended_asm block loc c_asmTemplate

{-
   Create a gcc_jit_extended_asm for an extended asm statement
   that may perform jumps, and use it to terminate the given block.
   This is equivalent to the "goto" qualifier in C's extended asm
   syntax.
-}

foreign import ccall "gcc_jit_block_end_with_extended_asm_goto" gcc_jit_block_end_with_extended_asm_gogo :: Ptr Block -> Ptr Location -> CString -> CInt -> Ptr (Ptr Block) -> Ptr Block -> IO (Ptr ExtendedAsm)
blockEndWithExtendedAsmGoto :: Ptr Block -> Ptr Location -> String -> [Ptr Block] -> Ptr Block -> IO (Ptr ExtendedAsm)
blockEndWithExtendedAsmGoto block loc asmTemplate gotoBlocks fallthroughBlock = do
    c_asmTemplate <- newCString asmTemplate
    c_gotoBlocks <- listToPtr gotoBlocks
    gcc_jit_block_end_with_extended_asm_gogo block loc c_asmTemplate (fromIntegral $ length gotoBlocks) c_gotoBlocks fallthroughBlock

-- Upcasting from extended asm to object.
foreign import ccall "gcc_jit_extended_asm_as_object" extendedAsmAsObject :: Ptr ExtendedAsm -> IO (Ptr Object)

-- Set whether the gcc_jit_extended_asm has side-effects, equivalent to
-- the "volatile" qualifier in C's extended asm syntax.
foreign import ccall "gcc_jit_extended_asm_set_volatile_flag" gcc_jit_extended_asm_set_volatile_flag :: Ptr ExtendedAsm -> CInt -> IO ()
extendedAsmSetVolatileFlag :: Ptr ExtendedAsm -> Bool -> IO ()
extendedAsmSetVolatileFlag extAsm = gcc_jit_extended_asm_set_volatile_flag extAsm . fromIntegral . fromEnum

-- Set the equivalent of the "inline" qualifier in C's extended asm syntax.
foreign import ccall "gcc_jit_extended_asm_set_inline_flag" gcc_jit_extended_asm_set_inline_flag :: Ptr ExtendedAsm -> CInt -> IO ()
extendedAsmSetInlineFlag :: Ptr ExtendedAsm -> Bool -> IO ()
extendedAsmSetInlineFlag extAsm = gcc_jit_extended_asm_set_inline_flag extAsm . fromIntegral . fromEnum

{-
   Add an output operand to the extended asm statement.
   "asm_symbolic_name" can be Nothing.
   "constraint" and "dest" must be non-Nothing.
   This function can't be called on an "asm goto" as such instructions
   can't have outputs 
-}

foreign import ccall "gcc_jit_extended_asm_add_output_operand" gcc_jit_extended_asm_add_output_operand :: Ptr ExtendedAsm -> CString -> CString -> Ptr LValue -> IO ()
extendedAsmAddOutputOperand :: Ptr ExtendedAsm -> Maybe String -> String -> Ptr LValue -> IO ()
extendedAsmAddOutputOperand extAsm symbolicName constraint dest = do
    c_symbolicName <- case symbolicName of
        Nothing -> return nullPtr
        Just symbolicName' -> newCString symbolicName'
    c_contsraint <- newCString constraint
    gcc_jit_extended_asm_add_output_operand extAsm c_symbolicName c_contsraint dest

{-
   Add an input operand to the extended asm statement.
   "asm_symbolic_name" can be Nothing.
   "constraint" and "src" must be non-Nothing.
-}

foreign import ccall "gcc_jit_extended_asm_add_input_operand" gcc_jit_extended_asm_add_input_operand :: Ptr ExtendedAsm -> CString -> CString -> Ptr RValue -> IO ()
extendedAsmAddInputOperand :: Ptr ExtendedAsm -> Maybe String -> String -> Ptr RValue -> IO ()
extendedAsmAddInputOperand extAsm symbolicName constraint src = do
    c_symbolicName <- case symbolicName of
        Nothing -> return nullPtr
        Just symbolicName' -> newCString symbolicName'
    c_contsraint <- newCString constraint
    gcc_jit_extended_asm_add_input_operand extAsm c_symbolicName c_contsraint src

{-
   Add "victim" to the list of registers clobbered by the extended
   asm statement.  It must be non-Nothing.
-}

foreign import ccall "gcc_jit_extended_asm_add_clobber" gcc_jit_extended_asm_add_clobber :: Ptr ExtendedAsm -> CString -> IO ()
extendedAsmAddClobber :: Ptr ExtendedAsm -> String -> IO ()
extendedAsmAddClobber extAsm victim = do
    c_victim <- newCString victim
    gcc_jit_extended_asm_add_clobber extAsm c_victim

{-
   Add "asm_stmts", a set of top-level asm statements, analogous to
   those created by GCC's "basic" asm syntax in C at file scope.
-}

foreign import ccall "gcc_jit_context_add_top_level_asm" gcc_jit_context_add_top_level_asm :: Ptr Context -> Ptr Location -> CString -> IO ()
contextAddTopLevelAsm :: Ptr Context -> Ptr Location -> String -> IO ()
contextAddTopLevelAsm ctxt loc asmStmts = do
    c_asmStmts <- newCString asmStmts
    gcc_jit_context_add_top_level_asm ctxt loc c_asmStmts

#endif -- LIBGCCJIT_HAVE_ASM_STATEMENTS

{-
   Reflection functions to get the number of parameters, return type of
   a function and whether a type is a bool from the C API.
-}

#ifdef LIBGCCJIT_HAVE_REFLECTION

-- Get the return type of a function.
foreign import ccall "gcc_jit_function_get_return_type" functionGetReturnType :: Ptr Function -> IO (Ptr Type)

-- Get the number of params of a function.
foreign import ccall "gcc_jit_function_get_param_count" gcc_jit_function_get_param_count :: Ptr Function -> IO CSize
functionGetParamCount :: Ptr Function -> IO Int64
functionGetParamCount = fmap fromIntegral . gcc_jit_function_get_param_count

-- Get the element type of an array type or Nothing if it's not an array.
foreign import ccall "gcc_jit_type_dyncast_array" gcc_jit_type_dyncast_array :: Ptr Type -> IO (Ptr Type)
typeDyncastArray :: Ptr Type -> IO (Maybe (Ptr Type))
typeDyncastArray = fmap ptrToMaybe . gcc_jit_type_dyncast_array

-- Return False if the type is a bool.
foreign import ccall "gcc_jit_type_is_bool" gcc_jit_type_is_bool :: Ptr Type -> IO CInt
typeIsBool :: Ptr Type -> IO Bool
typeIsBool = fmap (0 /=) . gcc_jit_type_is_bool

-- Return the function type if it is one or Nothing.
foreign import ccall "gcc_jit_type_dyncast_function_ptr_type" gcc_jit_type_dyncast_function_ptr_type :: Ptr Type -> IO (Ptr FunctionType)
typeDyncastFunctionPtrType :: Ptr Type -> IO (Maybe (Ptr FunctionType))
typeDyncastFunctionPtrType = fmap ptrToMaybe . gcc_jit_type_dyncast_function_ptr_type

-- Given a function type, return its return type.
foreign import ccall "gcc_jit_function_type_get_return_type" functionTypeGetReturnType :: Ptr FunctionType -> IO (Ptr Type)

-- Given a function type, return its number of parameters.
foreign import ccall "gcc_jit_function_type_get_param_count" gcc_jit_function_type_get_param_count :: Ptr FunctionType -> IO CSize
functionTypeGetParamCount :: Ptr FunctionType -> IO Int64
functionTypeGetParamCount = fmap fromIntegral . gcc_jit_function_type_get_param_count

-- Given a function type, return the type of the specified parameter.
foreign import ccall "gcc_jit_function_type_get_param_type" gcc_jit_function_type_get_param_type :: Ptr FunctionType -> CSize -> IO (Ptr Type)
functionTypeGetParamType :: Ptr FunctionType -> Int64 -> IO (Maybe (Ptr Type))
functionTypeGetParamType fnType = fmap ptrToMaybe . gcc_jit_function_type_get_param_type fnType . fromIntegral

-- Return False if the type is an integral.
foreign import ccall "gcc_jit_type_is_integral" gcc_jit_type_is_integral :: Ptr Type -> IO CInt
typeIsIntegral :: Ptr Type -> IO Bool
typeIsIntegral = fmap (0 /=) . gcc_jit_type_is_integral

-- Return the type pointed by the pointer type or Nothing if it's not a pointer.
foreign import ccall "gcc_jit_type_is_pointer" gcc_jit_type_is_pointer :: Ptr Type -> IO (Ptr Type)
typeIsPointer :: Ptr Type -> IO (Maybe (Ptr Type))
typeIsPointer = fmap ptrToMaybe . gcc_jit_type_is_pointer

-- Given a type, return a dynamic cast to a vector type or Nothing.
foreign import ccall "gcc_jit_type_dyncast_vector" gcc_jit_type_dyncast_vector :: Ptr Type -> IO (Ptr VectorType)
typeDyncastVector :: Ptr Type -> IO (Maybe (Ptr VectorType))
typeDyncastVector = fmap ptrToMaybe . gcc_jit_type_dyncast_vector

-- Given a type, return a dynamic cast to a struct type or Nothing.
foreign import ccall "gcc_jit_type_is_struct" gcc_jit_type_is_struct :: Ptr Type -> IO (Ptr Struct)
typeIsStruct :: Ptr Type -> IO (Maybe (Ptr Struct))
typeIsStruct = fmap ptrToMaybe . gcc_jit_type_is_struct

-- Given a vector type, return the number of units it contains.
foreign import ccall "gcc_jit_vector_type_get_num_units" gcc_jit_vector_type_get_num_units :: Ptr VectorType -> IO CSize
vectorTypeGetNumUnits :: Ptr VectorType -> IO Int64
vectorTypeGetNumUnits = fmap fromIntegral . gcc_jit_vector_type_get_num_units

-- Given a vector type, return the type of its elements.
foreign import ccall "gcc_jit_vector_type_get_element_type" vectorTypeGetElementType :: Ptr VectorType -> IO (Ptr Type)

-- Given a type, return the unqualified type, removing "const", "volatile"
-- and alignment qualifiers.
foreign import ccall "gcc_jit_type_unqualified" typeUnqualified :: Ptr Type -> IO (Ptr Type)

#endif -- LIBGCCJIT_HAVE_REFLECTION
