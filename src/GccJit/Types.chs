#include <libgccjit.h>

module GccJit.Types (
    -- C structs
    Context,
    Result,
    Object,
    Location,
    Type,
    Field,
    Struct,
    FunctionType,
    VectorType,
    Function,
    Block,
    RValue,
    LValue,
    Param,
#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
    Case,
#endif
#ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS
    ExtendedAsm,
#endif
#ifdef LIBGCCJIT_HAVE_TIMING_API
    Timer,
#endif
) where

{-
 **********************************************************************
 Data structures.
 **********************************************************************
 All structs within the API are opaque.
-}

{- 
   A gcc_jit_context encapsulates the state of a compilation.
   You can set up options on it, and add types, functions and code, using
   the API below.

   Invoking gcc_jit_context_compile on it gives you a gcc_jit_result *
   (or Nothing), representing in-memory machine code.

   You can call gcc_jit_context_compile repeatedly on one context, giving
   multiple independent results.

   Similarly, you can call gcc_jit_context_compile_to_file on a context
   to compile to disk.

   Eventually you can call gcc_jit_context_release to clean up the
   context; any in-memory results created from it are still usable, and
   should be cleaned up via gcc_jit_result_release.
-}
data Context

-- A gcc_jit_result encapsulates the result of an in-memory compilation.
data Result

{- 
   An object created within a context.  Such objects are automatically
   cleaned up when the context is released.

   The class hierarchy looks like this:

     +- gcc_jit_object
	 +- gcc_jit_location
	 +- gcc_jit_type
	    +- gcc_jit_struct
	    +- gcc_jit_function_type
	    +- gcc_jit_vector_type
	 +- gcc_jit_field
	 +- gcc_jit_function
	 +- gcc_jit_block
	 +- gcc_jit_rvalue
	     +- gcc_jit_lvalue
		 +- gcc_jit_param
	 +- gcc_jit_case
	 +- gcc_jit_extended_asm
-}
data Object

{- 
   A gcc_jit_location encapsulates a source code location, so that
   you can (optionally) associate locations in your language with
   statements in the JIT-compiled code, allowing the debugger to
   single-step through your language.

   Note that to do so, you also need to enable
     GCC_JIT_BOOL_OPTION_DEBUGINFO
   on the gcc_jit_context.

   gcc_jit_location instances are optional; you can always pass
   Nothing.
-}
data Location

-- A gcc_jit_type encapsulates a type e.g. "int" or a "struct foo*".
data Type

{- 
   A gcc_jit_field encapsulates a field within a struct; it is used
   when creating a struct type (using gcc_jit_context_new_struct_type).
   Fields cannot be shared between structs.
-}
data Field

{-
   A gcc_jit_struct encapsulates a struct type, either one that we have
   the layout for, or an opaque type.
-}
data Struct

-- A gcc_jit_function_type encapsulates a function type.
data FunctionType

-- A gcc_jit_vector_type encapsulates a vector type.
data VectorType

{-
   A gcc_jit_function encapsulates a function: either one that you're
   creating yourself, or a reference to one that you're dynamically
   linking to within the rest of the process.
-}
data Function

{-
   A gcc_jit_block encapsulates a "basic block" of statements within a
   function (i.e. with one entry point and one exit point).

   Every block within a function must be terminated with a conditional,
   a branch, or a return.

   The blocks within a function form a directed graph.

   The entrypoint to the function is the first block created within
   it.

   All of the blocks in a function must be reachable via some path from
   the first block.

   It's OK to have more than one "return" from a function (i.e. multiple
   blocks that terminate by returning).
-}
data Block

-- A gcc_jit_rvalue is an expression within your code, with some type.
data RValue

{-
   A gcc_jit_lvalue is a storage location within your code (e.g. a
   variable, a parameter, etc).  It is also a gcc_jit_rvalue; use
   gcc_jit_lvalue_as_rvalue to cast.
-}
data LValue

{-
    gcc_jit_param is a function parameter, used when creating a
   gcc_jit_function.  It is also a gcc_jit_lvalue (and thus also an
   rvalue); use gcc_jit_param_as_lvalue to convert.
-}
data Param

{-
   A gcc_jit_case is for use when building multiway branches via
   gcc_jit_block_end_with_switch and represents a range of integer
   values (or an individual integer value) together with an associated
   destination block.
-}
#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
data Case
#endif

{-
   A gcc_jit_extended_asm represents an assembly language statement,
   analogous to an extended "asm" statement in GCC's C front-end: a series
   of low-level instructions inside a function that convert inputs to
   outputs.
-}
#ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS
data ExtendedAsm
#endif

{-
    A gcc_jit_timer represents a timer context to measure compile processes 
-}
#ifdef LIBGCCJIT_HAVE_TIMING_API
data Timer
#endif

