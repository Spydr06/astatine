module GccJit.Utils (
    AsObject(..),
    AsType(..),
    AsRValue(..),
    Releasable(..),
) where

#include <libgccjit.h>

import Foreign.Ptr

import GccJit.Foreign
import GccJit.Types

class AsObject a where
    asObject :: Ptr a -> IO (Ptr Object)

instance AsObject Object where
    asObject = return

instance AsObject Type where
    asObject = typeAsObject

instance AsObject RValue where
    asObject = rValueAsObject

instance AsObject LValue where
    asObject = lValueAsObject

instance AsObject Function where
    asObject = functionAsObject

instance AsObject Block where
    asObject = blockAsObject

instance AsObject Field where
    asObject = fieldAsObject

instance AsObject Param where
    asObject = paramAsObject

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
instance AsObject Case where
    asObject = caseAsObject
#endif

#ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS
instance AsObject ExtendedAsm where
    asObject = extendedAsmAsObject
#endif

class AsType a where
    asType :: Ptr a -> IO (Ptr Type)

instance AsType Type where
    asType = return

instance AsType Struct where
    asType = structAsType

class AsRValue a where
    asRValue :: Ptr a -> IO (Ptr RValue)

instance AsRValue RValue where
    asRValue = return

instance AsRValue Param where
    asRValue = paramAsRValue

instance AsRValue LValue where
    asRValue = lValueAsRValue

class Releasable a where
    release :: Ptr a -> IO ()

instance Releasable Context where
    release = contextRelease

instance Releasable Result where
    release = resultRelease

#ifdef LIBGCCJIT_HAVE_TIMING_API
instance Releasable Timer where
    release = timerRelease
#endif
