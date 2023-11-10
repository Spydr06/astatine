module LibAstatine.Version (
    version,
    versionString
) where

import qualified Paths_astatine
import Data.Version

version :: Version
version = Paths_astatine.version 

versionString :: String
versionString = show version 

