module Function where

import Language

import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)

mkGlobalFunctions :: Variables
mkGlobalFunctions =
    let globals = foldr insert Map.empty gfunctions
    in  ([],globals)
  where
    insert :: (String,Value) -> Map.Map String Value-> Map.Map String Value
    insert (k,v) = Map.insert k v
    gfunctions :: [(String,Value)]
    gfunctions = 
        [ ("clock",VCallable 0 (
            NativeFunction (\_ -> do
                timestamp <- getPOSIXTime
                pure $ Right (VNumber $ realToFrac timestamp)
                )))
        ]