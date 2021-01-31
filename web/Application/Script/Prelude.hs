module Application.Script.Prelude
( module IHP.ControllerPrelude
, module Generated.Types
, module IHP.Prelude
, module IHP.ScriptSupport
, module Database.PostgreSQL.Simple
, module Database.PostgreSQL.Simple.SqlQQ
, mapIOLog_
)
where

import IHP.Prelude
import IHP.ControllerPrelude
import Generated.Types
import IHP.ScriptSupport
import qualified Data.List as List
import Database.PostgreSQL.Simple (Only(..), Query)
import Database.PostgreSQL.Simple.SqlQQ

mapIOLog_ :: (Int -> Int -> a -> Text) -> (a -> IO b) -> [a] -> IO ()
mapIOLog_ logF f as =
  let total = List.length as
   in mapIOLog' 1 total logF f as
  where
    mapIOLog' _ _ _ _ [] = pure ()
    mapIOLog' i total logF f (a : as) = do
      putStrLn $ logF i total a
      f a
      mapIOLog' (i + 1) total logF f as
