module Test.Test where

import Generated.Types
import IHP.FrameworkConfig (ConfigBuilder (..))
import IHP.Prelude
import IHP.Test.Mocking
import Main ()
import Test.Hspec
import Text.RawString.QQ
import Web.Routes
import Web.Types

import qualified Test.TestScrape
import qualified Test.TestFixSongs

main = hspec do
    Test.TestScrape.spec
    Test.TestFixSongs.spec
