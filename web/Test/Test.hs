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
import qualified Test.TestNightlyScrape
import qualified Test.TestInitialScrape

main = hspec do
    Test.TestScrape.spec
    Test.TestFixSongs.spec
    Test.TestNightlyScrape.spec
    Test.TestNightlyScrape.integration
    Test.TestInitialScrape.spec
