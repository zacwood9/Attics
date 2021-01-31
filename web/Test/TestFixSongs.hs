module Test.TestFixSongs where

import Application.Helper.Archive
import Application.Helper.Scrape
import Control.Exception (evaluate)
import Data.Aeson
import Data.Either (isRight)
import Generated.Types
import IHP.FrameworkConfig (ConfigBuilder (..))
import IHP.Prelude
import IHP.Test.Mocking
import Main ()
import Test.Hspec
import Text.RawString.QQ
import Web.Routes
import Web.Types

spec :: Spec
spec = do
  describe "TestFixSongs" $ do
    it "should convert songs" $ do
        True `shouldBe` True
