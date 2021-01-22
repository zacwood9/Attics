
module Test.TestScrape where

import           Test.Hspec
import           IHP.Test.Mocking
import           IHP.FrameworkConfig (ConfigBuilder(..))
import           IHP.Prelude
import           IHP.QueryBuilder (fetch, query)

import           Web.Types
import           Web.Routes
import           Generated.Types
import           Main ()

import Application.Helper.Archive
import Application.Helper.Scrape
import Text.RawString.QQ
import Data.Aeson (decode)

import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "ArchiveItem parser" $ do
    it "should parse all examples" $ do
      mapM_ attemptParse testArchiveItems

  where
    attemptParse text = do
      let result = decode (cs text) :: Maybe ArchiveItem
      result `shouldNotBe` Nothing

testArchiveItems :: [ByteString]
testArchiveItems = [
 [r|{
      "lineage": "Amadeus Pro > xACT > Flac",
      "date": "2010-08-27T00:00:00Z",
      "coverage": "Morrison, CO",
      "identifier": "ymsb2010-08-27.4mic.flac24",
      "venue": "Red Rocks Amphitheatre",
      "transferer": "Todd R",
      "downloads": 117,
      "source": [
        "4 mic mix: Milab vm44-link > Naiant LB > Tascam 680 + Gefell m270 > SD 744 > Tascam 680 (24\/44)",
        "4 mic mix: Gefell + Milab"
      ]
    }|],

    [r|{
      "lineage": "wav>pc>Soundforge(fades, edits)>waveditor",
      "date": "2010-08-21T00:00:00Z",
      "coverage": "Tempe, AZ",
      "identifier": "ymsb2010-08-21.jbillerb24",
      "venue": "The Marquee Theatre",
      "transferer": "Brian Hormann (Grout)",
      "downloads": 109,
      "source": "Schoeps CMC6\/Mk4 > Sound Devices 744T (24 bit@48KHZ)"
    }|]
 ]