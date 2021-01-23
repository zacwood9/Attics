
module Test.Application.TestScrape where

import           IHP.FrameworkConfig        (ConfigBuilder (..))
import           IHP.Prelude
import           IHP.QueryBuilder           (fetch, query)
import           IHP.Test.Mocking
import           Test.Hspec

import           Generated.Types
import           Main                       ()
import           Web.Routes
import           Web.Types

import           Application.Helper.Archive
import           Application.Helper.Scrape
import           Data.Aeson
import           Text.RawString.QQ

import           Control.Exception          (evaluate)
import           Data.Either                (isRight)

main = hspec spec

spec :: Spec
spec = do
  describe "ArchiveItem parser" $ do
    it "should parse all examples" $ do
      mapM_ attemptParse testArchiveItems

  where
    attemptParse text = do
      let result = eitherDecode (cs text) :: Either String ArchiveItem
      result `shouldSatisfy` isRight

testArchiveItems :: [ByteString]
testArchiveItems = [
  -- source :: Array
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

   -- source :: Text
    [r|{
      "lineage": "wav>pc>Soundforge(fades, edits)>waveditor",
      "date": "2010-08-21T00:00:00Z",
      "coverage": "Tempe, AZ",
      "identifier": "ymsb2010-08-21.jbillerb24",
      "venue": "The Marquee Theatre",
      "transferer": "Brian Hormann (Grout)",
      "downloads": 109,
      "source": "Schoeps CMC6\/Mk4 > Sound Devices 744T (24 bit@48KHZ)"
    }|],

   -- source :: Nothing
    [r|{
      "lineage": "wav>pc>Soundforge(fades, edits)>waveditor",
      "date": "2010-08-21T00:00:00Z",
      "coverage": "Tempe, AZ",
      "identifier": "ymsb2010-08-21.jbillerb24",
      "venue": "The Marquee Theatre",
      "transferer": "Brian Hormann (Grout)",
      "downloads": 109
    }|]
 ]
