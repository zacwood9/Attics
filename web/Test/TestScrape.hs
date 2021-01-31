module Test.TestScrape where

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
  describe "ArchiveItem parser" $ do
    it "should parse all examples" $ do
      mapM_
        ( \text -> do
            let result = eitherDecode (cs text) :: Either String ArchiveItem
            result `shouldSatisfy` isRight
          )
        testArchiveItems

  describe "advancedSearch Parser" $ do
    it "should parse with review date" $ do
      let result = eitherDecode (cs reviewWithReviewDate) :: Either String AdvancedSearchResult
      result `shouldSatisfy` \case
        Left _ -> False
        Right (ReviewDateResponse (_ : _)) -> True
        Right _ -> False

    it "should parse with public date" $ do
      let result = eitherDecode (cs reviewWithPublicDate) :: Either String AdvancedSearchResult
      result `shouldSatisfy` \case
        Left _ -> False
        Right (PublicDateResponse (_ : _)) -> True
        Right _ -> False

    it "should parse without review date" $ do
      let result = eitherDecode (cs reviewWithoutDate) :: Either String AdvancedSearchResult
      result `shouldSatisfy` \case
        Left _ -> False
        Right (ReviewDateResponse []) -> True
        Right _ -> False

-- Test data -------

testArchiveItems :: [ByteString]
testArchiveItems =
  [ -- source :: Array
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

reviewWithReviewDate :: Text
reviewWithReviewDate =
  [r|{
  "responseHeader": {
    "status": 0,
    "QTime": 11,
    "params": {
      "query": "collection:\"GratefulDead\"",
      "qin": "collection:\"GratefulDead\"",
      "fields": "identifier,publicdate,reviewdate",
      "wt": "json",
      "sort": "reviewdate desc",
      "start": 0
    }
  },
  "response": {
    "numFound": 15097,
    "start": 0,
    "docs": [
      {
        "avg_rating": "4.50",
        "coverage": "Ventura, CA",
        "date": "1985-07-13T00:00:00Z",
        "downloads": 5741,
        "identifier": "gd1985-07-13.BeyerM160.connor.76913.sbeok.flac16",
        "lineage": "Nak DR-10>Midiman Flying Cow 24bit/48kHz A/D>RME Digi96/8> Steinberg's Wavelab 3.0 (bit resolution & resampling to 16/44.1>CDWav beta1.57 (tracking only)>flac16",
        "num_reviews": 3,
        "source": "FOB>(Jim Vita's)Beyer Dynamic M160's>Sony TCD5M >Sony TCD5M (Charlie Connor's CM - TDK MA90's)",
        "transferer": "Charlie Connor",
        "venue": "Ventura County Fairgrounds",
        "reviewdate": "1985-07-13T00:00:00Z"
      }
    ]
  }
}|]

reviewWithoutDate :: Text
reviewWithoutDate =
  [r|{
  "responseHeader": {
    "status": 0,
    "QTime": 11,
    "params": {
      "query": "collection:\"GratefulDead\"",
      "qin": "collection:\"GratefulDead\"",
      "fields": "identifier,publicdate,reviewdate",
      "wt": "json",
      "sort": "reviewdate desc",
      "start": 0
    }
  },
  "response": {
    "numFound": 15097,
    "start": 0,
    "docs": [
      {
        "avg_rating": "4.50",
        "coverage": "Ventura, CA",
        "date": "1985-07-13T00:00:00Z",
        "downloads": 5741,
        "identifier": "gd1985-07-13.BeyerM160.connor.76913.sbeok.flac16",
        "lineage": "Nak DR-10>Midiman Flying Cow 24bit/48kHz A/D>RME Digi96/8> Steinberg's Wavelab 3.0 (bit resolution & resampling to 16/44.1>CDWav beta1.57 (tracking only)>flac16",
        "num_reviews": 3,
        "source": "FOB>(Jim Vita's)Beyer Dynamic M160's>Sony TCD5M >Sony TCD5M (Charlie Connor's CM - TDK MA90's)",
        "transferer": "Charlie Connor",
        "venue": "Ventura County Fairgrounds"
      }
    ]
  }
}|]

reviewWithPublicDate :: Text
reviewWithPublicDate =
  [r|{
  "responseHeader": {
    "status": 0,
    "QTime": 11,
    "params": {
      "query": "collection:\"GratefulDead\"",
      "qin": "collection:\"GratefulDead\"",
      "fields": "identifier,publicdate,reviewdate",
      "wt": "json",
      "sort": "publicdate desc",
      "start": 0
    }
  },
  "response": {
    "numFound": 15097,
    "start": 0,
    "docs": [
      {
        "avg_rating": "4.50",
        "coverage": "Ventura, CA",
        "date": "1985-07-13T00:00:00Z",
        "downloads": 5741,
        "identifier": "gd1985-07-13.BeyerM160.connor.76913.sbeok.flac16",
        "lineage": "Nak DR-10>Midiman Flying Cow 24bit/48kHz A/D>RME Digi96/8> Steinberg's Wavelab 3.0 (bit resolution & resampling to 16/44.1>CDWav beta1.57 (tracking only)>flac16",
        "num_reviews": 3,
        "source": "FOB>(Jim Vita's)Beyer Dynamic M160's>Sony TCD5M >Sony TCD5M (Charlie Connor's CM - TDK MA90's)",
        "transferer": "Charlie Connor",
        "venue": "Ventura County Fairgrounds",
        "publicdate": "1985-07-13T00:00:00Z"
      }
    ]
  }
}|]
