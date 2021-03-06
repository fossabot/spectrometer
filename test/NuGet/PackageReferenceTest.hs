{-# language TemplateHaskell #-}

module NuGet.PackageReferenceTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import DepTypes
import GraphUtil
import Parse.XML
import Polysemy
import Polysemy.Input
import Strategy.NuGet.PackageReference
import Test.Tasty.Hspec

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = NuGetType
                           , dependencyName = "one"
                           , dependencyVersion = Just (CEq "1.0.0")
                           , dependencyLocations = []
                           , dependencyTags = M.empty
                           }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = NuGetType
                           , dependencyName = "two"
                           , dependencyVersion = Just (CEq "2.0.0")
                           , dependencyLocations = []
                           , dependencyTags = M.empty
                           }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = NuGetType
                             , dependencyName = "three"
                             , dependencyVersion = Just (CEq "3.0.0")
                             , dependencyLocations = []
                             , dependencyTags = M.empty
                             }

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = NuGetType
                            , dependencyName = "four"
                            , dependencyVersion = Nothing
                            , dependencyLocations = []
                            , dependencyTags = M.empty
                            }

packageReference :: PackageReference
packageReference = PackageReference itemGroupList

itemGroupList :: [ItemGroup]
itemGroupList = [ItemGroup [refOne, refTwo], ItemGroup [refThree, refFour]]

refOne :: Package
refOne = Package "one" $ Just "1.0.0"

refTwo :: Package
refTwo = Package "two" $ Just "2.0.0"

refThree :: Package
refThree = Package "three" $ Just "3.0.0"

refFour :: Package
refFour = Package "four" Nothing

spec_analyze :: Spec
spec_analyze = do
  refFile <- runIO (TIO.readFile "test/NuGet/testdata/test.csproj")

  describe "Package Reference parser" $ do
    it "reads a file and constructs an accurate list of item groups" $ do
      case parseXML refFile of
        Right project -> (groups project) `shouldContain` itemGroupList
        Left err -> expectationFailure (T.unpack ("could not parse package reference file" <> xmlErrorPretty err))

    it "constructs an accurate graph" $ do
          let graph = analyze & runInputConst @PackageReference packageReference & run
          expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
          expectEdges [] graph
