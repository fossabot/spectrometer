module Gradle.GradleTest
  ( spec_buildGraph
  ) where

import Prologue hiding (empty)

import qualified Data.Map.Strict as M

import DepTypes
import Graphing (Graphing, empty)
import Strategy.Gradle (JsonDep(..), buildGraph)

import GraphUtil
import Test.Tasty.Hspec

projectOne :: Dependency
projectOne = Dependency
  { dependencyType = SubprojectType
  , dependencyName = ":projectOne"
  , dependencyVersion = Nothing
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

projectTwo :: Dependency
projectTwo = Dependency
  { dependencyType = SubprojectType
  , dependencyName = ":projectTwo"
  , dependencyVersion = Nothing
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

projectThree :: Dependency
projectThree = Dependency
  { dependencyType = SubprojectType
  , dependencyName = ":projectThree"
  , dependencyVersion = Nothing
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

packageOne :: Dependency
packageOne = Dependency
  { dependencyType = MavenType
  , dependencyName = "mygroup:packageOne"
  , dependencyVersion = Just (CEq "1.0.0")
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

packageTwo :: Dependency
packageTwo = Dependency
  { dependencyType = MavenType
  , dependencyName = "mygroup:packageTwo"
  , dependencyVersion = Just (CEq "2.0.0")
  , dependencyLocations = []
  , dependencyTags = M.empty
  }

gradleOutput :: Map Text [JsonDep]
gradleOutput = M.fromList
  [ (":projectOne", [ProjectDep ":projectTwo"])
  , (":projectTwo", [ProjectDep ":projectThree", PackageDep "mygroup:packageOne" "1.0.0" []])
  , (":projectThree", [PackageDep "mygroup:packageTwo" "2.0.0" []])
  ]

spec_buildGraph :: Spec
spec_buildGraph = do
  describe "buildGraph" $ do
    it "should produce an empty graph for empty input" $ do
      let graph = buildGraph M.empty
      graph `shouldBe` (empty :: Graphing Dependency)

    it "should produce expected output" $ do
      let graph = buildGraph gradleOutput
      expectDeps [projectOne, projectTwo, projectThree, packageOne, packageTwo] graph
      expectDirect [projectOne, projectTwo, projectThree] graph
      expectEdges [ (projectOne, projectTwo)
                  , (projectTwo, projectThree)
                  , (projectTwo, packageOne)
                  , (projectThree, packageTwo)
                  ] graph
