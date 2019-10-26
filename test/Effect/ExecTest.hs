{-# language QuasiQuotes #-}

module Effect.ExecTest
  ( spec_execHelpers
  ) where

import Prologue

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Text.Megaparsec

import           Diagnostics
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Gradle (JsonDep(..), buildGraph)

import GraphUtil
import Test.Tasty.Hspec

mockingExec :: (Path Rel Dir -> Command -> [String] -> (Either [CmdFailure] BL.ByteString)) -> InterpreterFor Exec r
mockingExec f = interpret $ \case
  Exec dir cmd args -> pure $ f dir cmd args

type Parser = Parsec Void Text

failingParser :: Parser ()
failingParser = fail "failingParser"

successfulParser :: Parser ()
successfulParser = void (chunk "{}")

instance FromJSON Failing where
  parseJSON _ = fail "Failing"

data Failing

data Successful = Successful
  deriving (Eq, Show)

instance FromJSON Successful where
  parseJSON _ = pure Successful

dummyPath :: Path Rel Dir
dummyPath = [reldir|dummypath|]

spec_execHelpers :: Spec
spec_execHelpers = do
  let dummyCmd = Command
        { cmdNames = ["bad"]
        , cmdBaseArgs = []
        , cmdAllowErr = Never
        }

  let runFail = run . runError @CLIErr . mockingExec (\_ _ _ -> Left [])
      runSuccess = run . runError @CLIErr . mockingExec (\_ _ _ -> Right "{}")

  describe "execThrow" $ do
    it "should fail when the command fails" $ do
      let result = runFail (execThrow dummyPath dummyCmd [])
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "did not fail"

  describe "execInputParser" $ do
    it "should fail when the parser fails" $ do
      let result = runSuccess (execInputParser failingParser dummyPath dummyCmd [] $ input @())
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "did not fail"

    it "should succeed when the parser succeeds" $ do
      let result = runSuccess (execInputParser successfulParser dummyPath dummyCmd [] $ input @())
      result `shouldBe` Right ()

  describe "execInputJson" $ do
    it "should fail when the parser fails" $ do
      let result = runSuccess (execInputJson @Failing dummyPath dummyCmd [] $ input @Failing)
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "did not fail"

    it "should succeed when the parser succeeds" $ do
      let result = runSuccess (execInputJson @Successful dummyPath dummyCmd [] $ input @Successful)
      result `shouldBe` Right Successful
