module Strategy.Go.Gomod
  ( discover
  , strategy
  , analyze
  , configure
  , buildGraph

  , Gomod(..)
  , Statement(..)
  , Require(..)
  , gomodParser
  )
  where

import Prologue hiding ((<?>))

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import DepTypes
import Diagnostics
import Discovery.Walk
import qualified Effect.Error as E
import Effect.Exec
import Effect.LabeledGrapher
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Go.Transitive
import Strategy.Go.Types
import Types

discover :: Discover
discover = Discover
  { discoverName = "gomod"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "go.mod") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "golang-gomod"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

data Statement =
    RequireStatement Text Text -- ^ package, version
  | ReplaceStatement Text Text Text -- ^ old, new, newVersion
  | ExcludeStatement Text Text -- ^ package, version
  | GoVersionStatement Text
    deriving (Eq, Ord, Show, Generic)

type PackageName = Text

data Gomod = Gomod
  { modName     :: PackageName
  , modRequires :: [Require]
  , modReplaces :: Map PackageName Require
  , modExcludes :: [Require]
  } deriving (Eq, Ord, Show, Generic)

data Require = Require
  { reqPackage :: PackageName
  , reqVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

type Parser = Parsec Void Text

gomodParser :: Parser Gomod
gomodParser = do
  _ <- sc
  _ <- lexeme (chunk "module")
  name <- packageName
  statements <- many statement
  eof

  let statements' = concat statements

  pure (toGomod name statements')
  where
  statement = (singleton <$> goVersionStatement) -- singleton wraps the Parser Statement into a Parser [Statement]
          <|> requireStatements
          <|> replaceStatements
          <|> excludeStatements

  -- top-level go version statement
  -- e.g., go 1.12
  goVersionStatement :: Parser Statement
  goVersionStatement = GoVersionStatement <$ lexeme (chunk "go") <*> semver

  -- top-level require statements
  -- e.g.:
  --   require golang.org/x/text v1.0.0
  --   require (
  --       golang.org/x/text v1.0.0
  --       golang.org/x/sync v2.0.0
  --   )
  requireStatements :: Parser [Statement]
  requireStatements = block "require" singleRequire

  -- parse the body of a single require (without the leading "require" lexeme)
  singleRequire = RequireStatement <$> packageName <*> semver

  -- top-level replace statements
  -- e.g.:
  --   replace golang.org/x/text => golang.org/x/text v3.0.0
  --   replace (
  --       golang.org/x/sync => golang.org/x/sync v15.0.0
  --       golang.org/x/text => golang.org/x/text v3.0.0
  --   )
  replaceStatements :: Parser [Statement]
  replaceStatements = block "replace" singleReplace

  -- parse the body of a single replace (without the leading "replace" lexeme)
  singleReplace :: Parser Statement
  singleReplace = ReplaceStatement <$> packageName <* optional semver <* lexeme (chunk "=>") <*> packageName <*> semver

  -- top-level exclude statements
  -- e.g.:
  --   exclude golang.org/x/text v3.0.0
  --   exclude (
  --       golang.org/x/text v3.0.0
  --       golang.org/x/sync v15.0.0
  --   )
  excludeStatements :: Parser [Statement]
  excludeStatements = block "exclude" singleExclude

  -- parse the body of a single exclude (without the leading "exclude" lexeme)
  singleExclude :: Parser Statement
  singleExclude = ExcludeStatement <$> packageName <*> semver

  -- helper combinator to parse things like:
  --
  --   prefix <singleparse>
  --
  -- or
  --
  --   prefix (
  --       <singleparse>
  --       <singleparse>
  --       <singleparse>
  --   )
  block prefix parseSingle = do
    _ <- lexeme (chunk prefix)
    parens (many parseSingle) <|> (singleton <$> parseSingle)

  -- package name, e.g., golang.org/x/text
  packageName :: Parser Text
  packageName = T.pack <$> lexeme (some (alphaNumChar <|> char '.' <|> char '/' <|> char '-' <|> char '_'))

  -- semver, e.g.:
  --   v0.0.0-20190101000000-abcdefabcdef
  --   v1.2.3
  semver :: Parser Text
  semver = T.pack <$> lexeme (some (alphaNumChar <|> oneOf ['.', '-', '+']))

  -- singleton list. semantically more meaningful than 'pure'
  singleton :: a -> [a]
  singleton = pure

  -- lexer combinators
  parens = between (symbol "(") (symbol ")")
  symbol = L.symbol sc
  lexeme = L.lexeme sc

  -- space consumer (for use with Text.Megaparsec.Char.Lexer combinators)
  sc :: Parser ()
  sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

toGomod :: Text -> [Statement] -> Gomod
toGomod name = foldr apply (Gomod name [] M.empty [])
  where
  apply (RequireStatement package version) gomod = gomod { modRequires = Require package version : modRequires gomod }
  apply (ReplaceStatement old new newVersion) gomod = gomod { modReplaces = M.insert old (Require new newVersion) (modReplaces gomod) }
  apply (ExcludeStatement package version) gomod = gomod { modExcludes = Require package version : modExcludes gomod }
  apply _ gomod = gomod

-- lookup modRequires and replace them with modReplaces as appropriate, producing the resolved list of requires
resolve :: Gomod -> [Require]
resolve gomod = map resolveReplace (modRequires gomod)
  where
  resolveReplace require = fromMaybe require (M.lookup (reqPackage require) (modReplaces gomod))

analyze :: Members '[Error ReadFSErr, Error ExecErr, ReadFS, Exec] r => BasicFileOpts -> Sem r (Graphing Dependency)
analyze BasicFileOpts{..} = graphingGolang $ do
  gomod <- readContentsParser gomodParser targetFile

  buildGraph gomod

  -- TODO: logging/etc
  _ <- E.try @ExecErr (fillInTransitive (parent targetFile))
  pure ()

buildGraph :: Member (LabeledGrapher GolangPackage) r => Gomod -> Sem r ()
buildGraph = traverse_ go . resolve
  where

  go :: Member (LabeledGrapher GolangPackage) r => Require -> Sem r ()
  go Require{..} = do
    let pkg = mkGolangPackage reqPackage

    direct pkg
    label pkg (mkGolangVersion reqVersion)

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
