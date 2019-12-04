module Data.XML
  ( Parser(..)
  , FromXML(..)
  , (.:)
  , (.:..)
  , (.:?)
  , withObject
  , withText
  ) where

import Prologue hiding ((.:), (.:?), withObject, withText)

import           Control.Monad.Fail (MonadFail)
import qualified Data.Text as T
import           Text.Read (readMaybe)
import qualified Text.XML.Light as XML
import           Text.XML.Light.Lexer (XmlSource)

data Parser a

class FromXML a where
  parseXML :: XML.Element -> Parser a

instance FromXML Int where
  parseXML = withText "Int" $ \text ->
    maybe (fail "") pure (readMaybe text)

instance {-# OVERLAPPING #-} XMLValue [Char] where
  fromXML el _ = undefined

class XMLValue a where
  fromXML :: XML.Element -> String -> Parser a

instance FromXML XML.Element where
  parseXML = undefined

instance {-# OVERLAPPABLE #-} FromXML a => XMLValue [a] where
  fromXML el key = traverse parseXML (XML.filterChildrenName (\name -> XML.qName name == key) el)

instance {-# OVERLAPPABLE #-} FromXML a => XMLValue a where

(.:) :: XMLValue a => XML.Element -> Text -> Parser a
(.:) = undefined

(.:?) :: XMLValue a => XML.Element -> Text -> Parser (Maybe a)
(.:?) = undefined

(.:..) :: FromXML a => XML.Element -> Text -> Parser [a]
(.:..) = undefined

withObject :: String -> (XML.Element -> Parser a) -> XML.Element -> Parser a
withObject = undefined

withText :: String -> (String -> Parser a) -> XML.Element -> Parser a
withText = undefined

decode :: (XmlSource s, FromXML a) => s -> Either Err a
decode = undefined

data Err = ParseFailure Text

instance MonadFail Parser where

instance Functor Parser where
instance Applicative Parser where
instance Alternative Parser where
instance Monad Parser where
