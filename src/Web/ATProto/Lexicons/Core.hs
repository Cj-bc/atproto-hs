{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.ATProto.Lexicons.Core where
import Data.Text (Text)
import Data.Aeson (Value(..), FromJSON(parseJSON), (.:), (.:?), Object, withObject, withText)
import Data.Aeson.Types (Parser)
import Control.Monad (when)


-- | NameSpace ID.
--
-- Currently, it's just wrapping Text. In the future,
-- I want to write more strict implementation.
--
-- https://atproto.com/specs/nsid
newtype NSID = NSID { unNSID :: Text } deriving (Show, Eq)

-- | Possible Body encodings.
--
-- e.g. 'application/ld+json'
type Encoding = Text

-- | XRPC method parameter type
--
-- Default value is also included to make implementation simpler.
data XrpcParameterType = XrpcParameterString (Maybe Text)
                       | XrpcParameterNumber (forall a. Num a => Maybe a)
                       | XrpcParameterNumbeInteger (Maybe Int)
                       | XrpcParameterBoolean (Maybe Bool)
    deriving (Show, Eq)

-- | Represents one parameter of Xrpc method.
--
-- Default value is stored in '_xrpcParameterType'
data XrpcParameter = XrpcParameter { _xrpcParameterType :: XrpcParameterType
                                   , _xrpcParameterDescription :: Text
                                   , _xrpcParameterRequired :: Maybe Bool
                                   , _xrpcParameterMinLength :: forall a. Num a => Maybe a
                                   , _xrpcParameterMaxLength :: forall a. Num a => Maybe a
                                   , _xrpcParameterMinimum :: forall a. Num a => Maybe a
                                   , _xrpcParameterMaximum :: forall a. Num a => Maybe a
                                   }

-- | XRPC body
--
-- This is used to represents input/output of each method
data XrpcBody = XrpcBody { _xrpcBodyEncoding :: [Encoding]
                         , _xrpcBodySchema :: Value
                         , _xrpcBodyDescription :: Maybe Text
                         }

-- | XRPC method Error
data XrpcError = XrpcError { _xrpcErrorName :: Text
                           , _xrpcErrorDescription :: Maybe Text
                           }

-- | Whole Lexicon Document
data LexiconDoc =
  RecordLexiconDocV1 { _id :: NSID
                     , _revision :: Num a => Maybe a
                     , _description  :: Maybe Text
                     , _defs :: Value
                     , _key :: Maybe Text
                     , _record :: Value
                     }
  | XrpcProcedureLexiconDocV1 { _id :: NSID
                              , _revision :: Num a => Maybe a
                              , _description  :: Maybe Text
                              , _defs :: Value
                              , _input :: Maybe XrpcBody
                              , _output :: Maybe XrpcBody
                              , _error :: Maybe XrpcError
                              }
  | XrpcQueryLexiconDocV1 { _id :: NSID
                          , _revision :: Num a => Maybe a
                          , _description  :: Maybe Text
                          , _defs :: Value
                          , _parameters :: Maybe (Map.Map Text XrpcParameter)
                          , _input :: Maybe XrpcBody
                          , _output :: Maybe XrpcBody
                          , _errors :: Maybe [XrpcError]
                          }

instance FromJSON LexiconDoc where
  parseJSON = withObject "lexicon" $ \v -> do
    (lexiconVersion :: Int) <- v .: "lexicon"
    when (lexiconVersion /= 1) $ fail  "only lexicon version 1 is supported for now"
    
    lexiconType <- v .: "type"
    case lexiconType of
      (String "query") -> parseQueryMethod v
      (String "procedure") -> parseProcedureMethod v
      (String "record") -> parseRecord v
  
parseQueryMethod :: Object -> Parser LexiconDoc
parseQueryMethod _ = fail "Not implemented yet"

parseProcedureMethod :: Object -> Parser LexiconDoc
parseProcedureMethod _ = fail "Not implemented yet"

parseRecord :: Object -> Parser LexiconDoc
parseRecord v = RecordLexiconDocV1
                <$> v .: "id"
                <*> v .:? "revision"
                <*> v .:? "description"
                <*> v .:? "defs"
                <*> v .:? "key"
                <*> v .: "record"
