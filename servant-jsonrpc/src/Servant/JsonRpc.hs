{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module: Servant.JsonRpc
--
-- Work with JSON-RPC protocol messages at both type and value level.
--
-- > type Mul = JsonRpc "mul" (Int, Int) String Int
-- >
-- > req :: Request (Int, Int)
-- > req = Request "mul" (3, 5) (Just 0)
-- >
-- > rsp :: JsonRpcResponse String Int
-- > rsp = Result 0 15
module Servant.JsonRpc
    (
    -- * API specification types
      RawJsonRpc
    , JsonRpc
    , JsonRpcNotification

    -- * JSON-RPC messages
    , Request (..)
    , JsonRpcResponse (..)
    , JsonRpcErr (..)

    -- ** Standard error codes
    , parseErrorCode
    , invalidRequestCode
    , methodNotFoundCode
    , invalidParamsCode
    , internalErrorCode

    -- * Type rewriting
    , JsonRpcEndpoint
    ) where


import           Control.Applicative (liftA3)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (Null),
                                      object, withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Maybe          (isNothing)
import           Data.Word           (Word64)
import           GHC.TypeLits        (Symbol)
import           Servant.API         ((:>), JSON, NoContent, Post, ReqBody)


-- | Client messages
data Request p
    = Request
    { method    :: String
    , params    :: p

    -- | should be omitted only if the message is a notification, with no response content
    , requestId :: Maybe Word64
    } deriving (Eq, Show)


instance ToJSON p => ToJSON (Request p) where
    toJSON (Request m p ix) =
        object
            . maybe id (onValue "id") ix
            $ [ "jsonrpc" .= ("2.0" :: String)
              , "method" .= m
              , "params" .= p
              ]

        where
        onValue n v = ((n .= v) :)


instance FromJSON p => FromJSON (Request p) where
    parseJSON = withObject "JsonRpc Request" $ \obj -> do
        ix <- obj .:? "id"
        m  <- obj .:  "method"
        p  <- obj .:  "params"
        v  <- obj .:  "jsonrpc"

        versionGuard v . pure $ Request m p ix


versionGuard :: Maybe String -> Parser a -> Parser a
versionGuard v x
    | v == Just "2.0" = x
    | isNothing v     = x
    | otherwise       = fail "unknown version"


-- | Server messages.  An 'Ack' is a message which refers to a 'Request' but
-- both its "errors" and "result" keys are null
data JsonRpcResponse e r
    = Result Word64 r
    | Ack Word64
    | Errors (Maybe Word64) (JsonRpcErr e)
    deriving (Eq, Show)


data JsonRpcErr e = JsonRpcErr
    { errorCode    :: Int
    , errorMessage :: String
    , errorData    :: Maybe e
    } deriving (Eq, Show)


parseErrorCode :: Int
parseErrorCode = -32700


invalidRequestCode :: Int
invalidRequestCode = -32600


methodNotFoundCode :: Int
methodNotFoundCode = -32601


invalidParamsCode :: Int
invalidParamsCode = -32602


internalErrorCode :: Int
internalErrorCode = -32603


instance (FromJSON e, FromJSON r) => FromJSON (JsonRpcResponse e r) where
    parseJSON = withObject "Response" $ \obj -> do
        ix      <- obj .:  "id"
        version <- obj .:? "jsonrpc"
        result  <- obj .:? "result"
        err     <- obj .:? "error"
        versionGuard version $ pack ix result err

        where

        pack (Just ix) (Just r) Nothing = pure $ Result ix r
        pack ix Nothing (Just e)        = Errors ix <$> parseErr e
        pack (Just ix) Nothing Nothing  = pure $ Ack ix
        pack _ _ _                      = fail "invalid response"

        parseErr = withObject "Error" $
            liftA3 JsonRpcErr <$> (.: "code") <*> (.: "message") <*> (.:? "data")


instance (ToJSON e, ToJSON r) => ToJSON (JsonRpcResponse e r) where
    toJSON (Result ix r) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "result"  .= r
               , "id"      .= ix
               ]

    toJSON (Ack ix) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "id"      .= ix
               , "result"  .= Null
               , "error"   .= Null
               ]

    toJSON (Errors ix (JsonRpcErr c msg err)) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "id"      .= ix
               , "error"   .= detail
               ]

         where
         detail = object [ "code"    .= c
                         , "message" .= msg
                         , "data"    .= err
                         ]


-- | A JSON RPC server handles any number of methods.  Represent this at the type level using this type.
data RawJsonRpc api


-- | JSON-RPC endpoints which respond with a result
data JsonRpc (method :: Symbol) p e r


-- | JSON-RPC endpoints which do not respond
data JsonRpcNotification (method :: Symbol) p


type family JsonRpcEndpoint a where
    JsonRpcEndpoint (JsonRpc m p e r)
        = ReqBody '[JSON] (Request p) :> Post '[JSON] (JsonRpcResponse e r)

    JsonRpcEndpoint (JsonRpcNotification m p)
        = ReqBody '[JSON] (Request p) :> Post '[JSON] NoContent
