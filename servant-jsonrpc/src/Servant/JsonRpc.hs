-- |
-- Module: Servant.JsonRpc

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.JsonRpc
    ( JsonRpc
    , JsonRpcEndpoint
    , Request (..)
    , JsonRpcResponse (..)
    , JsonRpcErr (..)
    ) where


import           Control.Applicative (liftA3)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (Null),
                                      object, withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Proxy
import           Data.Word           (Word64)
import           GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)
import           Servant.API         ((:>), JSON, Post, ReqBody)
import           Servant.Server      (HasServer (..))


data Request p
    = Request { method :: String, params :: p, id :: Word64 }
    deriving (Eq, Show)


instance ToJSON p => ToJSON (Request p) where
    toJSON (Request m p ix) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "method" .= m
               , "params" .= p
               , "id" .= ix ]


instance FromJSON p => FromJSON (Request p) where
    parseJSON = withObject "JsonRpc Request" $ \obj -> do
        ix      <- obj .: "id"
        method  <- obj .: "method"
        p       <- obj .: "params"
        version <- obj .: "jsonrpc"

        versionGuard version . pure $ Request method p ix


versionGuard :: String -> Parser a -> Parser a
versionGuard v x
    | v == "2.0" = x
    | otherwise  = fail "unknown version"


data JsonRpcResponse e r
    = Result Word64 r
    | Errors (JsonRpcErr e)
    deriving (Eq, Show)


data JsonRpcErr e = JsonRpcErr Int String (Maybe e)
    deriving (Eq, Show)


instance (FromJSON e, FromJSON r) => FromJSON (JsonRpcResponse e r) where
    parseJSON = withObject "Response" $ \obj -> do
        ix      <- obj .: "id"
        version <- obj .: "jsonrpc"
        result  <- obj .:? "result"
        err     <- obj .:? "error"
        versionGuard version $ pack ix result err

        where

        pack (Just ix) (Just r) Nothing = pure $ Result ix r
        pack Nothing Nothing (Just e)   = Errors <$> parseErr e
        pack _ _ _                      = fail "invalid response"

        parseErr = withObject "Error" $
            liftA3 JsonRpcErr <$> (.: "code") <*> (.: "message") <*> (.:? "data")


instance (ToJSON e, ToJSON r) => ToJSON (JsonRpcResponse e r) where
    toJSON (Result ix r) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "result" .= r
               , "id" .= ix
               ]

    toJSON (Errors (JsonRpcErr c msg err)) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "id" .= Null
               , "error" .=
                    object [ "code" .= c
                           , "message" .= msg
                           , "data" .= err
                           ]
               ]


-- | This is the type used to specify JSON-RPC endpoints
data JsonRpc (method :: Symbol) p e r


type JsonRpcEndpoint p e r
    = ReqBody '[JSON] (Request p) :> Post '[JSON] (JsonRpcResponse e r)
