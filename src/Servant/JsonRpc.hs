-- |
-- Module: Servant.JsonRpc

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.JsonRpc where


import           Control.Applicative (liftA3)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (Null),
                                      object, withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Proxy
import           Data.Word           (Word64)
import           GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)
import           Servant.API         ((:>), JSON, Post, ReqBody)
import           Servant.Client.Core (HasClient (..), RunClient)
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
        ix <- obj .: "id"
        method <- obj .: "method"
        p <- obj .: "params"
        version <- obj .: "jsonrpc"

        versionGuard version . pure $ Request method p ix


versionGuard :: String -> Parser a -> Parser a
versionGuard v x
    | v == "2.0" = x
    | otherwise  = fail "unknown version"


data Response e r
    = Result Word64 r
    | Errors (JsonRpcErr e)
    deriving (Eq, Show)


data JsonRpcErr e = JsonRpcErr Int String (Maybe e)
    deriving (Eq, Show)

instance (FromJSON e, FromJSON r) => FromJSON (Response e r) where
    parseJSON = withObject "Response" $ \obj -> do
        ix <- obj .: "id"
        version <- obj .: "jsonrpc"
        result <- obj .:? "result"
        err <- obj .:? "error"
        versionGuard version $ pack ix result err

        where

        pack (Just ix) (Just r) Nothing = pure $ Result ix r
        pack Nothing Nothing (Just e)   = Errors <$> parseErr e
        pack _ _ _                      = fail "invalid response"

        parseErr = withObject "Error" $
            liftA3 JsonRpcErr <$> (.: "code") <*> (.: "message") <*> (.:? "data")

instance (ToJSON e, ToJSON r) => ToJSON (Response e r) where
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
    = ReqBody '[JSON] (Request p) :> Post '[JSON] (Response e r)

instance (RunClient m, KnownSymbol method, ToJSON p, FromJSON e, FromJSON r)
    => HasClient m (JsonRpc method p e r) where

    type Client m (JsonRpc method p e r)
        = Word64 -> p -> m (Either (JsonRpcErr e) r)

    clientWithRoute _ _ req ix p =
        let client = clientWithRoute (Proxy @m) (Proxy @(JsonRpcEndpoint p e r))
            repack (Result _ r) = Right r
            repack (Errors x)   = Left x
        in repack <$> client req (Request (symbolVal $ Proxy @method) p ix)

    hoistClientMonad _ _ f x ix p = f $ x ix p

instance (KnownSymbol method, FromJSON p, ToJSON e, ToJSON r)
    => HasServer (JsonRpc method p e r) context where

    type ServerT (JsonRpc method p e r) m = p -> m (Either (JsonRpcErr e) r)

    route _ cx = route (Proxy @(JsonRpcEndpoint p e r)) cx . fmap f
        where
        f x (Request _ p ix) = g ix <$> x p
        g ix (Right r) = Result ix r
        g _ (Left e)   = Errors e

    hoistServerWithContext _ _ f x p = f $ x p
