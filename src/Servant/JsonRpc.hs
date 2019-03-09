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


import           Data.Aeson          (FromJSON (..), ToJSON (..), object,
                                      withObject, (.:), (.:?), (.=))
import           Data.Proxy
import           Data.Word           (Word64)
import           GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)
import           Servant.API         ((:>), JSON, Post, ReqBody)
import           Servant.Client.Core (HasClient (..), RunClient)


data Request p
    = Request { method :: String, params :: p, id :: Word64 }
    deriving Eq


instance ToJSON p => ToJSON (Request p) where
    toJSON (Request m p ix) =
        object [ "jsonrpc" .= ("2.0" :: String)
               , "method" .= m
               , "params" .= p
               , "id" .= ix ]


data Response e r
    = Result Word64 r
    | Errors Int String (Maybe e)

instance (FromJSON e, FromJSON r) => FromJSON (Response e r) where
    parseJSON = withObject "Response" $ \obj -> do
        ix <- obj .: "id"
        version :: String <- obj .: "jsonrpc"
        result <- obj .:? "result"
        err <- obj .:? "error"
        versionGuard version $ pack ix result err

        where

        versionGuard v x
            | v == "2.0" = x
            | otherwise  = fail "unknown version"

        pack (Just ix) (Just r) Nothing = pure $ Result ix r
        pack Nothing Nothing (Just e)   = parseErr e
        pack _ _ _                      = fail "invalid response"

        parseErr = withObject "Error" $ \err -> do
            code <- err .: "code"
            msg <- err .: "message"
            dat <- err .:? "data"
            return $ Errors code msg dat


-- | This is the type used to specify JSON-RPC endpoints
data JsonRpc (method :: Symbol) p e r

type JsonRpcEndpoint p e r
    = ReqBody '[JSON] (Request p) :> Post '[JSON] (Response e r)

instance (RunClient m, KnownSymbol method, ToJSON p, FromJSON e, FromJSON r)
    => HasClient m (JsonRpc method p e r) where

    type Client m (JsonRpc method p e r)
        = Word64 -> p -> m (Either String r)

    clientWithRoute _ _ req ix p =
        let client = clientWithRoute (Proxy @m) (Proxy @(JsonRpcEndpoint p e r))
            repack (Result _ r)     = Right r
            repack (Errors _ msg _) = Left msg
        in repack <$> client req (Request (symbolVal $ Proxy @method) p ix)

    hoistClientMonad _ _ f x ix p = f $ x ix p
