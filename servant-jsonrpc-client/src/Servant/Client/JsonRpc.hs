-- |
-- Module: Servant.Client.JsonRpc

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Client.JsonRpc
    ( JsonRpc
    , Request (..)
    , Response (..)
    , JsonRpcErr (..)
    ) where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Proxy
import           Data.Word           (Word64)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.Client.Core (HasClient (..), RunClient)
import           Servant.JsonRpc     (JsonRpc, JsonRpcEndpoint (..),
                                      JsonRpcErr (..), Request (..),
                                      Response (..))

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


