-- |
-- Module: Servant.Client.JsonRpc
--
-- This client implementation runs over HTTP and the semantics of HTTP remove
-- the need for the message id.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Client.JsonRpc
    ( JsonRpc
    , Request (..)
    , JsonRpcResponse (..)
    , JsonRpcErr (..)
    ) where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Proxy
import           Data.Word           (Word64)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.Client.Core (HasClient (..), RunClient)
import           Servant.JsonRpc     (JsonRpc, JsonRpcEndpoint (..),
                                      JsonRpcErr (..), JsonRpcResponse (..),
                                      Request (..))

instance (RunClient m, KnownSymbol method, ToJSON p, FromJSON e, FromJSON r)
    => HasClient m (JsonRpc method p e r) where

    type Client m (JsonRpc method p e r)
        = p -> m (JsonRpcResponse e r)

    clientWithRoute _ _ req p =
        client req jsonRpcRequest
        where
        client = clientWithRoute (Proxy @m) (Proxy @(JsonRpcEndpoint p e r))
        jsonRpcRequest = Request (symbolVal $ Proxy @method) p 0

    hoistClientMonad _ _ f x p = f $ x p
