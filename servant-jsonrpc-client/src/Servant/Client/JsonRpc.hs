{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Servant.Client.JsonRpc
--
-- This module provides support for generating JSON-RPC clients in the Servant framework.
--
-- > type Mul = JsonRpc "mul" (Int, Int) String Int
-- > mul :: (Int, Int) -> ClientM (JsonRpcResponse String Int)
-- > mul = client $ Proxy @Mul
--
-- Note: This client implementation runs over HTTP and the semantics of HTTP
-- remove the need for the message id.
module Servant.Client.JsonRpc
    ( module Servant.JsonRpc
    ) where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Proxy          (Proxy (..))
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Servant.API         (NoContent)
import           Servant.Client.Core (HasClient (..), RunClient)

import           Servant.JsonRpc


instance (RunClient m, KnownSymbol method, ToJSON p, FromJSON e, FromJSON r)
    => HasClient m (JsonRpc method p e r) where

    type Client m (JsonRpc method p e r)
        = p -> m (JsonRpcResponse e r)

    clientWithRoute _ _ req p =
        client req jsonRpcRequest

        where
        client = clientWithRoute (Proxy @m) endpoint
        jsonRpcRequest = Request (symbolVal $ Proxy @method) p (Just 0)

        endpoint = Proxy @(JsonRpcEndpoint (JsonRpc method p e r))

    hoistClientMonad _ _ f x p = f $ x p


instance (RunClient m, KnownSymbol method, ToJSON p)
    => HasClient m (JsonRpcNotification method p) where

    type Client m (JsonRpcNotification method p)
        = p -> m NoContent

    clientWithRoute _ _ req p =
        client req jsonRpcRequest
        where
        client = clientWithRoute (Proxy @m) endpoint
        jsonRpcRequest = Request (symbolVal $ Proxy @method) p Nothing

        endpoint = Proxy @(JsonRpcEndpoint (JsonRpcNotification method p))

    hoistClientMonad _ _ f x p = f $ x p
