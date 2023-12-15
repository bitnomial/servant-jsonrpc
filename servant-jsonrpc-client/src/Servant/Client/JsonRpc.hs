{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           Servant.API         (NoContent, JSON)
import           Servant.Client.Core (HasClient (..), RunClient)

import           Servant.Client.JsonRpc.ContentType
import           Servant.JsonRpc

-- | The 'RawJsonRpc' construct is completely transparent to clients
instance (RunClient m, HasClient m api) => HasClient m (RawJsonRpc api) where
    type Client m (RawJsonRpc api) = Client m api
    clientWithRoute pxm _  = clientWithRoute pxm (Proxy @api)
    hoistClientMonad pxm _ = hoistClientMonad pxm (Proxy @api)

-- | Forwarding instance, picks JSONRPC as the defau;t
instance (RunClient m, KnownSymbol method, ToJSON p, FromJSON e, FromJSON r)
    => HasClient m (JsonRpc method p e r) where

    type Client m (JsonRpc method p e r)
        = Client m (UsingContentType JSONRPC (JsonRpc method p e r))

    clientWithRoute pm _ =
        clientWithRoute
          pm
          (Proxy @(UsingContentType JSON (JsonRpc method p e r)))

    hoistClientMonad pm _ hoist =
        hoistClientMonad
          pm
          (Proxy @(UsingContentType JSON (JsonRpc method p e r)))
          hoist

instance ( RunClient m
         , KnownSymbol method
         , ToJSON p
         , FromJSON e
         , FromJSON r
         , IsSupportedContentType ctyp
         )
    => HasClient m (UsingContentType ctyp (JsonRpc method p e r)) where

    type Client m (UsingContentType ctyp (JsonRpc method p e r))
        = p -> m (JsonRpcResponse e r)

    clientWithRoute _ _ req p =
        client req jsonRpcRequest

        where
        client = case isSupportedContentType (Proxy @ctyp) of
                   SupportedContentTypeJSON ->
                     clientWithRoute (Proxy @m) endpoint
                   SupportedContentTypeJSONRPC ->
                     clientWithRoute (Proxy @m) endpoint
        jsonRpcRequest = Request (symbolVal $ Proxy @method) p (Just 0)

        endpoint = Proxy @(JsonRpcEndpoint ctyp (JsonRpc method p e r))

    hoistClientMonad _ _ f x p = f $ x p

-- | Forwarding instance, picks JSONRPC as the defau;t
instance (RunClient m, KnownSymbol method, ToJSON p)
    => HasClient m (JsonRpcNotification method p) where

    type Client m (JsonRpcNotification method p)
        = Client m (UsingContentType JSONRPC (JsonRpcNotification method p))

    clientWithRoute pm _ =
        clientWithRoute
          pm
          (Proxy @(UsingContentType JSONRPC (JsonRpcNotification method p)))

    hoistClientMonad pm _ hoist =
        hoistClientMonad
          pm
          (Proxy @(UsingContentType JSONRPC (JsonRpcNotification method p)))
          hoist

instance ( RunClient m
         , KnownSymbol method
         , ToJSON p
         , IsSupportedContentType ctyp
         )
    => HasClient m (UsingContentType ctyp (JsonRpcNotification method p)) where

    type Client m (UsingContentType ctyp (JsonRpcNotification method p))
        = p -> m NoContent

    clientWithRoute _ _ req p =
        client req jsonRpcRequest
        where
        client = case isSupportedContentType (Proxy @ctyp) of
                   SupportedContentTypeJSON ->
                     clientWithRoute (Proxy @m) endpoint
                   SupportedContentTypeJSONRPC ->
                     clientWithRoute (Proxy @m) endpoint
        jsonRpcRequest = Request (symbolVal $ Proxy @method) p Nothing

        endpoint = Proxy @(JsonRpcEndpoint ctyp (JsonRpcNotification method p))

    hoistClientMonad _ _ f x p = f $ x p
