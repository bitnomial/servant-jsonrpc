{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module: Servant.Client.JsonRpc

This module provides support for generating JSON-RPC clients in the Servant framework.

> type Mul = JsonRpc "mul" (Int, Int) String Int
> mul :: (Int, Int) -> ClientM (JsonRpcResponse String Int)
> mul = client $ Proxy @Mul

Note: This client implementation runs over HTTP and the semantics of HTTP
remove the need for the message id.
-}
module Servant.Client.JsonRpc (
  module Servant.JsonRpc,
) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Servant.API (MimeRender, MimeUnrender, NoContent, (:<|>))
import Servant.Client.Core (HasClient (..), RunClient)

import Servant.JsonRpc

-- | The 'RawJsonRpc' construct is completely transparent to clients
instance
  (RunClient m, HasClient m (RawJsonRpc ctype apiL), HasClient m (RawJsonRpc ctype apiR)) =>
  HasClient m (RawJsonRpc ctype (apiL :<|> apiR))
  where
  type Client m (RawJsonRpc ctype (apiL :<|> apiR)) = Client m (RawJsonRpc ctype apiL :<|> RawJsonRpc ctype apiR)
  clientWithRoute pxm _ = clientWithRoute pxm (Proxy @(RawJsonRpc ctype apiL :<|> RawJsonRpc ctype apiR))
  hoistClientMonad pxm _ = hoistClientMonad pxm (Proxy @(RawJsonRpc ctype apiL :<|> RawJsonRpc ctype apiR))

instance
  ( RunClient m
  , KnownSymbol method
  , MimeRender ctype (Request p)
  , MimeUnrender ctype (JsonRpcResponse e r)
  ) =>
  HasClient m (RawJsonRpc ctype (JsonRpc method p e r))
  where
  type
    Client m (RawJsonRpc ctype (JsonRpc method p e r)) =
      p -> m (JsonRpcResponse e r)

  clientWithRoute _ _ req p =
    client req jsonRpcRequest
   where
    client = clientWithRoute (Proxy @m) endpoint
    jsonRpcRequest = Request (symbolVal $ Proxy @method) p (Just 0)

    endpoint = Proxy @(JsonRpcEndpoint ctype (JsonRpc method p e r))

  hoistClientMonad _ _ f x p = f $ x p

instance
  ( RunClient m
  , KnownSymbol method
  , MimeRender ctype (Request p)
  ) =>
  HasClient m (RawJsonRpc ctype (JsonRpcNotification method p))
  where
  type
    Client m (RawJsonRpc ctype (JsonRpcNotification method p)) =
      p -> m NoContent

  clientWithRoute _ _ req p =
    client req jsonRpcRequest
   where
    client = clientWithRoute (Proxy @m) endpoint
    jsonRpcRequest = Request (symbolVal $ Proxy @method) p Nothing

    endpoint = Proxy @(JsonRpcEndpoint ctype (JsonRpcNotification method p))

  hoistClientMonad _ _ f x p = f $ x p
