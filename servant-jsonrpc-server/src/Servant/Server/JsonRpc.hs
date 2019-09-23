{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Servant.Server.JsonRpc
--
-- This module provides support for writing handlers for JSON-RPC endpoints
--
-- > type Mul = JsonRpcEndpoint "mul" (Int, Int) String Int
-- > mulHandler :: (Int, Int) -> Handler (Either (JsonRpcErr String) Int)
-- > mulHandler = _
-- >
-- > server :: Application
-- > server = serve (Proxy @Mul) mulHandler
module Servant.Server.JsonRpc
    ( module Servant.JsonRpc
    ) where


import           Control.Monad.Error.Class (throwError)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Proxy                (Proxy (..))
import           GHC.TypeLits              (KnownSymbol)
import           Servant.API               (NoContent)
import           Servant.Server            (HasServer (..), err400)

import           Servant.JsonRpc

instance (KnownSymbol method, FromJSON p, ToJSON e, ToJSON r)
    => HasServer (JsonRpc method p e r) context where

    type ServerT (JsonRpc method p e r) m = p -> m (Either (JsonRpcErr e) r)

    route _ cx = route endpoint cx . fmap f
        where
        f x (Request _ p (Just ix)) = g ix <$> x p
        f _ _                       = throwError err400
        g ix (Right r) = Result ix r
        g ix (Left e)  = Errors (Just ix) e

        endpoint = Proxy @(JsonRpcEndpoint (JsonRpc method p e r))

    hoistServerWithContext _ _ f x p = f $ x p


instance (KnownSymbol method, FromJSON p)
    => HasServer (JsonRpcNotification method p) context where

    type ServerT (JsonRpcNotification method p) m = p -> m NoContent

    route _ cx = route endpoint cx . fmap f
        where
        f x (Request _ p _) = x p
        endpoint = Proxy @(JsonRpcEndpoint (JsonRpcNotification method p))

    hoistServerWithContext _ _ f x p = f $ x p
