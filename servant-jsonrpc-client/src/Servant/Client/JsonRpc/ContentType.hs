{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Client.JsonRpc.ContentType
  ( UsingContentType
  , SupportedContentType(..)
  , IsSupportedContentType(..)
  ) where

import Data.Proxy
import Servant.API.ContentTypes
import Servant.Client.Core
import Servant.JsonRpc
import Servant.API

-- | Manually specify the content type
--
-- Defaults to JSONRPC.
data UsingContentType ctyp a

data SupportedContentType ctyp where
  SupportedContentTypeJSON    :: SupportedContentType JSON
  SupportedContentTypeJSONRPC :: SupportedContentType JSONRPC

class IsSupportedContentType ctyp where
  isSupportedContentType :: Proxy ctyp -> SupportedContentType ctyp

instance IsSupportedContentType JSON where
  isSupportedContentType _ = SupportedContentTypeJSON
instance IsSupportedContentType JSONRPC where
  isSupportedContentType _ = SupportedContentTypeJSONRPC

-- | Distribute 'UsingContentType' over '(:<>)'
instance ( RunClient m
         , HasClient m (UsingContentType ctyp a)
         , HasClient m (UsingContentType ctyp b)
         )
  => HasClient m (UsingContentType ctyp (a :<|> b)) where

  type Client m (UsingContentType ctyp (a :<|> b))
      = Client m (UsingContentType ctyp a :<|> UsingContentType ctyp b)

  clientWithRoute pm _ =
    clientWithRoute
      pm
      (Proxy @(UsingContentType ctyp a :<|> UsingContentType ctyp b))

  hoistClientMonad pm _ hoist =
    hoistClientMonad
      pm
      (Proxy @(UsingContentType ctyp a :<|> UsingContentType ctyp b))
      hoist

