{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

#if MIN_VERSION_servant_server(0,18,0)
{-# LANGUAGE UndecidableInstances  #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Servant.Server.JsonRpc
--
-- This module provides support for writing handlers for JSON-RPC endpoints
--
-- > type Mul = JsonRpc "mul" (Int, Int) String Int
-- > mulHandler :: (Int, Int) -> Handler (Either (JsonRpcErr String) Int)
-- > mulHandler = _
--
-- > type Add = JsonRpc "add" (Int, Int) String Int
-- > addHandler :: (Int, Int) -> Handler (Either (JsonRpcErr String) Int)
-- > addHandler = _
--
-- > type API = Add :<|> Mul
-- > server :: Application
-- > server = serve (Proxy @(RawJsonRpc API)) $ addHandler :<|> mulHandler
module Servant.Server.JsonRpc
    ( serveJsonRpc
    , RouteJsonRpc (..)
    , module Servant.JsonRpc
    , PossibleContent
    , PossibleJsonRpcResponse
    ) where

import           Data.Aeson               (FromJSON (..), ToJSON (..), Value)
import           Data.Aeson.Types         (parseEither)
import           Data.Bifunctor           (bimap)
import           Data.Kind                (Type)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Proxy               (Proxy (..))
import           GHC.TypeLits             (KnownSymbol, symbolVal)
import           Servant.API              (NoContent (..), Post, ReqBody,
                                           (:<|>) (..), (:>))
import           Servant.API.ContentTypes (AllCTRender (..))

#if MIN_VERSION_servant_server(0,18,0)
import           Servant.Server           (DefaultErrorFormatters,
                                           ErrorFormatters, Handler,
                                           HasContextEntry, HasServer (..),
                                           type (.++))
#elif MIN_VERSION_servant_server(0,14,0)
import           Servant.Server           (Handler, HasServer (..))
#endif

import           Servant.JsonRpc


-- | Since we collapse an entire JSON RPC api down to a single Servant
--   endpoint, we need a type that /can/ return content but might not.
data PossibleContent a = SomeContent a | EmptyContent


instance ToJSON a => AllCTRender '[JSONRPC] (PossibleContent a) where
    handleAcceptH px h = \case
        SomeContent x -> handleAcceptH px h x
        EmptyContent  -> handleAcceptH px h NoContent


type PossibleJsonRpcResponse = PossibleContent (JsonRpcResponse Value Value)


type RawJsonRpcEndpoint
    = ReqBody '[JSONRPC] (Request Value)
   :> Post '[JSONRPC] PossibleJsonRpcResponse


#if MIN_VERSION_servant_server(0,18,0)
instance (RouteJsonRpc api, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters) => HasServer (RawJsonRpc api) context where
#elif MIN_VERSION_servant_server(0,14,0)
instance RouteJsonRpc api => HasServer (RawJsonRpc api) context where
#endif
    type ServerT (RawJsonRpc api) m = RpcHandler api m
    route _ cx = route endpoint cx . fmap (serveJsonRpc pxa pxh)
        where
        endpoint = Proxy @RawJsonRpcEndpoint
        pxa      = Proxy @api
        pxh      = Proxy @Handler

    hoistServerWithContext _ _ f x = hoistRpcRouter (Proxy @api) f x


-- | This internal class is how we accumulate a map of handlers for dispatch
class RouteJsonRpc a where
    type RpcHandler a (m :: Type -> Type)
    jsonRpcRouter
        :: Monad m => Proxy a -> Proxy m -> RpcHandler a m
        -> Map String (Value -> m (PossibleContent (Either (JsonRpcErr Value) Value)))
    hoistRpcRouter :: Proxy a -> (forall x . m x -> n x) -> RpcHandler a m -> RpcHandler a n


generalizeResponse
    :: (ToJSON e, ToJSON r)
    => Either (JsonRpcErr e) r
    -> Either (JsonRpcErr Value) Value
generalizeResponse = bimap repack toJSON
    where
    repack e = e { errorData = toJSON <$> errorData e }


onDecodeFail :: String -> JsonRpcErr e
onDecodeFail msg = JsonRpcErr invalidParamsCode msg Nothing


instance (KnownSymbol method, FromJSON p, ToJSON e, ToJSON r) => RouteJsonRpc (JsonRpc method p e r) where
    type RpcHandler (JsonRpc method p e r) m = p -> m (Either (JsonRpcErr e) r)

    jsonRpcRouter _ _ h = Map.fromList [ (methodName, h') ]
        where
        methodName = symbolVal $ Proxy @method
        onDecode   = fmap generalizeResponse . h

        h' = fmap SomeContent
           . either (return . Left . onDecodeFail) onDecode
           . parseEither parseJSON

    hoistRpcRouter _ f x = f . x


instance (KnownSymbol method, FromJSON p) => RouteJsonRpc (JsonRpcNotification method p) where
    type RpcHandler (JsonRpcNotification method p) m = p -> m NoContent

    jsonRpcRouter _ _ h = Map.fromList [ (methodName, h') ]
        where
        methodName = symbolVal $ Proxy @method
        onDecode x = EmptyContent <$ h x

        h' = either (return . SomeContent . Left . onDecodeFail) onDecode
           . parseEither parseJSON

    hoistRpcRouter _ f x = f . x


instance (RouteJsonRpc a, RouteJsonRpc b) => RouteJsonRpc (a :<|> b) where
    type RpcHandler (a :<|> b) m = RpcHandler a m :<|> RpcHandler b m

    jsonRpcRouter _ pxm (ha :<|> hb) = jsonRpcRouter pxa pxm ha <> jsonRpcRouter pxb pxm hb
        where
        pxa = Proxy @a
        pxb = Proxy @b

    hoistRpcRouter _ f (x :<|> y) = hoistRpcRouter (Proxy @a) f x :<|> hoistRpcRouter (Proxy @b) f y


-- | This function is the glue required to convert a collection of
-- handlers in servant standard style to the handler that 'RawJsonRpc'
-- expects.
serveJsonRpc
    :: (Monad m, RouteJsonRpc a)
    => Proxy a
    -> Proxy m
    -> RpcHandler a m
    -> Request Value
    -> m PossibleJsonRpcResponse
serveJsonRpc px pxm hs (Request m v ix')
    | Just h <- Map.lookup m hmap
    = h v >>= \case
        SomeContent (Right x) | Just ix <- ix' -> return . SomeContent $ Result ix x
                              | otherwise      -> return . SomeContent $ Errors ix' invalidRequest
        SomeContent (Left e)                   -> return . SomeContent $ Errors ix' e
        EmptyContent                           -> return EmptyContent
    | otherwise = return . SomeContent $ Errors ix' missingMethod
    where
    missingMethod  = JsonRpcErr methodNotFoundCode ("Unknown method: " <> m) Nothing
    hmap           = jsonRpcRouter px pxm hs
    invalidRequest = JsonRpcErr invalidRequestCode "Missing id" Nothing
