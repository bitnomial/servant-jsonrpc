{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Servant.JsonRpc.Example
    ( API
    , NonEndpoint
    ) where

import           Servant.API     ((:<|>) (..), (:>), Get, NoContent, PlainText,
                                  Post, ReqBody)
import           Servant.JsonRpc (JsonRpc, JsonRpcNotification, RawJsonRpc)


type Add      = JsonRpc "add"      (Int, Int) String Int
type Multiply = JsonRpc "multiply" (Int, Int) String Int
type Print    = JsonRpcNotification "print" String


type RpcAPI = Add :<|> Multiply :<|> Print


type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI


type GetTime = "time" :> Get '[PlainText] String
type PrintMessage = "print" :> ReqBody '[PlainText] String :> Post '[PlainText] NoContent


type RestAPI = "rest" :> (GetTime :<|> PrintMessage)


type API = JsonRpcAPI :<|> RestAPI


type NonEndpoint = "json-rpc" :> RawJsonRpc (JsonRpc "launch-missles" Int String Bool)
