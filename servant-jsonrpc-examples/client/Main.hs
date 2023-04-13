{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Proxy              (Proxy (..))
import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Servant.API             ((:<|>) (..), NoContent)
import           Servant.Client          (ClientM, client, mkClientEnv,
                                          parseBaseUrl, runClientM)
import           Servant.Client.JsonRpc  (JsonRpcResponse)
import           Servant.JsonRpc.Example (API, NonEndpoint)


main :: IO ()
main = do
    env <- mkClientEnv <$> newManager defaultManagerSettings <*> parseBaseUrl "http://localhost:8080"
    void . flip runClientM env $ do
        jsonRpcPrint "Starting RPC calls"
        liftIO . print =<< add (2, 10)
        liftIO . print =<< multiply (2, 10)

        printMessage "Starting REST calls"
        liftIO . print =<< getTime

    -- A JSON-RPC error response
    print =<< runClientM (launchMissiles 100) env


add, multiply :: (Int, Int) -> ClientM (JsonRpcResponse String Int)
jsonRpcPrint :: String -> ClientM NoContent
getTime :: ClientM String
printMessage :: String -> ClientM NoContent
(add :<|> multiply :<|> jsonRpcPrint) :<|> (getTime :<|> printMessage) = client $ Proxy @API


launchMissiles :: Int -> ClientM (JsonRpcResponse String Bool)
launchMissiles = client $ Proxy @NonEndpoint
