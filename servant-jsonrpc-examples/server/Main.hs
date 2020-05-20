{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Proxy               (Proxy (..))
import           Data.Time                (defaultTimeLocale, formatTime,
                                           getCurrentTime)
import           Network.Wai.Handler.Warp (run)
import           Servant.API              ((:<|>) (..), NoContent (..))
import           Servant.Server           (Handler, Server, serve)

import           Servant.JsonRpc.Example  (API)
import           Servant.Server.JsonRpc   (JsonRpcErr)


main :: IO ()
main = run 8080 $ serve (Proxy @API) server


server :: Server API
server = (add :<|> multiply :<|> printMessage) :<|> (getTime :<|> printMessage)


add :: (Int, Int) -> Handler (Either (JsonRpcErr String) Int)
add = return . Right . uncurry (+)


multiply :: (Int, Int) -> Handler (Either (JsonRpcErr String) Int)
multiply = return . Right . uncurry (*)


printMessage :: String -> Handler NoContent
printMessage msg = NoContent <$ liftIO (putStrLn msg)


getTime :: Handler String
getTime = timeString <$> liftIO getCurrentTime
    where
    timeString = formatTime defaultTimeLocale "%T"
