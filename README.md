servant-jsonrpc
====

This module extends [servant][1] to make it easy to define [JSON-RPC][2] servers and
clients.

[1]: https://haskell-servant.readthedocs.io/en/stable/
[2]: https://www.jsonrpc.org


Notes
----

* Does not enforce the `jsonrpc` key in the response
* Does not enforce `id` key on error responses
* We allow for server messages with `null` for both `error` and `result` keys
* The client interface hides the `id` key since the semantics of HTTP determine
  which server responses correspond to which client requests.

Examples
----

### Server

```haskell
module Server where

import Data.Map as Map
import Servant.Server
import Servant.JsonRpc


type ThingId = String
data Thing = Thing Int String


type Api = JsonRpc "getthing" ThingId () (Maybe Thing)


getThing :: Map ThingId Thing -> ThingId -> Handler (Maybe Thing)
getThing = pure . flip Map.lookup

app :: Map ThingId Thing -> Application
app m = serve (Proxy @Api) $ getThing m
```

### Client

```haskell
module Client where

import Servant.JsonRpc
import Servant.Client
import Data.Proxy

import Server

getThing :: ThingId -> ClientM (JsonRpcResponse () (Maybe Thing))
getThing = client (Proxy @Api)
```
