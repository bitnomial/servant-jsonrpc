servant-jsonrpc
====

This module extends [servant][1] to make it easy to define JSON-RPC servers and
clients.

[1]: https://haskell-servant.readthedocs.io/en/stable/

Server
----

```haskell
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

Client
----

```haskell
import Servant.JsonRpc
import Servant.Client
import Data.Proxy


getThing :: ThingId -> ClientM (JsonRpcResponse () (Maybe Thing))
getThing = client (Proxy @Api)
```
