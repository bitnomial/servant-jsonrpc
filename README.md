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
