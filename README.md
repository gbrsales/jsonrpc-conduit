jsonrpc-conduit implements the basic building block of a JSON-RPC 2.0 server.

It provides a Conduit that consumes RPC requests and invokes user-provided
functions to handle them. Conversion of values to and from JSON is almost
completely automatic thanks to the aeson library.

The JSON-RPC conduit is generic with respect to the channel used to exchange
data with the client. It can use a network connection or, for example,
the standard input / ouput of a process.

![CI Status](https://github.com/gbrsales/jsonrpc-conduit/actions/workflows/ci.yaml/badge.svg)
