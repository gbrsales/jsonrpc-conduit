jsonrpc-conduit implements the basic building block of a JSON-RPC 2.0 server.

It provides a Conduit that consumes RPC requests and invokes user-provided
functions to handle them. Conversion of values to and from JSON is almost
completely automatic thanks to the aeson library.

The JSON-RPC conduit is generic with respect to the channel used to exchange
data with the client. It can use a network connection or, for example,
the standard input / ouput of a process. The latter is demonstrated by the
"jsonrpc-conduit-demo" executable, which can be compiled using the "demo" flag.

[![Build Status](https://travis-ci.org/gbrsales/jsonrpc-conduit.svg?branch=master)](https://travis-ci.org/gbrsales/jsonrpc-conduit)
