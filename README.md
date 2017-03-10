# language-server

The [language-server protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md) is a set of JSON-RPC methods and data types that enable code editors, in particular VS Code, to talk to server backends that provide IDE features such as symbol lookup, type checking etc.

The goal of this package is to provide a Haskell implementation of the protocol. It takes care of the JSON-RPC packing and unpacking and manages procedure calls and notifications in either direction. The package comes with an example plugin for VS Code (not implemented yet).

# Current status

This package is not usable in its current state and has not been published.

# License

BSD-3, see LICENSE

# Contributions

Bug reports, pull requests, feature requests are welcome