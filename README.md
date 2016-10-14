# `nix-binary-cache`

This is a Haskell implementation of a nix binary cache server (a.k.a.
nix repo) and a client which can perform uploads and fetches against
this server.

The server is modeled after the python implementation
[servenix](https://github.com/adnelson/servenix), but is intended to
support more features and have better performance. See that project's
readme for more information on the server itself until this project
becomes more mature.
