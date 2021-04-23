## ca-certs - detect root CA certificates from the operating system

TLS requires a set of root anchors (Certificate Authorities) to authenticate
servers. This library exposes this list so that it can be registered with
[ocaml-tls].

[ocaml-tls]: https://github.com/mirleft/ocaml-tls
