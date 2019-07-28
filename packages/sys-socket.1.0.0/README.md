# sys-socket

This module provides OCaml ctypes bindings to system-specific low-level socket structure and data-types.

The interface is implemented using [ocaml-ctypes](https://github.com/ocamllabs/ocaml-ctypes) and is intended
to exposed the machine-specific, low-level details of the most important parts of socket implementations.

[Sys_socket](src/sys_socket.mli) provides an API compatible for both `Unix` and `Win32` systems, while [Sys_socket_unix](src/unix/sys_socket_unix.mli) provides the API specific to `Unix` systems, mostly the `sockaddr_u` structure.

On POSIX systems, the following headers define the bound types and structures:
* [sys/sock.h](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/sys/socket.h.html)
* [sys/un.h](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/sys/un.h.html)
* [netinet/in.h](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/netinet/in.h.html)

On windows systems, the following headers define the bound types and structures:
* [winsock.h](https://docs.microsoft.com/en-us/windows/win32/api/winsock/)
* [ws2tcpip.h](https://docs.microsoft.com/en-us/windows/win32/api/ws2tcpip/)

Its API mirrors as much as possible the original POSIX definitions, including integers representation (network bytes order,
host byte order). It is defined in [sys_socket.mli](src/sys_socket.mli)

A high-level wrapper for the OCaml [Unix](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html) module is provided as well.

Happy hacking!
