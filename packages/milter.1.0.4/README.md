# OCaml-Milter

## Introduction

This library provides OCaml bindings to [libmilter](https://www.milter.org/).

## Build and installation

OCaml-Milter can be installed via opam.

    $ opam install milter

## Limitations

Since libmilter uses pthreads internally, this module is thread-safe. However,
due to the current OCaml implementation, each of the milter callbacks must
acquire a global runtime lock while being executed, meaning that effectively
only a single thread will be running at a given time.

One interesting possibility is to patch libmilter so that it sets the
`SO_REUSEPORT` socket option, which allows you to spawn multiple milter
procecess listening on the same address and port, with kernel-provided
load-balancing.

In the sendmail source, you can add the following code in
`libmilter/listener.c`, right after the point where `SO_REUSEADDR` is set.

```c
+       sockopt = 1;
+       if (
+#if NETUNIX
+           addr.sa.sa_family != AF_UNIX &&
+#endif /* NETUNIX */
+           setsockopt(sock, SOL_SOCKET, SO_REUSEPORT, (void *) &sockopt,
+                      sizeof(sockopt)) == -1)
+       {
+               smi_log(SMI_LOG_ERR,
+                       "%s: set reuseport failed (%s)", name,
+                       sm_errstring(errno));
+               (void) closesocket(sock);
+               return INVALID_SOCKET;
+       }
+
```
