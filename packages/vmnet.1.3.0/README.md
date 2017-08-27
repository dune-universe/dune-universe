MacOS X `vmnet` NAT networking

MacOS X 10.10 (Yosemite) introduced the somewhat undocumented `vmnet`
framework.  This exposes virtual network interfaces to userland applications.
There are a number of advantages of this over previous implementations:

- Unlike [tuntaposx](http://tuntaposx.sourceforge.net/), this is builtin
  to MacOS X now and so is easier to package up and distribute for end users.
- `vmnet` uses the XPC sandboxing interfaces and should make it easier to
  drop a hard dependency on running networking applications as `root`.
- Most significantly, `vmnet` optionally supports NATing network traffic to the
  outside world, which was previously unsupported.

These OCaml bindings are constructed against the documentation contained
in the `<vmnet.h>` header file in Yosemite, and may not be correct due to
the lack of any other example code.  However, they do suffice to run
[MirageOS](http://openmirage.org) applications that can connect to the
outside world.

Note the application must be configured to use DHCP: static IPs are not supported.
To configure a MirageOS application use a command like:

```
mirage configure -t macosx --dhcp true
```

The bindings are also slightly complicated by the need
to interface [GCD](http://en.wikipedia.org/wiki/Grand_Central_Dispatch)
thread pools with the OCaml runtime, so please report any instabilities
that you see when using this interface as a consumer.

There are two libraries provided:

- `Vmnet` is the raw OCaml binding to the `vmnet` framework, using
   OCaml preemptive threads to handle synchronisation.
- `Lwt_vmnet` uses the [Lwt](http://ocsigen.org/lwt) framework to
  provide a monadic asynchronous I/O interface at a higher level.

Most users should use `Lwt_vmnet` to handle guest traffic.

- WWW: <https://github.com/mirage/ocaml-vmnet>
- Issues: <https://github.com/mirage/ocaml-vmnet/issues>
- Email: <mirageos-devel@lists.xenproject.org>
