### mirage-net-macosx -- MacOS X implementation of the Mirage NETWORK interface.

This interface exposes raw Ethernet frames using the
[Vmnet](https://github.com/mirage/ocaml-vmnet) framework that
is available on MacOS X Yosemite onwards.  It is suitable for
use with an OCaml network stack such as the one found at
<https://github.com/mirage/mirage-tcpip>.

## limitations

The backing [Vmnet.framework](https://developer.apple.com/documentation/vmnet) supports only IPV4 over DHCP, and does not allow for static configuration.  `mirage-net-macosx` therefore must have access to a DHCP server in order to get a working network configuration, and unikernels built using `mirage-net-macosx` must be configured with `--dhcp=true`.

## where it fits

For a complete system that uses this, please see the
[MirageOS](http://mirage.io) homepage.

- docs: <https://mirage.github.io/mirage-net-macosx/>
- Issues: <https://github.com/mirage/mirage-net-macosx/issues>
- Email: <mirageos-devel@lists.xenproject.org>
