### v0.5.0 (2019-10-30)

- adapt to mirage-net 3.0.0 interface changes (#28 @hannesm)

### v0.4.2 (2019-02-24)

- adjust to mirage-net 2.0.0 changes
- port build system to dune

### v0.4.1 (2019-01-10)

- Ipaddr 3.0.0 compatibility
- remove open Result
- require 4.04.2

### v0.4.0 (2017-06-12)

- Add optional `size_limit` to `Vnetif.create` to make it easier to test
  MTUs (e.g. in TCP/IP) (#16 @yomimono).
- Port build to Jbuilder.
- Add Travis CI tests.

### v0.3.1 (2016-02-22)

- Don't export `Delayed_backend` yet as it is not ready for primetime use.

### v0.3

- Use topkg
- Use new mirage-time and mirage-clock modules
- Adapt to MirageOS version 3 errors scheme

### v0.2 (2016-09-10)

- New call `unregister_and_flush` lets a node wait for all its listener
  callbacks to return before unregistering from the backend (#4)
- Support more than 254 connected nodes

### v0.1 (2015-04-30)

- Initial public release.
