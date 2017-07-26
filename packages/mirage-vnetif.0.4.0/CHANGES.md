v0.4.0 2017-06-12
-----------------

- Add optional `size_limit` to `Vnetif.create` to make it easier to test
  MTUs (e.g. in TCP/IP) (#16 @yomimono).
- Port build to Jbuilder.
- Add Travis CI tests.

v0.3.1 2016-02-22
-----------------

- Don't export `Delayed_backend` yet as it is not ready for primetime use.

v0.3
----

- Use topkg
- Use new mirage-time and mirage-clock modules
- Adapt to MirageOS version 3 errors scheme

v0.2 2016-09-10
---------------

- New call `unregister_and_flush` lets a node wait for all its listener
  callbacks to return before unregistering from the backend (#4)
- Support more than 254 connected nodes

v0.1 2015-04-30
---------------

- Initial public release.
