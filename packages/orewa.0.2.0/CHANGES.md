0.2.0
=====

* Implement pipelining while simplifying parsing: it is now possible to queue
  up a lot of requests to Redis that will be fulfilled in order (thanks,
  @andersfugmann)
* Remove `` `Eof`` error code since it was semantically equal to ``
  `Connection_closed`` (@andersfugmann)
* Added `Always` option to `SET` to explicitly specify the default

0.1.1
=====

* Expose result of `SET`
* Fix bug when setting non-existing key

0.1.0
=====

* Initial release
* Designed to be async thread-safe
* Supports all string commands of Redis 5.0 and a few others
* All implemented commands tested against an actual Redis server
