### v2.7.0 (30-Oct-2019)

* Adapt to mirage-net 3.0.0 changes (@hannesm)

### v2.6.0 (26-Feb-2019)

* Adapt to mirage-net 2.0.0 changs (@hannesm)

### v2.5.0 (06-Feb-2019)

* Build with dune instead of jbuilder (@avsm)
* Upgrade opam metadata to 2.0 (@avsm)
* Refresh Travis matrix to 4.07 as well (@avsm)

### 2.4.1 (16-Jun-2017)

* Build with jbuilder
* Update to use io-page-unix

### 2.4.0 (02-Apr-2017)

* Expose the underlying fd (#40, from @samoht)

### 2.3.0 (20-Jan-2017)

* Support MirageOS 3; drop support for earlier versions
* `listen` no longer restarts itself when canceled (#36, from @yomimono)
* Remove misleading error message referencing OSX (#31, from @hannesm)
* Use topkg instead of OASIS
* `writev` no longer fails when called with a list of total length > 1 page (#26, by @yomimono)
* `disconnect` no longer causes a crash on invocation (#26, by @yomimono)

### 2.2.3 (17-May-2016)

* Generate a random MAC address for the interface on each run. (#24, from @yomimono)
* Remove unused id type (#21, from @talex5)

### 2.2.2 (07-Jun-2015)

* Force non-blocking mode in the tun file descriptor to workaround
  a Linux 3.19+ kernel bug (see mirage/ocaml-tuntap#15).
  Requires tuntap 1.3.0+ for the corresponding fix.

### 2.2.1 (02-Jun-2015)

* Make `Netif.listen` tail-recursive. This fixes a long-standing (small)
  memory leak (#16, by @hannesm and @samoht)
* Expose `Netif.pp_error`

### 2.2.0 (07-Mar-2015)

* Leave the tuntap persistence flag unchanged (#9, from @infidel)
* Add explicit `connect` function to interface (#10)

### 2.1.0 (27-Jan-2015)

* Support `io-page` 1.3.0+ interface.
* Add local `opam` file for convenient OPAM 1.2 developer workflow.

### 2.0.0 (14-Aug-2014)

* Make `mac` field mutable and add a `set_mac` function to modify it.

### 1.1.1 (30-May-2014)

* Improve error message for permission denied (#6).
* Fix the order of linking to ensure `io-page.unix` comes first.
  This works around a linking hack to ensure the C symbols load.

### 1.1.0 (01-Feb-2014)

* Depend on the unified io-page library instead of io-page-unix.

### 1.0.0 (08-Dec-2013)

* Use the NETWORK.listen interface from mirage-types-0.5.0.
* Fix findlib package name to `mirage-net-unix`.

### 0.9.0 (08-Dec-2013)

* Adapt to mirage-types-0.4.0
* Do not pack Netif, making it a toplevel module instead.
* import first version of Netif driver based on mirage-platform-0.9.8
