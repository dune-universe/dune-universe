### 2.3.0 (2017-06-01)

* Build with jbuilder and release with topkg
* Update to new shared-memory-ring-lwt dependency

### 2.2.1 (2017-05-24)

* Link to `cstruct.lwt` explicitly rather than implicitly for the unix package.

### 2.2.0 (2016-12-21)

* Import `V1.CONSOLE` from `mirage-types` into `Mirage_console.S` and create
  a new `mirage-console` opam package (@samoht)
- Import `V1_LWT.CONSOLE` from `mirage-types-lwt` into `Mirage_console_lwt.S`
  and create a new `mirage-console-lwt` opam package (@samoht)

### 2.1.3 (2015-03-08-03)

* Fix installation of Unix library defaults by splitting out the
  base Unix dependency and the Xenctrl ones.  This needs a new `--enable-xenctrl`
  flag that explicitly depends on the Xen libraries being installed. (#36)
* Install the `mirage-console-cli` executable if it is built.

### 2.1.2 (2015-03-08)

* Add an explicit `connect` function to interface. (#34)
* Modernise Travis scripts with central sourcing.
* Only build Unix executable if relevant `xenctrl` libraries are installed.

### 2.1.1 (2015-01-23)

* Add an `error_message` function to turn an `error` into a string.

### 2.1.0 (2014-12-23)

* [xen]: for secondary consoles, Console.connect blocks in a watch waiting for
  a hotplug. This allows stand-alone console servers time to connect (such as
  xentropyd)

### 2.0.0 (2014-10-31)

* enable travis (for both xen and unix cases)
* fix dependencies: drop mirage-{xen,unix}; keep dependencies on implementation
  libraries and mirage-types
* switch build to OASIS
* add command-line tool to attach consoles
* add experimental support for named consoles
* add support for reading from consoles (so we can do user interaction)
* [xen] support connecting to additional (named) consoles
* [xen] don't zero the initial console ring
* install findlib packages as `mirage-console.[xen/unix]`

### 1.0.2 (2014-02-01):

* [xen] Fix console on resume by reattaching the ring.
* [xen] Switch to ocamlfind xen-{gnt,evtchn}.

### 1.0.1 (2013-12-08):

* Set the type of `id` in the Xen console to `string`.

### 1.0.0 (2013-12-08):

* Adapt to mirage-types-0.4.0 interface.

### 0.9.9 (2013-12-07):

* Install separate libraries for `mirage-console-unix` and `mirage-console-xen`.
* Update library dependencies for mirage-types-0.3.0
* Adapt to V1.CONSOLE interface.
* Initial public release, based on mirage/mirage-platform#0.9.8
