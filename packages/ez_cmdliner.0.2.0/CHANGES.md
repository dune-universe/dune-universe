
## v0.2.0 (2020-11-27)

* Version interface: use `open Ezcmd.V1` (old) or `open Ezcmd.V2` (new)
* Version V2 replaces type `command` by type `sub`, that should not be used
   directly. Instead, the function `Ezcmd.sub` should be used to build it.
   It's better for long term compatibility.
* Add a `?version` argument to `info` and `sub`, to indicate at which version
   they appeared
* Module `V2.EZCMD.RAWTYPES` can be used to switch from abstract types to
  manifest types to generate documentation, with `raw_env`, `raw_info` and
  `raw_sub`

## v0.1.1 (2020-08-26)

* Renamed to ez_cmdliner
* Use `drom` to generate distribution files

## v0.1.0 (2020-07-20)

Initial version as ez-cmdliner

