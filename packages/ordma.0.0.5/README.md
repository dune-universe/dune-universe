# Ordma

This library provides RDMA capabilities for OCaml. It wraps librdmacm's rsocket
API and provides lwt bindings (and an engine) for the calls.

The API tries to provide a drop in replacement for Lwt_unix, but you need to
select the rselect engine as all events on rsockets need to be handled by rpoll:

```ocaml
let () =
  let engine = new Lwt_rsocket.rselect in
  Lwt_engine.set engine;
  let t = ...
  in
  Lwt_main.run t
```

The rselect engine can be used for normal files and sockets too.
