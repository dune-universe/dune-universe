OCaml Gremlin Client
====================================

Overview
------------------------------------

This is a _very rough_ [Apache Tinkerpop3](http://tinkerpop.apache.org/docs/current/) Gremlin client library written in OCaml. It was implemented using the [Graph Driver Provider Requirements](http://tinkerpop.apache.org/docs/current/dev/provider/#_graph_driver_provider_requirements).

Given this is my first public OCaml library and I was fairly rushed, it's not pretty.

I'd like to improve it which probably means completely refactoring it but I'm not sure when I'll get the chance. In the meantime, any suggestions or PRs are welcome.

Instructions
------------------------------------

### Build/Install

```sh
git clone https://github.com/bramford/ocaml-gremlin
cd ocaml-gremlin
opam pin add .
```

### Usage

Assuming you're running a gremlin server on `localhost:8182`, in a _toplevel_ such as `utop`:

```ocaml
#require "gremlin";;

let conn =
    Gremlin.Websocket.new_connection
    (Uri.of_string "http://localhost:8182/gremlin")

(* Add vertices *)
let add_vertices = Lwt_main.run (
  Gremlin.Websocket.run_queries_transaction
    conn
    ["g.addV('user').property('name', 'foo')";
    "g.addV('user').property('name', 'bar')";]
)

(* Get vertices *)
let get_vertices =
  Lwt_main.run (
    Gremlin.Websocket.run_query
      conn
      "g.V()"
  )
;;
```

See `examples/` for other examples.

To do
------------------------------------

There's obviously a lot to improve but here are some starting points:

- [ ] Properly close gremlin session on completion
- [ ] Handle streamed responses
- [ ] Add interface documentation (e.g. odoc)
- [ ] Implement proper logging (e.g. using `Logs`)
- [ ] Improve types and results
- [ ] Abstract internals and types properly
- [ ] Refactor/improve response handler (currently uses `Lwt_stream` but poorly)
- [ ] Refactor response JSON checking/parsing
- [ ] Improve concurrency (currently all serial using `Lwt_list.map_s`)
- [ ] Support other serialization formats (e.g. bytecode)
- [ ] Support Traversal OpProcessor
- [ ] Support REST interface
- [ ] _MirageOS_ compatibility
