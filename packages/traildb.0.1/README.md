# OCaml bindings for TrailDB

Here are OCaml bindings for [TrailDB](http://traildb.io/),
an efficient tool for storing and querying series of events.

Pre-requisites
--------------
* [OCaml](http://caml.inria.fr/)
* [TrailDB](https://github.com/traildb/traildb) 
* [uuidm](https://opam.ocaml.org/packages/uuidm/) OCaml module for UUIDs.

To build from source.
* [jbuilder](https://opam.ocaml.org/packages/jbuilder/) OCaml build system.

# Install

From source:

```sh
$ make            # use jbuilder
$ make test
$ make install    # use opam
```

Or using directly jbuilder:

```sh
$ jbuilder build @install
$ jbuilder runtest
$ jbuilder install
```

# Documentation

A [TrailDB file](http://traildb.io/docs/technical_overview/)
is a read only collection of trails,
*i.e.* keyed series of event records with a timestamp
and sharing a common set of fields.

The API is documented in [lib/trailDB.mli](lib/trailDB.mli).

# Usage

A TrailDB file is created after a series of event records with
* a trail identifier,
* a timestamp
* and a shared set of fields.

```ocaml
  (* Start the creation of a new traildb file to record events
     with two fields (measure kind and value) plus an implicit timestamp *)
  let db = TrailDB.Cons.open_w "/tmp/foo" ["measure";"value"] in

  (* Make some trail identifiers *)
  let uuid_0 = Uuidm.(v5 ns_oid "trail_0") in
  let uuid_1 = Uuidm.(v5 ns_oid "trail_1") in

  (* Insert events with a string value for each field *)
  TrailDB.Cons.add db uuid_0  123456L ["temperature";"22"];
  TrailDB.Cons.add db uuid_1  123456L ["temperature";"12"];
  TrailDB.Cons.add db uuid_1  123457L ["temperature";"13"];
  TrailDB.Cons.add db uuid_0  123457L ["temperature";"23"];
  TrailDB.Cons.add db uuid_0  123458L ["temperature";"24"];
  TrailDB.Cons.add db uuid_1  123458L ["temperature";"14"];
  TrailDB.Cons.add db uuid_1  123459L ["temperature";"13"];

  (* Dump on disk the TrailDB file *)
  TrailDB.Cons.finalize db
```

Once build a TrailDB database is read only file.

```ocaml
  (* Open a read only TrailDB file *)
  let db = TrailDB.open_r "/tmp/foo.tdb" in

  (* Get some meta data *)
  assert (TrailDB.num_trails db = 2L);
  assert (TrailDB.num_events db = 7L);
  assert (TrailDB.num_fields db = 3L); (* timestamp, measure, value *)

  (* Get the identifier of a trail given its UUIDs *)
  let trail_uuid = Uuidm.(v5 ns_oid "trail_1") in
  let (Some trail_id) = TrailDB.get_trail_id db trail_uuid in

  (* Extract the events of a trail using a cursor *)
  let cursor = TrailDB.Cursor.create db in
  TrailDB.Cursor.get_trail cursor trail_id;

  let rec loop () =
    match TrailDB.Cursor.next cursor with
    | None -> ()
    | Some event ->
      let [measure;value] =
        event.TrailDB.values
        |> List.map (TrailDB.get_item_value db)
      in
      Printf.printf "(%Ld,%s,%s)\n" event.TrailDB.timestamp measure value
      |> loop
  in
  loop ()
```
