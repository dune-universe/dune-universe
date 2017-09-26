OCaml bindings for kyoto cabinet DBM
====================================

Pre-requisites
--------------
* [OCaml](http://caml.inria.fr/)
* [Kyoto Cabinet](http://fallabs.com/kyotocabinet/)

License
-------
GNU General Public License.

Install
-------
    $ make         # use jbuilder
    $ make test
    $ make install

Documentation
----------
The API is documented in [lib/kyoto.mli](lib/kyoto.mli)

Basic
-----

```ocaml
    #use "topfind";;
    #require "kyotocabinet";;

    (* create a database, here an in-memory tree database. *)
    let db = Kyoto.opendb "+" [Kyoto.OWRITER; Kyoto.OCREATE];;

    (* store records *)
    Kyoto.set db "foo" "hop";;
    Kyoto.set db "bar" "step";;
    Kyoto.set db "baz" "jump";;
    Kyoto.set db "baz2" "jump";;

    (* retrieve records *)
    Kyoto.get db "foo";;
    Kyoto.get db "xoxox";;

    (* update records *)
    Kyoto.set db "bar" "step2";;
    Kyoto.remove db "baz2";;

    (* fold the whole database *)
    Kyoto.fold db (fun n x -> n+1) 0;;

    (* use a cursor to iter over the database *)
    let cursor = Kyoto.cursor_open db;;
    
    Kyoto.cursor_next cursor;;
    Kyoto.cursor_next cursor;;
    Kyoto.cursor_next cursor;;
    Kyoto.cursor_next cursor;;

    Kyoto.cursor_close cursor;;

    (* close the database *)
    Kyoto.close db;;
```
