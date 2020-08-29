
type 'a t = {
    deep:bool;
    dft:[ `After | `Before ] option;
    filter:(bool -> string -> string -> bool);
    follow_links:bool;
    error:(exn -> string -> 'a -> unit);
}

val create :
           ?deep:bool ->
           ?dft:[ `After | `Before ] ->
           ?glob:string ->
           ?filter:(bool -> string -> string -> bool) ->
           ?follow_links:bool ->
           ?error:(exn -> string -> 'a -> unit) -> unit -> 'a t

val globber :  ?pathname:bool -> string -> (string -> bool)
