
type 'a t = {
    deep:bool;
    dft:[ `After | `Before ] option;
    filter:(bool -> string -> string -> bool);
    follow_links:bool;
    error:(exn -> string -> 'a -> unit);
}

let ok _dir _file _path = true
let no_error _exn _path _filename = ()

let globber ?pathname glob =
  let regexp = Re.Glob.glob ~anchored:true ?pathname glob in
  let re = Re.compile regexp in
  Re.execp re

let create
    ?(deep=false)
    ?dft
    ?glob
    ?(filter=ok)
    ?(follow_links=false)
    ?(error=no_error)
    () =
  let filter = match glob with
    | None -> filter
    | Some glob ->
      let glob = globber glob in
      fun is_dir file path ->
        (is_dir || glob file) && filter is_dir file path
  in
  {
    deep;
    dft;
    filter;
    follow_links;
    error;
  }
