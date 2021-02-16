module LowLevel = struct
  (** delete a directory that may contain files *)
  let rmdir dir =
    Array.iter (fun x -> Sys.remove (Filename.concat dir x)) (Sys.readdir dir);
    Unix.rmdir dir

  (** copy a file to another *)
  let copy src dest =
    let cin = open_in src
    and cout = open_out dest
    and buff = Bytes.make 1024 ' '
    and n = ref 0 in
    while
      n := input cin buff 0 1024;
      !n <> 0
    do
      output cout buff 0 !n
    done;
    close_in cin;
    close_out cout

  (** rename a file *)
  let move src dest =
    try Unix.rename src dest
    with Unix.Unix_error (Unix.EXDEV, _, _) -> copy src dest

  let read_from fn f =
    let c = open_in fn in
    let r = f c in
    close_in c;
    r

  let write_to filename f =
    let chan = open_out filename in
    let r = f chan in
    close_out chan;
    r

  let write_to_formatted filename f =
    write_to filename (fun chan ->
        let fmt = Format.formatter_of_out_channel chan in
        let r = f fmt in
        Format.fprintf fmt "@?";
        r)

  let exists = Sys.file_exists

  let rm = Sys.remove
end

(* Filename.dir_sep in Ocaml >= 3.11.2 *)
let dir_sep =
  match Sys.os_type with
  | "Cygwin" | "Unix" -> '/'
  | "Win32" -> '\\'
  | _ -> assert false

let dir_sep_string = String.make 1 dir_sep

module Dir = struct
  type t = { dirs : string list; is_relative : bool }

  (* the type t is a stack of directories, innermost is on top *)

  let print_sep fmt () = Format.pp_print_char fmt dir_sep

  let to_string s =
    let prefix = if s.is_relative then "" else "/" in
    Misc.sprintf "%s%a" prefix
      (Misc.print_list print_sep Format.pp_print_string)
      (List.rev s.dirs)

  let rec eat_dirsep_back i s =
    if s.[i] = dir_sep then eat_dirsep_back (i - 1) s else i

  let rec eat_dirsep_forw i s =
    if s.[i] = dir_sep then eat_dirsep_forw (i + 1) s else i

  let split_sep s =
    let l = String.length s in
    if l = 0 then []
    else
      let rec aux acc i =
        let i = eat_dirsep_forw i s in
        try
          let j = String.index_from s i dir_sep in
          let dir = String.sub s i (j - i) in
          let acc = dir :: acc in
          if j = l - 1 then acc else aux acc (j + 1)
        with Not_found ->
          let j = eat_dirsep_back (l - 1) s in
          String.sub s i (j - i + 1) :: acc
      in
      aux [] 0

  let from_string s =
    let f = { dirs = split_sep s; is_relative = Filename.is_relative s } in
    f

  let concat d1 d2 =
    assert d2.is_relative;
    { d1 with dirs = d2.dirs @ d1.dirs }

  let temp = from_string (Filename.get_temp_dir_name ())

  let mk t rights = Unix.mkdir (to_string t) rights

  let ch t = Sys.chdir (to_string t)

  let cwd () = from_string (Sys.getcwd ())

  let empty = { dirs = []; is_relative = true }

  let rm d = LowLevel.rmdir (to_string d)

  let compare = Stdlib.compare

  (* this one is actually difficult ... for now we just compare the elements
       *)
end

type t = { dir : Dir.t; bn : string; ext : string option }

let dir t = t.dir

let extension t = match t.ext with Some s -> s | None -> ""

let basename t = t.bn

let file_to_string bn ext =
  match ext with None -> bn | Some ext -> Misc.sprintf "%s.%s" bn ext

let to_string t =
  let dir = Dir.to_string t.dir in
  let sep =
    if String.length dir <> 0 && dir.[String.length dir - 1] <> '/' then "/"
    else ""
  in
  Misc.sprintf "%s%s%s" (Dir.to_string t.dir) sep (file_to_string t.bn t.ext)

let debug_to_string t =
  Misc.sprintf "(%s,%s,%s)" (Dir.to_string t.dir) t.bn (extension t)

let split_ext s =
  let l = String.length s in
  if s.[l - 1] = '.' || s.[l - 1] = dir_sep then (s, None)
  else
    try
      let i = String.rindex_from s (l - 1) '.' in
      (String.sub s 0 i, Some (String.sub s (i + 1) (l - i - 1)))
    with Not_found -> (s, None)

let from_string s =
  let d = Dir.from_string s in
  match d.Dir.dirs with
  | [] -> { dir = d; bn = ""; ext = None }
  | fn :: dirs ->
      let bn, ext = split_ext fn in
      { dir = { Dir.dirs; is_relative = d.Dir.is_relative }; bn; ext }

let place d t = { t with dir = d }

let concat d t = { t with dir = Dir.concat d t.dir }

let append t s = { t with bn = t.bn ^ s }

let prepend t s = { t with bn = s ^ t.bn }

let set_ext t s =
  let ext = if s = "" then None else Some s in
  { t with ext }

let clear_dir t = { t with dir = Dir.empty }

let compare a b =
  let c = Stdlib.compare a.bn b.bn in
  if c <> 0 then c
  else
    let c = Stdlib.compare a.ext b.ext in
    if c <> 0 then c else Dir.compare a.dir b.dir

module Map = Map.Make (struct
  type t' = t

  type t = t'

  let compare = compare
end)

(** wrappers for low level functions *)
let move a b = LowLevel.move (to_string a) (to_string b)

let copy a b = LowLevel.copy (to_string a) (to_string b)

let read_from t f = LowLevel.read_from (to_string t) f

let write_to t f = LowLevel.write_to (to_string t) f

let write_to_formatted t f = LowLevel.write_to_formatted (to_string t) f

let exists t = LowLevel.exists (to_string t)

let rm t = LowLevel.rm (to_string t)

let open_out t = open_out (to_string t)

let open_in t = open_in (to_string t)

let open_in_gen f i t = open_in_gen f i (to_string t)

let mk ?(dir = Dir.empty) bn ext =
  let ext = if ext = "" then None else Some ext in
  { dir; bn; ext }
