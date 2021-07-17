open Tezos_context_hash_irmin
open Encoding
module Node = Node (Hash) (Path) (Metadata)
module Inter = Irmin_pack.Inode.Make_internal (Conf) (Hash) (Node)

module Spec = struct
  type kind = Tree | Contents [@@deriving irmin]

  type entry = { name : Node.step; kind : kind; hash : Node.hash }
  [@@deriving irmin]

  type inode = Inter.Val.Concrete.t [@@deriving irmin]

  type node = { hash : Node.hash; bindings : entry list; inode : inode option }
  [@@deriving irmin]

  let entry (s, b) =
    match b with
    | `Node h -> { name = s; kind = Tree; hash = h }
    | `Contents (h, _) -> { name = s; kind = Contents; hash = h }

  let value e =
    ( e.name,
      match e.kind with
      | Tree -> `Node e.hash
      | Contents -> `Contents (e.hash, Metadata.default) )

  let inode = Inter.Val.to_concrete

  let of_t (t : Inter.Val.t) =
    let inode = if Inter.Val.stable t then None else Some (inode t) in
    let hash = Inter.Val.hash t in
    { hash; bindings = List.map entry (Inter.Val.list t); inode }

  let pp_entry = Irmin.Type.pp_json entry_t
  let pps = Irmin.Type.pp_json ~minify:false (Irmin.Type.list node_t)

  module N = Irmin.Hash.Typed (Store.Hash) (Store.Private.Node.Val)

  let hash_node = N.hash
  let pp_hash = Irmin.Type.pp Store.Hash.t

  let hash_of_string s =
    match Irmin.Type.of_string Store.Hash.t s with
    | Error (`Msg e) -> failwith e
    | Ok x -> x

  let kind_of_string = function
    | "Tree" -> Tree
    | "Contents" -> Contents
    | e -> failwith e
end

let contents x = `Contents (x, Metadata.default)
let node x = `Node x

module Gen = struct
  let init = Random.init
  let char () = char_of_int (33 + Random.int 94)
  let int () = Random.int ((1 lsl 30) - 1)
  let fixed_string n () = String.init n (fun _ -> char ())
  let string () = fixed_string (1 + Random.int (1 lsl 5)) ()
  let fixed_bytes n () = Bytes.init n (fun _ -> char ())
  let bytes () = fixed_bytes (Random.int (1 lsl 10)) ()
  let fixed_list n gen () = List.init n (fun _ -> gen ())
  let pair gen1 gen2 () = (gen1 (), gen2 ())
  let content () = bytes ()
  let hash () = content () |> Store.Contents.hash

  let atom () =
    let b = Random.bool () in
    hash () |> if b then contents else node

  let fixed_inode n () = fixed_list n (pair string atom) () |> Inter.Val.v

  let long_inode () =
    let len = Conf.stable_hash + Random.int (1 lsl 10) in
    fixed_inode len ()

  let short_inode () =
    let len = 1 + Random.int Conf.stable_hash in
    fixed_inode len ()
end

let rec mkdir s =
  try
    (match Filename.dirname s with "." -> () | parent -> mkdir parent);
    Unix.mkdir (Filename.basename s) 0o755
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

type file = OCaml_hash | Inodes | Nodes
type config = { prefix : string option; dir : string; n : int; seed : int }

let ( / ) = Filename.concat

let file dir prefix file =
  let file =
    match file with
    | OCaml_hash -> "ocaml_hash"
    | Inodes -> "inodes"
    | Nodes -> "nodes"
  in
  let prefix = match prefix with None -> "" | Some p -> p ^ "." in
  dir / Fmt.strf "%s%s.json" prefix file

let check_int msg expected got =
  if expected <> got then (
    Fmt.epr "%s: expected %d, got %d\n%!" msg expected got;
    failwith __LOC__)

let check_hash msg expected got =
  if expected <> got then (
    Fmt.epr "%s: expected %a, got %a\n%!" msg Spec.pp_hash expected Spec.pp_hash
      got;
    failwith __LOC__)

module OCaml_hashes = struct
  let generate config =
    mkdir config.dir;
    let path = file config.dir config.prefix OCaml_hash in
    Fmt.pr "Generating ocaml_hash test cases in `%s'@." path;
    let oc = open_out path in
    Gen.init config.seed;
    Gen.(fixed_list config.n (pair string int)) ()
    |> List.map (fun (s, seed) ->
           `Assoc
             [
               ("s", `String s);
               ("seed", `Int seed);
               ("ocaml_hash", `Int (Hashtbl.seeded_hash seed s));
             ])
    |> (fun l -> `List l)
    |> Yojson.pretty_to_channel oc;
    close_out oc

  let check dir =
    let path = file dir None OCaml_hash in
    let ic = open_in path in
    let v = Yojson.Safe.from_channel ic |> Yojson.Safe.to_basic in
    let open Yojson.Basic.Util in
    List.iter
      (fun json ->
        let s = json |> member "s" |> to_string in
        let seed = json |> member "seed" |> to_int in
        let ocaml_hash = json |> member "ocaml_hash" |> to_int in
        let result = Hashtbl.seeded_hash seed s in
        let msg = Fmt.strf "hash(seed=%d, s=%S)" seed s in
        check_int msg ocaml_hash result)
      (to_list v);
    close_in ic
end

module Nodes = struct
  let to_json ppf inodes = Spec.pps ppf (List.map Spec.of_t inodes)

  let generate_file config gen f =
    mkdir config.dir;
    let path = file config.dir config.prefix f in
    let msg =
      match f with Nodes -> "nodes" | Inodes -> "inodes" | _ -> assert false
    in
    Fmt.pr "Generating %s test cases in `%s'@." msg path;
    let oc = open_out path in
    let ppf = Format.formatter_of_out_channel oc in
    Gen.init config.seed;
    let l = Gen.fixed_list config.n gen () in
    Fmt.pf ppf "%a\n%!" to_json l;
    close_out oc

  let generate config =
    generate_file config Gen.short_inode Nodes;
    generate_file config Gen.long_inode Inodes

  let check_file dir f =
    let path = file dir None f in
    let msg =
      match f with Nodes -> "nodes" | Inodes -> "inodes" | _ -> assert false
    in
    Fmt.pr "Checking %s test cases in `%s'@." msg path;
    let ic = open_in path in
    let v = Yojson.Safe.from_channel ic |> Yojson.Safe.to_basic in
    let open Yojson.Basic.Util in
    List.iter
      (fun json ->
        let hash = json |> member "hash" |> to_string |> Spec.hash_of_string in
        let bindings =
          json
          |> member "bindings"
          |> to_list
          |> List.map (fun json ->
                 let name = json |> member "name" |> to_string in
                 let kind =
                   json |> member "kind" |> to_string |> Spec.kind_of_string
                 in
                 let hash =
                   json |> member "hash" |> to_string |> Spec.hash_of_string
                 in
                 { Spec.name; kind; hash })
        in
        let vs = List.map Spec.value bindings in
        let n = Store.Private.Node.Val.v vs in
        let h = Spec.hash_node n in
        let msg = Fmt.strf "%a\n" Fmt.Dump.(list Spec.pp_entry) bindings in
        check_hash msg hash h)
      (to_list v);
    close_in ic

  let check dir =
    Printexc.record_backtrace true;
    check_file dir Nodes;
    check_file dir Inodes
end

let generate config =
  Printexc.record_backtrace true;
  OCaml_hashes.generate config;
  Nodes.generate config

let check dir =
  Printexc.record_backtrace true;
  OCaml_hashes.check dir;
  Nodes.check dir

open Cmdliner

let directory =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s then `Ok s
        else `Error (Fmt.str "`%s' is not a directory" s)
    | false -> `Ok s
  in
  (parse, Fmt.string)

let seed =
  let doc = "Seed used to generate random data." in
  Arg.(value & opt int 0 & info [ "s"; "seed" ] ~doc)

let prefix =
  let doc = "Optional string to prefix to generated files ." in
  Arg.(value & opt (some string) None & info [ "prefix" ] ~doc)

let number =
  let doc = "Number of data points to generate for each test." in
  Arg.(value & opt int 100 & info [ "n"; "number" ] ~doc)

let dir =
  let doc = "The directory where the files will be written." in
  Arg.(value & opt directory "data" & info [ "d"; "directory" ] ~doc)

let config =
  let f prefix n dir seed = { prefix; n; dir; seed } in
  Term.(pure f $ prefix $ number $ dir $ seed)

let gen =
  let doc = "Tezos context hash test cases generation" in
  Term.(const generate $ config, info "gen" ~doc)

let check =
  let doc = "Tezos context hash test cases checks" in
  Term.(const check $ dir, info "check" ~doc)

let () = Term.(exit @@ eval_choice gen [ gen; check ])
