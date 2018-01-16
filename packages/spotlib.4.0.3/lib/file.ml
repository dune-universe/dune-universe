open Base
open Exn

(* It is very stupid but camlp4 of 4.00.1 has a bug of
   converting `True to ` True and `False to ` False.
   We have to use `TRUE or `true instead.
*)

let iter_lines_exn fname f =
  let ic = open_in fname in
  ~~ protect () 
    ~f:(fun () ->
      let rec iter () = f (input_line ic); iter () in
      try iter () with Exit | End_of_file -> ())
    ~finally:(fun _ -> close_in ic)
;;

let iter_lines fname f =
  ~~ catch () ~f:(fun () -> iter_lines_exn fname f)
;;

let to_lines fname =
  let rev_lines = ref [] in
  match iter_lines fname (fun x -> rev_lines := x :: !rev_lines) with
  | Error e -> Error e
  | Ok () -> Ok (List.rev !rev_lines)
;;

let to_string fname =
  let buf = Buffer.create 0 in
  let ic = open_in_bin fname in
  let s = Bytes.create 10000 in
  let rec loop () =
    let bytes = input ic s 0 10000 in
    if bytes = 0 then Buffer.contents buf
    else begin
      Buffer.add_subbytes buf s 0 bytes;
      loop ()
    end;
  in
  catch_ loop

let open_out string f =
  let oc = open_out string in
  protect f oc ~finally:(fun _ -> close_out oc)
;;

let write_lines p lines = 
  open_out p (fun oc ->
    List.iter (fun l -> 
      output_string oc l; output_char oc '\n') lines)

module Test = struct
  open Unix
  
  type test_unary_result = ([ `TRUE of stats | `FALSE of stats ], Unix.error) result
  
  type test_unary = string -> test_unary_result
  
  let gen_uop ~stat ~f file =
    try 
      let st = stat file in
      Ok (if f st then `TRUE st else `FALSE st)
    with
    | Unix_error (error, _, _) -> Error error
  ;;
  
  let uop = gen_uop ~stat:stat
  let luop = gen_uop ~stat:lstat

  let simplify f name =
    match f name with
    | Ok (`TRUE _) -> true
    | _ -> false
  ;;

  (*     FILE1 -ef FILE2
                FILE1 and FILE2 have the same device and inode numbers
  
         FILE1 -nt FILE2
                FILE1 is newer (modification date) than FILE2
  
         FILE1 -ot FILE2
                FILE1 is older than FILE2
  
  val _b : test_unary (* FILE exists and is block special*)
  val _c : test_unary (* FILE exists and is character special*)
  val _d : test_unary (* FILE exists and is a directory*)
  *)
  let _d' = uop &~ fun stat -> stat.st_kind = S_DIR
  let _d = simplify _d'
  ;;
  
  (*
  val _e : (* FILE exists*)
  *)
  let _e' file = 
    try let stat = stat file in Ok (`TRUE stat) with 
    | Unix_error (error, _, _) -> 
	match error with
        | ENOENT -> Ok `FALSE
        | e -> Error e
  ;;
  let _e = simplify _e'

  (*
  val _f : test_unary (* FILE exists and is a regular file*)
  *)
  let _f' = uop &~ fun stat -> stat.st_kind = S_REG
  let _f = simplify _f'

  (*
  val _g : test_unary (* FILE exists and is set-group-ID*)
  val _G : test_unary (* FILE exists and is owned by the effective group ID*)
  val _h : test_unary (* FILE exists and is a symbolic link (same as -L)*)
  *)
  let _h' = luop &~ fun stat -> stat.st_kind = S_LNK
  let _h = simplify _h'

  (*
  val _k : test_unary (* FILE exists and has its sticky bit set*)
  val _L : test_unary (* FILE exists and is a symbolic link (same as -h)*)
  *)
  let _L' = _h'
  let _L = simplify _L'

  (*
  val _O : test_unary (* FILE exists and is owned by the effective user ID*)
  val _p : test_unary (* FILE exists and is a named pipe*)
  val _r : test_unary (* FILE exists and read permission is granted*)
  val _s : test_unary (* FILE exists and has a size greater than zero*)
  *)
  let _s' = uop &~ fun stat -> stat.st_size > 0
  let _s = simplify _s'

  (*
  val _S : test_unary (* FILE exists and is a socket*)
  
         -t FD  file descriptor FD is opened on a terminal
  
  val _u : test_unary (* FILE exists and its set-user-ID bit is set*)
  val _w : test_unary (* FILE exists and write permission is granted*)
  val _x : test_unary (* FILE exists and execute (or search) permission is granted*)
  *)
end

let get_inode p = try Some (Unix.stat p).st_ino with _ -> None

(* Seems working in MinGW, too *)
let equal p1 p2 =
  p1 = p2
  || let i1 = get_inode p1 in
     i1 <> None && i1 = get_inode p2

let contains p1 =
  let i1 = get_inode p1 in
  fun p2 ->
    if p1 = p2 then Some []
    else
      let rec f st p2 =
        if p1 = p2 then Some st
        else
          let i2 = get_inode p2 in
          if i1 <> None && i1 = i2 then Some st
          else
            let d = Filename.dirname p2 in
            if d = p2 then None
            else f (Filename.basename p2 :: st) d
      in
      f [] p2
