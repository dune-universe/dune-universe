
module B = Batsat

let spf = Printf.sprintf

module Parse : sig
  type 'a event =
    | Add_clause of 'a array

  type 'a t

  val make : file:string -> (int -> 'a) -> 'a t

  val iter : 'a t -> ('a event -> unit) -> unit
end = struct
  module Vec = CCVector
  module L = Lexer

  type 'a event =
    | Add_clause of 'a array

  type 'a t = {
    mk: int -> 'a;
    vec: 'a Vec.vector;
    lex: Lexing.lexbuf;
  }

  let make ~file mk : _ t =
    let ic = open_in file in
    let lex = Lexing.from_channel ic in
    at_exit (fun () -> close_in_noerr ic);
    {lex; vec=Vec.create(); mk; }

  let read_ints ?first self : _ array =
    Vec.clear self.vec; (* reuse local vec *)
    CCOpt.iter (Vec.push self.vec) first;
    let rec aux() =
      match L.token self.lex with
      | L.I 0 -> Vec.to_array self.vec (* done *)
      | L.I n ->
        let x = self.mk n in
        Vec.push self.vec x;
        aux()
      | L.EOF -> failwith "unexpected end of file"
    in
    aux()

  let rec iter (self:_ t) f : unit =
    match L.token self.lex with
    | L.EOF -> ()
    | L.I 0 ->
      f (Add_clause [| |]); iter self f
    | L.I x ->
      let c = read_ints ~first:(self.mk x) self in
      f (Add_clause c); iter self f
end


(* solve a single file *)
let solve_file ~debug file : unit =
  Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"solve" () @@ fun _sp ->
  Tracy.add_text _sp file;
  let solver = B.create() in
  let parse =
    Parse.make ~file (fun i ->
        let a = abs i in
        B.Lit.make_with_sign (i>0) a
      ) in
  try
    Parse.iter parse
      (function
        | Parse.Add_clause c ->
          if debug then Format.eprintf "add clause %a@." B.pp_clause (Array.to_list c);
          B.add_clause_a solver c);

    B.solve solver;
    Format.printf "%s: sat@." file
  with B.Unsat ->
    Format.printf "%s: unsat@." file

type task =
  | T_solve of string
  | T_exit

let solve_files_par ~debug ~j (files:_ list) : unit =
  let module Q = CCBlockingQueue in

  let tasks = Q.create 16 in

  let worker i =
    let rec loop() : unit =
      match Q.take tasks with
      | T_exit -> Tracy.message "exit"; ()
      | T_solve file ->
        solve_file ~debug file;
        loop()
    in
    Tracy.name_thread (spf "worker %d" i);
    loop()
  in

  (* start workers *)
  let pool = Array.init j (fun _i -> Thread.create worker _i) in

  begin
    let t_list = CCList.map (fun f -> T_solve f) files in
    Q.push_list tasks t_list
  end;

  begin
    Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"cleanup" () @@ fun _ ->
    for _=0 to j-1 do Q.push tasks T_exit done;
    Array.iter Thread.join pool;
  end;

  ()

let () =
  let files = ref [] in
  let debug = ref false in
  let j = ref 4 in
  let opts = [
    "-d", Arg.Set debug, " debug";
    "-j", Arg.Set_int j, " <int> number of tasks";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "solver [options] <file>";

  Tracy.enable();
  Tracy.name_thread "main";
  solve_files_par ~debug:!debug ~j:!j !files;
  ()
