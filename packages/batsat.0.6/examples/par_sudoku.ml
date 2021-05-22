
(** {1 simple sudoku solver} *)

module Fmt = CCFormat
module B = Batsat

let spf = Printf.sprintf
let errorf msg = CCFormat.kasprintf failwith msg

type cell = int

module Grid : sig
  type t = cell array

  (** A set of related cells *)
  type set = (int*int*cell) Iter.t
  val rows : t -> set Iter.t
  val cols : t -> set Iter.t
  val squares : t -> set Iter.t
  val all_cells : t -> set

  val get : t -> int -> int -> cell
  val set : t -> int -> int -> cell -> unit

  val empty : unit -> t
  val parse : string -> t
  val pp : t Fmt.printer
end = struct
  type t = cell array

  let[@inline] get (s:t) i j = s.(i*9 + j)
  let[@inline] set (s:t) i j n : unit = s.(i*9 + j) <- n

  (** A set of related cells *)
  type set = (int*int*cell) Iter.t

  open Iter.Infix

  let all_cells (g:t) =
    0 -- 8 >>= fun i ->
    0 -- 8 >|= fun j -> (i,j,get g i j)

  let rows (g:t) =
    0 -- 8 >|= fun i ->
    ( 0 -- 8 >|= fun j -> (i,j,get g i j))

  let cols g =
    0 -- 8 >|= fun j ->
    ( 0 -- 8 >|= fun i -> (i,j,get g i j))

  let squares g =
    0 -- 2 >>= fun sq_i ->
    0 -- 2 >|= fun sq_j ->
    ( 0 -- 2 >>= fun off_i ->
      0 -- 2 >|= fun off_j ->
      let i = 3*sq_i + off_i in
      let j = 3*sq_j + off_j in
      (i,j,get g i j))

  let is_full g = Array.for_all (fun c->c>0) g

  let is_valid g =
    let all_distinct (s:set) =
      (s >|= fun (_,_,c) -> c)
      |> Iter.diagonal
      |> Iter.for_all (fun (c1,c2) -> c1 <> c2)
    in
    Iter.for_all all_distinct @@ rows g &&
    Iter.for_all all_distinct @@ cols g &&
    Iter.for_all all_distinct @@ squares g

  let matches ~pat:g1 g2 : bool =
    all_cells g1
    |> Iter.filter (fun (_,_,c) -> c>0)
    |> Iter.for_all (fun (x,y,c) -> c = get g2 x y)

  let pp out g =
    Fmt.fprintf out "@[<v>";
    Array.iteri
      (fun i n ->
         if n=0 then Fmt.string out "-" else Fmt.int out n;
         if i mod 9 = 8 then Fmt.fprintf out "@,")
      g;
    Fmt.fprintf out "@]"

  let empty () : t = Array.make 81 0

  let parse (s:string) : t =
    if String.length s < 81 then (
      errorf "line is too short, expected 81 chars, not %d" (String.length s);
    );
    let a = Array.make 81 0 in
    for i = 0 to 80 do
      let c = String.get s i in
      let n = if c = '.' then 0 else Char.code c - Char.code '0' in
      if n < 0 || n > 9 then errorf "invalid char %c" c;
      a.(i) <- n
    done;
    a
end

module Solver : sig
  type t
  val create : Grid.t -> t
  val solve : t -> Grid.t option
  val solve_grid : Grid.t -> Grid.t option
end = struct
  let (!!) = B.Lit.neg
  type var = (int*int*cell) (* x,y := c *)

  type t = {
    grid0: Grid.t;
    solver: B.t;
    vars: (var, B.Lit.t) Hashtbl.t;
    mutable new_lit: int;
  }

  let create grid0 : t =
    { grid0; solver=B.create();
      new_lit=1;
      vars=Hashtbl.create 128; }

  let var_ (self:t) (v:var): B.Lit.t =
    match Hashtbl.find_opt self.vars v with
    | Some l -> l
    | None ->
      let lit = B.Lit.make self.new_lit in
      self.new_lit <- 1 + self.new_lit;
      Hashtbl.add self.vars v lit;
      lit

  let encode_ self : unit =
    Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"sudoku.encode" () @@ fun _sp ->

    begin
      (* all cells are full *)
      Grid.all_cells self.grid0
        (fun (x,y,c) ->
           if c=0 then (
             let c = CCList.init 9 (fun c -> var_ self (x,y,c+1)) in
             B.add_clause_l self.solver c;
           ) else (
             let c = [var_ self (x,y,c)] in (* fixed cell *)
             B.add_clause_l self.solver c;
           ));
    end;

    (* enforce "all diff" constraints *)
    let all_diff (f:_ -> Grid.set Iter.t) =
      let pairs =
        f self.grid0
        |> Iter.flat_map Iter.diagonal
      in
      pairs
        (fun ((x1,y1,c1),(x2,y2,c2)) ->
           if c1 = c2 then (
             assert (x1<>x2 || y1<>y2);
             let c = [!! (var_ self (x1,y1,c1)); !! (var_ self (x2,y2,c2))] in
             B.add_clause_l self.solver c;
           ));
    in
    begin
      all_diff Grid.rows;
      all_diff Grid.cols;
      all_diff Grid.squares;
    end;
    ()

  let decode_ self : Grid.t =
    let grid = Grid.empty() in
    Hashtbl.iter
      (fun (x,y,c) var ->
         if B.value self.solver var = B.V_true then (
           Grid.set grid x y c
         ))
      self.vars;
    grid

  let solve self =
    try
      encode_ self;
      begin
        Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"sudoku.solve" () @@ fun _sp ->
        B.solve self.solver;
      end;
      Some (decode_ self)
    with B.Unsat -> None

  let solve_grid (g:Grid.t) : Grid.t option =
    let s = create g in
    solve s
end

type task =
  | T_solve of Grid.t
  | T_exit

let solve_files ~j files : unit =
  let start = Sys.time() in
  let parse_file file =
    Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"parse.grids" () @@ fun _sp ->
    Format.printf "parse grids from file %S@." file;
    CCIO.with_in file CCIO.read_lines_l
    |> CCList.filter_map
      (fun s ->
         let s = String.trim s in
         if s="" then None
         else match Grid.parse s with
           | g -> Some g
           | exception e ->
             errorf "cannot parse sudoku %S: %s@." s (Printexc.to_string e))
  in
  let grids = CCList.flat_map parse_file files in

  Tracy.message_f (fun k->k "n-grid: %d" (List.length grids));
  Format.printf "parsed %d grids (in %.3fs)@." (List.length grids) (Sys.time()-.start);

  let module Q = CCBlockingQueue in
  let tasks = Q.create 16 in
  let worker i =
    let rec loop() : unit =
      match Q.take tasks with
      | T_exit -> Tracy.message "exit"; ()
      | T_solve g ->
        begin match Solver.solve_grid g with
          | None -> Fmt.printf "unsat grid@."
          | Some _g' -> Fmt.printf "sat grid@."
        end;
        loop()
    in
    Tracy.name_thread (spf "worker %d" i);
    loop()
  in

  (* start workers *)
  let pool = Array.init j (fun _i -> Thread.create worker _i) in

  begin
    let t_list = CCList.map (fun g -> T_solve g) grids in
    Q.push_list tasks t_list
  end;

  begin
    Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"cleanup" () @@ fun _ ->
    for _=0 to j-1 do Q.push tasks T_exit done;
    Array.iter Thread.join pool;
  end;
  Format.printf "@.solved %d grids@." (List.length grids);
  ()

let () =
  Fmt.set_color_default true;
  let files = ref [] in
  let debug = ref 0 in
  let j = ref 4 in
  let opts = [
    "--debug", Arg.Set_int debug, " debug";
    "-d", Arg.Set_int debug, " debug";
    "-j", Arg.Set_int j, " number of jobs";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "sudoku_solve [options] <file>";

  Tracy.enable();
  try
    solve_files ~j:!j !files;
  with
  | Failure msg | Invalid_argument msg ->
    Format.printf "@{<Red>Error@}:@.%s@." msg;
    exit 1
