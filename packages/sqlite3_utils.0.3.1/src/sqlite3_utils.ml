
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Utils for SQLite} *)

module Data = Sqlite3.Data
module Rc = Sqlite3.Rc
type db = Sqlite3.db

exception RcError of Sqlite3.Rc.t
exception Type_error of Data.t

let () = Printexc.register_printer
    (function
      | RcError rc -> Some ("sqlite error: " ^ Sqlite3.Rc.to_string rc)
      | _ -> None)

type t = db

let check_ret_exn = function
  | Sqlite3.Rc.DONE
  | Sqlite3.Rc.OK -> ()
  | rc -> raise (RcError rc)

let check_ret x = function
  | Sqlite3.Rc.DONE | Sqlite3.Rc.OK -> Ok x
  | rc -> Error rc

let err_string = function
  | Ok x -> Ok x
  | Error rc -> Error (Rc.to_string rc)

(* on "busy", wait 300ms before failing *)
let setup_timeout ?(ms=300) db : unit =
  Sqlite3.busy_timeout db ms

let bind_ stmt i d : unit = check_ret_exn (Sqlite3.bind stmt i d)

(** Parameters passed to a statement *)
module Ty = struct
  type _ arg =
    | Int : int arg
    | Int64 : int64 arg
    | Float : float arg
    | String : [`Blob|`Text|`Both] -> string arg
    | Data : Data.t arg
    | Nullable : 'a arg -> 'a option arg

  type (_,_) t =
    | Nil : ('res, 'res) t
    | Cons : 'a arg * ('b, 'res) t -> ('a -> 'b, 'res) t
    (* TODO: add this (with distinct count for args and row)
       | Raw : (Data.t array, 'res) t
    *)

  let nil = Nil
  let int = Int
  let int64 = Int64
  let float = Float
  let text = String `Text
  let blob = String `Blob
  let any_str = String `Both
  let data = Data

  let nullable
    : type a. a arg -> a option arg
    = function
      | Nullable _ -> invalid_arg "Sqlite3_utils.Ty.nullable can't be nested"
      | x -> Nullable x

  let (@>) x y = Cons (x,y)
  let p1 x = Cons (x,Nil)
  let p2 x y = Cons (x,Cons (y,Nil))
  let p3 x y z = Cons (x,Cons (y,Cons (z,Nil)))
  let p4 x y z w = Cons (x,Cons (y,Cons (z,Cons (w,Nil))))
  let p5 x y z w1 w2 = Cons (x,Cons (y,Cons (z,Cons (w1, Cons (w2, Nil)))))
  let p6 x y z w1 w2 w3 = Cons (x,Cons (y,Cons (z,Cons (w1, Cons (w2, Cons (w3, Nil))))))

  let rec (@>>)
    : type a b res. (a, b) t -> (b, res) t -> (a, res) t =
    fun ty1 ty2 ->
    match ty1 with
      | Nil -> ty2
      | Cons (x, ty1') -> Cons (x, ty1' @>> ty2)

  let id x = x
  let mkp2 x y = x,y
  let mkp3 x y z = x,y,z
  let mkp4 x y z w = x,y,z,w
  let mkp5 x y z w1 w2 = x,y,z,w1,w2
  let mkp6 x y z w1 w2 w3 = x,y,z,w1,w2,w3

  let rec count : type a r. (a, r) t -> int
    = function
    | Nil -> 0
    | Cons (_, tl) -> 1 + count tl

  (* translate parameters *)
  let rec tr_args
    : type a res. Sqlite3.stmt -> int -> (a,res) t -> (unit->res) -> a
    = fun stmt i p cb ->
      match p with
      | Nil -> cb()
      | Cons(Int, k) ->
        (fun x -> bind_ stmt i (Data.INT (Int64.of_int x)); tr_args stmt (i+1) k cb)
      | Cons (Int64, k) ->
        (fun x -> bind_ stmt i (Data.INT x); tr_args stmt (i+1) k cb)
      | Cons (String (`Text|`Both),k) ->
        (fun x -> bind_ stmt i (Data.TEXT x); tr_args stmt (i+1) k cb)
      | Cons (String `Blob,k) ->
        (fun x -> bind_ stmt i (Data.BLOB x); tr_args stmt (i+1) k cb)
      | Cons (Float, k) ->
        (fun x -> bind_ stmt i (Data.FLOAT x); tr_args stmt (i+1) k cb)
      | Cons (Data, k) ->
        (fun x -> bind_ stmt i x; tr_args stmt (i+1) k cb)
      | Cons (Nullable p1, k) ->
        (function
          | None -> bind_ stmt i Data.NULL; tr_args stmt (i+1) k cb
          | Some x -> tr_args stmt i (p1 @> k) cb x
        )

  (* translate results *)
  let rec tr_row
    : type a res. (int->Data.t) -> int -> (a,res) t -> a -> res
    = fun get i ty f -> match ty with
      | Nil -> f
      | Cons (ty, k) ->
        let data = get i in
        tr_row_data get data i ty k f
  and tr_row_data
    : type a b res. (int->Data.t) -> Data.t -> int -> a arg -> (b,res) t -> (a->b) -> res
    = fun get data i ty k f ->
      match ty with
      | Data -> tr_row get (i+1) k (f data)
      | Int64 ->
        (match data with
         | Data.INT x -> tr_row get (i+1) k (f x)
         | d -> raise (Type_error d))
      | Int ->
        (match data with
         | Data.INT x as d -> 
           let x = try Int64.to_int x with _ -> raise (Type_error d) in
           tr_row get (i+1) k (f x)
         | d -> raise (Type_error d))
      | Float ->
        (match data with
         | Data.FLOAT x -> tr_row get (i+1) k (f x)
         | d -> raise (Type_error d))
      | String kind ->
        (match data, kind with
         | Data.BLOB x, `Blob -> tr_row get (i+1) k (f x)
         | Data.TEXT x, `Text -> tr_row get (i+1) k (f x)
         | (Data.BLOB x | Data.TEXT x), `Both -> tr_row get (i+1) k (f x)
         | d, _ -> raise (Type_error d))
      | Nullable ty' ->
        (match data with
         | Data.NONE | Data.NULL -> tr_row get (i+1) k (f None)
         | _ -> tr_row_data get data i ty' k (fun x -> f (Some x)))
end

module Cursor = struct
  type 'a t = {
    stmt: Sqlite3.stmt;
    read: Sqlite3.stmt -> 'a;
    mutable cur: 'a option;
  }

  let ignore _ = ()

  let next_ self : unit =
    match Sqlite3.step self.stmt with
    | Sqlite3.Rc.DONE ->
      self.cur <- None;
    | Sqlite3.Rc.ROW ->
      let x = self.read self.stmt in
      self.cur <- Some x
    | rc -> raise (RcError rc)

  let make_ stmt read =
    let self = { stmt; cur=None; read; } in
    next_ self;
    self

  let opt_map_ f = function None -> None | Some x -> Some (f x)

  let get_one c = match c.cur with
    | None -> Error Rc.NOTFOUND
    | Some x -> Ok x

  let get_one_exn c = match get_one c with
    | Ok x -> x
    | Error rc -> raise (RcError rc)

  let map ~f c : _ t =
    {stmt=c.stmt;
     read=(fun stmt -> f (c.read stmt));
     cur=opt_map_ f c.cur;
    }

  let make stmt ty f =
    let read stmt = Ty.tr_row (Sqlite3.column stmt) 0 ty f in
    make_ stmt read

  let make_raw stmt : Data.t array t =
    make_ stmt Sqlite3.row_data

  (* next value in the cursor *)
  let next self : _ option =
    match self.cur with
    | None -> None
    | Some _ as x ->
      next_ self;
      x

  let rec iter ~f self = match self.cur with
    | None -> ()
    | Some res ->
      f res;
      next_ self;
      iter ~f self

  let to_seq self =
    let rec get_next () =
      let n = lazy (
        match self.cur with
        | None -> Seq.Nil
        | Some x ->
          next_ self;
          let tl = get_next () in
          Seq.Cons (x, tl)
      ) in
      fun () -> Lazy.force n
    in
    get_next ()

  (* convert a cursor into a list of answers *)
  let to_list_rev (c:'a t) : 'a list =
    let rec aux acc c = match next c with
      | None -> acc
      | Some d -> aux (d::acc) c
    in
    aux [] c

  let to_list c = List.rev (to_list_rev c)
end

let finally_ ~hok ~herr x f =
  try
    let res = f x in
    hok x;
    res
  with e ->
    herr x;
    raise e

let finalize_check_ stmt = check_ret_exn @@ Sqlite3.finalize stmt
let finalize_nocheck_ stmt =
  try ignore (Sqlite3.finalize stmt : Rc.t) with Sqlite3.Error _ -> ()

let db_close_rec_ db =
  while not (Sqlite3.db_close db) do () done

let with_db ?mode ?mutex ?cache ?vfs ?timeout str f =
  let db = Sqlite3.db_open ?mode ?mutex ?cache ?vfs str in
  (match timeout with Some ms -> setup_timeout db ~ms | None -> ());
  finally_ ~hok:db_close_rec_ ~herr:db_close_rec_ db f

let with_stmt db str ~f =
  let stmt = Sqlite3.prepare db str in
  finally_ ~hok:finalize_check_ ~herr:finalize_nocheck_ stmt f

let check_arity_params_ stmt n : unit =
  if Sqlite3.bind_parameter_count stmt <> n then (
    invalid_arg
      (Format.sprintf "wrong number of parameters: expected %d, got %d"
         (Sqlite3.bind_parameter_count stmt) n);
  )

let check_arity_res_ stmt n : unit =
  if Sqlite3.column_count stmt <> n then (
    invalid_arg
      (Format.sprintf "wrong number of columns in result: expected %d, got %d"
         (Sqlite3.column_count stmt) n);
  )

let exec0_exn db str : unit = check_ret_exn @@ Sqlite3.exec db str
let exec0 db str : _ result = check_ret () @@ Sqlite3.exec db str

(* execute statement, return cursor *)
let exec_raw_exn db str ~f =
  with_stmt db str
    ~f:(fun stmt ->
        check_arity_params_ stmt 0;
        f (Cursor.make_raw stmt))

let exec_raw db str ~f =
  try Ok (exec_raw_exn db str ~f)
  with RcError c -> Error c

(* execute statement parametrized by the array of arguments *)
let exec_raw_args_exn db str a ~f =
  with_stmt db str
    ~f:(fun stmt ->
        check_arity_params_ stmt (Array.length a);
        Array.iteri (fun i x -> check_ret_exn (Sqlite3.bind stmt (i+1) x)) a;
        f (Cursor.make_raw stmt))

let exec_raw_args db str a ~f =
  try Ok (exec_raw_args_exn db str a ~f)
  with RcError c -> Error c

(* execute statement parametrized by the array of arguments *)
let exec_exn db str ~ty ~f =
  let params, ty_r, f_r = ty in
  let stmt = Sqlite3.prepare db str in
  check_arity_params_ stmt (Ty.count params);
  (* caution, bind starts at [1] *)
  Ty.tr_args stmt 1 params
    (fun () ->
       finally_ ~hok:finalize_check_ ~herr:finalize_nocheck_ stmt
         (fun stmt ->
            check_arity_res_ stmt (Ty.count ty_r);
            f (Cursor.make stmt ty_r f_r)))

let exec db str ~ty ~f =
  exec_exn db str ~ty
    ~f:(fun c -> try (Ok (f c)) with RcError rc -> Error rc)

let exec_no_params_exn db str ~ty ~f =
  with_stmt db str
    ~f:(fun stmt ->
        check_arity_params_ stmt 0;
        let ty_r, f_r = ty in
        f (Cursor.make stmt ty_r f_r))

let exec_no_params db str ~ty ~f =
  exec_no_params_exn db str ~ty
    ~f:(fun c -> try (Ok (f c)) with RcError rc -> Error rc)

let exec_no_cursor_ db str ~ty ~check =
  let stmt = Sqlite3.prepare db str in
  check_arity_params_ stmt (Ty.count ty);
  (* caution, bind starts at [1] *)
  Ty.tr_args stmt 1 ty
    (fun () ->
       finally_ ~hok:finalize_check_ ~herr:finalize_nocheck_ stmt
         (fun stmt ->
            check_arity_res_ stmt 0;
            (* just execute one step *)
            check @@ Sqlite3.step stmt))

let exec_no_cursor_exn db str ~ty =
  exec_no_cursor_ db str ~ty ~check:check_ret_exn

let exec_no_cursor db str ~ty =
  exec_no_cursor_ db str ~ty ~check:(check_ret ())

(* From [ocaml-sqlite3EZ](https://github.com/mlin/ocaml-sqlite3EZ),
   with some changes. Compatible license (MIT) *)
let transact db f =
  exec0_exn db "BEGIN;";
  try
    let y = f db in
    exec0_exn db "COMMIT;";
    y
  with
  | e ->
    exec0_exn db "ROLLBACK;";
    raise e

(* From [ocaml-sqlite3EZ](https://github.com/mlin/ocaml-sqlite3EZ),
   with some changes. Compatible license (MIT) *)
let atomically db f =
  exec0_exn db "SAVEPOINT a;";
  try
    let y = f db in
    exec0_exn db "RELEASE a;";
    y
  with
  | exn ->
    exec0_exn db "RELEASE a;";
    exec0_exn db "ROLLBACK TO a;";
    raise exn

(*$inject
  let with_test_db f : unit =
    with_db ":memory:" (fun db ->
        exec0_exn db
          "create table person(name text, job text, age int); ";
        exec0_exn db
          "insert into person values
            ('john', 'PHB', 42), ('alice', 'hacker', 20),
            ('bob', 'caller', 20), ('eve', 'spy', 99);";
        exec0_exn db
          "create table friendorfoe(p1 text, p2 text, level int); ";
        exec0_exn db
          "insert into friendorfoe values
            ('john', 'bob', 1), ('bob', 'alice', 1),
            ('bob', 'eve', -2), ('alice', 'eve', -5) ; ";
        f db)
*)

(* update test *)
(*$R
  with_test_db (fun db ->
        exec0_exn db
          "insert or ignore into friendorfoe (p1, p2, level)
            select p2, p1, level from friendorfoe ;";
        let l = exec_exn db ~ty:Ty.(p1 text, p2 text int, mkp2)
          "select p2, level from friendorfoe where p1 = ? order by p2;"
          "eve" ~f:Cursor.to_list
        in
        assert_equal ~printer:Q.Print.(list @@ pair string int) [
          "alice", -5;
          "bob", -2;
        ] l;)
*)


(* basic cursor test *)
(*$R
  with_test_db (fun db ->
        exec_no_params_exn db
          "select name, job from person where age=20 order by name;"
          ~ty:Ty.(p2 text text, mkp2)
          ~f:(fun c ->
              match Cursor.next c with
              | None -> assert_failure "cursor too short"
              | Some (n1,j1) ->
                assert_equal "alice" n1;
                assert_equal "hacker" j1;
                match Cursor.next c with
                | None -> assert_failure "cursor too short"
                | Some (n2,j2) ->
                  assert_equal "bob" n2;
                  assert_equal "caller" j2;
                  match Cursor.next c with
                  | None -> ()
                  | Some _ -> assert_failure "cursor too long"
            ))
*)

(* recursive query test *)
(*$R
  let q = "with recursive fib(a,b,c) as 
    ( values (1,1,1),(2,1,2) UNION select a+1, c, b+c from fib where a<100)
    select a, c from fib where a<= ?;"
  in
  let l_expect =  [
    1,1;
    2,2;
    3,3;
    4,5;
    5,8;
    6,13;
    7,21;
    8,34;
    9,55;
    10,89;
    11,144;
    12,233;
    13,377;
    14,610;
    15,987;
  ] in
  with_db ":memory:" (fun db ->
      let l =
        exec_exn db q ~ty:Ty.(p1 int, p2 int int, (fun x y->x,y))
          15 ~f:Cursor.to_list
      in
      assert_equal ~printer:Q.Print.(list @@ pair int int) l_expect l 
    )
*)
