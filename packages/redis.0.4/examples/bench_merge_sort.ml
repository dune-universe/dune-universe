module C = Redis_sync.Client

type t = {
  c: C.connection;
  l: int list;
  n: int;
}

(* generate a random list, but always the same *)
let mk_list n : int list =
  let st = Random.State.make [| 42 |] in
  CCList.init n (fun _ -> Random.State.int st 5_000)

(* make a fresh index *)
let mk_id (self:t) (pre:string) : string =
  let i = C.incr self.c "bms:cur_id" in
  Printf.sprintf "bms:id:%s:%d" pre i

let ignore_int (_:int) = ()

let str_of_list (self:t) (id:string) : string =
  Printf.sprintf "[%s]" (String.concat ","@@ C.lrange self.c id 0 self.n)

let run (self:t) : unit =
  let c = self.c in
  let id_list = mk_id self "list" in
  (* insert the whole list *)
  let _n = C.rpush c id_list (List.rev_map string_of_int self.l) in
  assert (_n = self.n);
  Printf.printf "initial: %s\n%!" (str_of_list self id_list);
  (* merge [id1] and [id2] into [into] *)
  let merge (id1:string) (id2:string) ~into : unit =
    (*Printf.printf "merge %s=%s and %s=%s into %s=%s\n%!"
      id1 (str_of_list self id1)
      id2 (str_of_list self id2) into (str_of_list self into); *)
    let rec loop () : unit =
      let len1 = C.llen c id1 in
      let len2 = C.llen c id2 in
      (* Printf.printf "  len1=%d, len2=%d\n%!" len1 len2; *)
      if len1=0 && len2=0 then ()
      else if len1=0 then (
        C.rpush c into (C.lrange c id2 0 len2) |> ignore_int;
      ) else if len2=0 then (
        C.rpush c into (C.lrange c id1 0 len1) |> ignore_int;
      ) else (
        let x = C.lpop c id1 |> CCOpt.get_exn |> int_of_string in
        let y = C.lpop c id2 |> CCOpt.get_exn |> int_of_string in
        (* Printf.printf "  x=%d, y=%d\n%!" x y; *)
        if x<y then (
          C.lpush c id2 [string_of_int y] |> ignore_int;
          C.rpush c into [string_of_int x] |> ignore_int;
          loop ();
        ) else (
          C.lpush c id1 [string_of_int x] |> ignore_int;
          C.rpush c into [string_of_int y] |> ignore_int;
          loop ();
        )
      )
    in
    loop ();
    (* Printf.printf "  -> %s\n%!" (str_of_list self into); *)
  in
  (* now recursively do merge sort *)
  let rec sort (id_list:string) : unit =
    let len = C.llen c id_list in
    if len >= 2 then (
      let mid = len/2 in
      let l1 = mk_id self "list_tmp" in
      let l2 = mk_id self "list_tmp" in
      C.rpush c l1 (C.lrange c id_list 0 (mid-1)) |> ignore_int;
      C.rpush c l2 (C.lrange c id_list mid len)  |> ignore_int;
      assert (C.llen c l1 + C.llen c l2 = len);
      C.del c [id_list] |> ignore_int;
      sort l1;
      sort l2;
      merge l1 l2 ~into:id_list;
      C.del self.c [l1; l2] |> ignore_int; (* collect tmp clauses *)
    )
  in
  sort id_list;
  Printf.printf "result: %s\n%!" (str_of_list self id_list);
  let l = C.lrange c id_list 0 self.n |> List.map int_of_string in
  C.del self.c [id_list] |> ignore_int;
  C.del self.c ["bms:cur_id"] |> ignore_int;
  (* must be sorted *)
  assert ( CCList.is_sorted ~cmp:CCInt.compare l);
  (* same length *)
  assert (List.length l = List.length self.l);
  (* same elements *)
  assert (
    let module IS = CCSet.Make(CCInt) in
    IS.equal (IS.of_list l) (IS.of_list self.l));
  ()

let run ?(n=100_000) host port : unit =
  let c = C.connect {C.host; port} in
  let st = {n; c; l=mk_list n} in
  let start = Unix.gettimeofday () in
  run st;
  let stop = Unix.gettimeofday () in
  Printf.printf "time: %.3fs\n%!" (stop -. start);
  ()

