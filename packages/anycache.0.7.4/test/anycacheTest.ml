(******************************************************************************)
(* Copyright (c) 2014-2016 Skylable Ltd. <info-copyright@skylable.com>        *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)

open Alcotest
module type MonadTest = sig
  include Anycache.Monad
  val run : (unit -> 'a t) -> 'a
end

module Make(Monad: MonadTest) = struct
  module C = Anycache.Make(String)(Monad)

  let key1 = "test"
  let key2 = "test2"
  let val_d = 42L
  let val_p = 43L
  let val_p2 = 44L

  open! Monad

  let compute_data_direct cnt key =
    incr cnt;
    check string "cache key" key1 key;
    return val_d

  let compute_data cnt _ =
    incr cnt;
    return val_p

  let compute_data_p cnt key =
    incr cnt;
    check string "cache key" key2 key;
    return val_p

  let cache = C.create 100
  let lookup cache key f =
    let revalidate = function
    | key, None -> f key
    | _, Some data -> Monad.return data
    in
    C.with_validator cache revalidate key

  (*BISECT-IGNORE-BEGIN*)
  let pp_cache_result ppf = function
  | Ok v -> Fmt.int64 ppf v
  | Error e -> Fmt.exn ppf e
  (*BISECT-IGNORE-END*)

  let cached =
    of_pp pp_cache_result

  let (>>=) v f =
    let open Monad in
    v >>? function
    | Ok r -> f r
    | Error e -> Monad.fail e (*BISECT-IGNORE*)

  let test_cache_bind_seq () =
    let cnt = ref 0 in
    let v1 = Monad.run (fun () -> lookup cache key1 (compute_data_direct cnt)) in
    check int64 "cache value1" val_d v1;
    let v2 = Monad.run (fun () -> lookup cache key1 (compute_data_direct cnt)) in
    check int64 "cache value2" val_d v2;
    check int "number of computations" 1 !cnt

  let test_cache_bind_p () =
    let cnt = ref 0 in
    Monad.run (fun () ->
        lookup cache key2 (compute_data_p cnt) >>? fun v1 ->
        lookup cache key2 (compute_data_p cnt) >>? fun v2 ->
        lookup cache key2 (compute_data_p cnt) >>? fun v3 ->
        check cached "cache value1" (Ok val_p) v1;
        check cached "cache value2" (Ok val_p) v2;
        check cached "cache value3" (Ok val_p) v3;
        check int "number of computations" 1 !cnt;
        return ()
      )

  let get cache key =
    C.with_cache cache (fun _ -> Monad.fail Not_found) key

  let direct_get cache key =
    C.get cache key

  let set cache key value =
    C.with_validator cache (fun _ -> Monad.return value) key 

  let direct_set cache key value =
    C.set cache key value

  let test_cache_size n () =
    let cnt = ref 0 in
    let c = C.create n in
    Monad.run (fun () ->
        let prev = ref (return 0L) in
        for i = 1 to n+1 do
          let key = string_of_int i in
          prev := !prev >>= fun _ -> lookup c key (compute_data cnt)
        done;
        !prev >>= fun _ ->
        check int "computations" (n+1) !cnt;

        get c (string_of_int 2) >>? fun second ->
        check cached "still in cache(2)" (Ok val_p) second;

        direct_get c (string_of_int 2) >>= fun second' ->
        check (option int64) "still in cache(2)" (Some val_p) second';

        get c (string_of_int 1) >>? fun first ->
        check cached "not in cache anymore(1)" (Error Not_found) first;

        get c (string_of_int 2) >>? fun third ->
        check cached "still in cache(2) second" (Ok val_p) third;

        set c (string_of_int 2) val_p2 >>? fun _ ->
        get c (string_of_int 2) >>? fun fourth ->
        check cached "updated value(2)" (Ok val_p2) fourth;

        direct_set c (string_of_int 3) val_p2;
        get c (string_of_int 3) >>? fun fifth ->
        check cached "updated value(3)" (Ok val_p2) fifth;

        prev := return 0L;
        for i = 3 to n do
          let key = string_of_int i in
          prev := !prev >>= fun _ -> lookup c key (compute_data cnt)
        done;
        !prev >>= fun _ ->
        get c (string_of_int 2) >>? fun fourth' ->
        check cached "updated value(2')" (Ok val_p2) fourth';

        prev := return 0L;
        for i = n*2 to n*4 do
          let key = string_of_int i in
          prev := !prev >>= fun _ -> lookup c key (compute_data cnt)
        done;
        !prev >>= fun _ ->
        get c (string_of_int 2) >>? fun fourth'' ->
        check cached "updated value(3')" (Ok val_p2) fourth'';

        prev := return 0L;
        for i = n*4 downto n*2 do
          let key = string_of_int i in
          prev := !prev >>= fun _ -> lookup c key (compute_data cnt)
        done;
        for i = n*2 to n*3 do
          let key = string_of_int i in
          prev := !prev >>= fun _ -> lookup c key (compute_data cnt)
        done;
        for i = n*3 downto n*2 do
          let key = string_of_int i in
          prev := !prev >>= fun _ -> lookup c key (compute_data cnt)
        done;
        !prev >>= fun _ ->
        get c (string_of_int 2) >>? fun final ->
        check cached "gone from main" (Error Not_found) final;

        return ()
      )

  let test_nocache_err () =
    let cnt = ref 0 in
    let c = C.create 100 in
    Monad.run (fun () ->
        let r1 = lookup c "foo" (fun _ -> Monad.fail Exit) in
        let r2 = lookup c "foo" (compute_data cnt) in
        r1 >>? fun v1 ->
        r2 >>? fun v2 ->
        lookup c "foo" (compute_data cnt) >>? fun v3 ->
        check cached "error is propagated" v1 (Error Exit);
        check cached "recomputed" v2 (Ok val_p);
        check cached "cached" v3 (Ok val_p);
        check int "number of computations" 1 !cnt;
        return ()
      )

  let tests =
    [
      "Cache.bind", [
        "seq", `Quick, test_cache_bind_seq;
        "parallel", `Quick, test_cache_bind_p;
      ];
      "Cache size", Array.to_list (Array.init 8 (fun i ->
          let i = i + 3 in
          string_of_int i, `Quick, test_cache_size i
        ));
      "Error semantics", [
        "not cached", `Quick, test_nocache_err
      ]
    ]
end

module MakePending(Monad:sig
    include MonadTest
    val delay : float -> unit t
  end) = struct

  module PL = Anycache.PendingLimit.Make(String)(Monad)

  open! Monad

  let (>>=) v f =
    v >>? function
    | Ok r -> f r
    | Error e -> Monad.fail e(*BISECT-IGNORE*)

  let compute cnt x =
    incr cnt;
    return (x ^ "1")

  let test_no_overlap () =
    let pending = PL.create () in
    Monad.run (fun () ->
        let cnt = ref 0 in
        let a1m = PL.bind pending "a" (compute cnt) in
        let b1m = PL.bind pending "b" (compute cnt) in
        a1m >>= fun a1 ->
        b1m >>= fun b1 ->
        check string "1" "a1" a1;
        check string "2" "b1" b1;
        check int "computation count" 2 !cnt;
        return ()
      )

  let compute_delay cnt x =
    compute cnt x >>= fun r ->
    Monad.delay 0.1 >>= fun () ->
    return r

  let compute_fail cnt x =
    compute cnt x >>= fun _ ->
    Monad.delay 0.1 >>= fun () ->
    fail Exit

  let test_overlap () =
    let pending = PL.create () in
    Monad.run (fun () ->
        let cnt = ref 0 in
        (* only computes a twice because it detects that computation of a is in progress *)
        let a1m = PL.bind pending "a" (compute_delay cnt) in
        let a1m' = PL.bind pending "a" (compute_delay cnt) in
        let b1m = PL.bind pending "b" (compute_delay cnt) in
        a1m >>= fun a1 ->
        a1m' >>= fun a1' ->
        b1m >>= fun b1 ->
        check string "1" "a1" a1;
        check string "1'" "a1" a1';
        check string "2" "b1" b1;
        check int "computation count" 2 !cnt;
        return ()
      )

  (*BISECT-IGNORE-BEGIN*)
  let pp_cache_result ppf = function
  | Ok v -> Fmt.string ppf v
  | Error e -> Fmt.exn ppf e
  (*BISECT-IGNORE-END*)

  let cached =
    of_pp pp_cache_result

  let test_failed () =
    let pending = PL.create () in
    Monad.run (fun () ->
        let cnt = ref 0 in
        (* only computes a twice because it detects that computation of a is in progress *)
        let a1m = PL.bind pending "a" (compute_fail cnt) in
        let a1m' = PL.bind pending "a" (compute_delay cnt) in
        let b1m = PL.bind pending "b" (compute_delay cnt) in
        a1m >>? fun a1 ->
        a1m' >>? fun a1' ->
        let a1m'' = PL.bind pending "a" (compute_delay cnt) in
        a1m'' >>? fun a1'' ->
        b1m >>= fun b1 ->
        check cached "1" (Error Exit) a1;
        check cached "1'" (Error Exit) a1';
        check cached "1''" (Ok "a1") a1'';
        check string "2" "b1" b1;
        check int "computation count" 3 !cnt;
        return ()
      )

  let tests = [
    "Anycache_pendinglimit", [
      "no overlap", `Quick, test_no_overlap;
      "overlap", `Slow, test_overlap;
      "failed", `Slow, test_failed;
    ]
  ]
end
