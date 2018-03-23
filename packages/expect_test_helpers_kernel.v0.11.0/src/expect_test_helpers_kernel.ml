open! Core_kernel

include (Expect_test_helpers_kernel_intf
         : (module type of struct include Expect_test_helpers_kernel_intf end
             with module Allocation_limit :=
               Expect_test_helpers_kernel_intf.Allocation_limit
             with module CR := Expect_test_helpers_kernel_intf.CR))

let print_endline = print_endline

let print_string string =
  print_string string;
  Out_channel.(flush stdout);
;;

module Allocation_limit = struct
  include Expect_test_helpers_kernel_intf.Allocation_limit

  let is_ok t ~major_words_allocated ~minor_words_allocated =
    match t with
    | Major_words n -> major_words_allocated <= n
    | Minor_words n -> major_words_allocated = 0 && minor_words_allocated <= n
  ;;

  let show_major_words = function
    | Major_words _ -> true
    | Minor_words _ -> false
  ;;
end

module CR = struct
  include Expect_test_helpers_kernel_intf.CR

  let message t here =
    let cr cr =
      String.concat
        [ "(* ";cr;" require-failed: "
        ; here |> Source_code_position.to_string
        ; ".\n"
        ; "   Do not 'X' this CR; instead make the required property true,\n"
        ; "   which will make the CR disappear.  For more information, see\n"
        ; "   [Expect_test_helpers_kernel.require]. *)" ]
    in
    match t with
    | CR         -> cr "CR"
    | CR_soon    -> cr "CR-soon"
    | CR_someday -> cr "CR-someday"
    | Comment ->
      String.concat
        [ "(* require-failed: "
        ; here |> Source_code_position.to_string
        ; ". *)" ]
  ;;

  let hide_unstable_output = function
    | CR                             -> false
    | CR_soon | CR_someday | Comment -> true
  ;;
end

let hide_positions_in_string =
  let module Re = Re_pcre in
  let expanders = lazy (
    [ (* This first pattern has an alphabetic prefix because we want to match exceptions
         and their file positions without also matching timestamps.  However, [Re_pcre]
         doesn't implement back-references, precluding a simple substitution.  Instead,
         we provide a length of matched data to copy into the output, effectively acting
         like a back-reference in this special case. *)
      "[a-zA-z]:[0-9]+:[0-9]+" , 1, ":LINE:COL"
    ; "line [0-9]+:", 0, "line LINE:"
    ; "line [0-9]+, characters [0-9]+-[0-9]+" , 0, "line LINE, characters C1-C2" ]
    |> List.map ~f:(fun (pattern, prefix_len, expansion) ->
      let rex = Re.regexp pattern in
      fun string ->
        Re.substitute ~rex string ~subst:(fun orig ->
          String.concat [ String.prefix orig prefix_len; expansion])))
  in
  fun string ->
    List.fold (force expanders) ~init:string ~f:(fun ac expander -> expander ac)
;;

let maybe_hide_positions_in_string ?(hide_positions = false) string =
  if hide_positions
  then hide_positions_in_string string
  else string
;;

let sexp_to_string ?hide_positions sexp =
  let string = Sexp_pretty.sexp_to_string sexp in
  maybe_hide_positions_in_string ?hide_positions string
;;

let print_s ?hide_positions sexp =
  print_string (sexp_to_string ?hide_positions sexp);
;;

let on_print_cr = ref print_endline

let print_cr_with_optional_message
      ?(cr             = CR.CR)
      ?(hide_positions = CR.hide_unstable_output cr)
      here
      optional_message
  =
  let cr =
    CR.message cr here
    |> maybe_hide_positions_in_string ~hide_positions
  in
  !on_print_cr (
    match optional_message with
    | None      -> cr
    | Some sexp ->
      String.concat [ cr; "\n"
                    ; String.rstrip (sexp_to_string ~hide_positions sexp) ]);
;;

let print_cr ?cr ?hide_positions here message =
  print_cr_with_optional_message ?cr ?hide_positions here (Some message)
;;

let require ?cr ?hide_positions ?if_false_then_print_s here bool =
  match bool with
  | true  -> ()
  | false ->
    print_cr_with_optional_message ?cr ?hide_positions here
      (Option.map if_false_then_print_s ~f:force)
;;

let require_equal (type a)
      ?cr
      ?hide_positions
      ?if_false_then_print_s
      ?(message = "values are not equal")
      here
      (module M : With_equal with type t = a)
      x y =
  require ?cr ?hide_positions here (M.equal x y)
    ~if_false_then_print_s:(
      lazy [%message
        message
          ~_:(x : M.t)
          ~_:(y : M.t)
          ~_:(if_false_then_print_s : Sexp.t Lazy.t sexp_option)])
;;

let require_compare_equal (type a)
      ?cr ?hide_positions ?message here
      (module M : With_compare with type t = a)
      x y =
  require_equal ?cr ?hide_positions ?message here
    (module struct
      include M
      let equal = [%compare.equal: t]
    end)
    x y
;;

let require_sets_are_equal (type a)
      ?cr
      ?hide_positions
      ?(names = ("first", "second"))
      here
      (module M : Set with type t = a)
      first
      second =
  require ?cr ?hide_positions here (M.equal first second)
    ~if_false_then_print_s:(lazy (
      let show_diff
            (name1, set1)
            (name2, set2) =
        let diff = M.diff set1 set2 in
        if M.is_empty diff
        then [%message]
        else [%message (sprintf "in %s but not in %s" name1 name2) ~_:(diff : M.t)]
      in
      let first  = (fst names, first)  in
      let second = (snd names, second) in
      [%message.omit_nil
        "sets are not equal"
          ~_:(show_diff first  second : Sexp.t)
          ~_:(show_diff second first  : Sexp.t)]))
;;

type try_with_result =
  | Did_not_raise
  | Raised of Sexp.t

let try_with
      ?raise_message
      ?(show_backtrace = false)
      (type a)
      (f : unit -> a) =
  Backtrace.Exn.with_recording show_backtrace ~f:(fun () ->
    match ignore (f () : a) with
    | ()            -> Did_not_raise
    | exception exn ->
      let backtrace =
        if not show_backtrace
        then None
        else Some (Backtrace.Exn.most_recent ())
      in
      Ref.set_temporarily Backtrace.elide (not show_backtrace) ~f:(fun () ->
        Raised [%message
          ""
            ~_:(raise_message : string sexp_option)
            ~_:(exn : exn)
            (backtrace : Backtrace.t sexp_option)]))
;;

let require_does_not_raise ?cr ?hide_positions ?show_backtrace here f =
  match try_with f ?show_backtrace ~raise_message:"unexpectedly raised" with
  | Did_not_raise -> ()
  | Raised message -> print_cr ?cr ?hide_positions here message
;;

let require_does_raise ?cr ?hide_positions ?show_backtrace here f =
  match try_with f ?show_backtrace with
  | Raised message -> print_s ?hide_positions message
  | Did_not_raise  -> print_cr ?cr ?hide_positions here [%message "did not raise"]

module type Int63able = sig
  type t
  val to_int63     : t -> Int63.t
  val of_int63_exn : Int63.t -> t
end

let print_and_check_stable_internal
      (type a) ?cr ?hide_positions ?max_binable_length here
      (module M : Stable_without_comparator with type t = a)
      (int63able : (module Int63able with type t = a) option)
      list =
  let equal = [%compare.equal: M.t] in
  print_s ?hide_positions [%message
    "" ~bin_shape_digest:(Bin_prot.Shape.eval_to_digest_string M.bin_shape_t : string)];
  require_does_not_raise ?cr ?hide_positions here (fun () ->
    List.iter list ~f:(fun original ->
      let sexp   = M.sexp_of_t                  original in
      let bin_io = Binable.to_string (module M) original in
      let int63  = Option.map int63able ~f:(fun (module I) -> I.to_int63 original) in
      print_s ?hide_positions [%message
        ""
          (sexp   : Sexp.t)
          (bin_io : string)
          (int63  : Int63.t sexp_option)];
      let sexp_roundtrip = M.t_of_sexp sexp in
      require ?cr ?hide_positions here (equal original sexp_roundtrip)
        ~if_false_then_print_s:
          (lazy [%message
            "sexp serialization failed to round-trip"
              (original       : M.t)
              (sexp           : Sexp.t)
              (sexp_roundtrip : M.t)]);
      let bin_io_roundtrip = Binable.of_string (module M) bin_io in
      require ?cr ?hide_positions here (equal original bin_io_roundtrip)
        ~if_false_then_print_s:
          (lazy [%message
            "bin-io serialization failed to round-trip"
              (original         : M.t)
              (bin_io           : string)
              (bin_io_roundtrip : M.t)]);
      begin
        match max_binable_length with
        | None -> ()
        | Some max_binable_length ->
          let bin_io_length = String.length bin_io in
          require ?cr ?hide_positions here (bin_io_length <= max_binable_length)
            ~if_false_then_print_s:
              (lazy [%message
                "bin-io serialization exceeds max binable length"
                  (original           : M.t)
                  (bin_io             : string)
                  (bin_io_length      : int)
                  (max_binable_length : int)])
      end;
      begin
        match int63able with
        | None            -> ()
        | Some (module I) ->
          let int63           = Option.value_exn int63 in
          let int63_roundtrip = I.of_int63_exn   int63 in
          require ?cr ?hide_positions here (equal original int63_roundtrip)
            ~if_false_then_print_s:
              (lazy [%message
                "int63 serialization failed to round-trip"
                  (original        : M.t)
                  (int63           : Int63.t)
                  (int63_roundtrip : M.t)])
      end))
;;

let print_and_check_stable_type
      (type a) ?cr ?hide_positions ?max_binable_length here
      (module M : Stable_without_comparator with type t = a)
      list =
  print_and_check_stable_internal ?cr ?hide_positions ?max_binable_length
    here (module M) None list
;;

let print_and_check_stable_int63able_type
      (type a) ?cr ?hide_positions ?max_binable_length here
      (module M : Stable_int63able with type t = a)
      list =
  print_and_check_stable_internal ?cr ?hide_positions ?max_binable_length
    here (module M) (Some (module M)) list
;;

let show_raise (type a) ?hide_positions ?show_backtrace (f : unit -> a) =
  print_s ?hide_positions
    (match try_with f ?show_backtrace ~raise_message:"raised" with
     | Did_not_raise  -> [%message "did not raise"]
     | Raised message -> message)
;;

(* We disable inlining for [require_allocation_does_not_exceed] so the GC stats and the
   call to [f] are never rearranged. *)
let [@inline never] require_allocation_does_not_exceed
      ?(cr = CR.CR)
      ?hide_positions
      allocation_limit
      here
      f =
  (* We call [Gc.minor] twice here in order to work around non-determinism in minor
     collections, by getting the GC and minor heap in a consistent state.  Without these
     we see occassional failures in tests that are not related to the function under test.
     This is possibly an Ocaml bug as of 4.05. *)
  Gc.minor ();
  Gc.minor ();
  let minor_words_before = Gc.minor_words () in
  let major_words_before = Gc.major_words () in
  (* We wrap [f ()] with [Sys.opaque_identity] to prevent the return value from being
     optimized away. *)
  let x = Sys.opaque_identity (f ()) in
  let minor_words_after = Gc.minor_words () in
  let major_words_after = Gc.major_words () in
  let major_words_allocated = major_words_after - major_words_before in
  let minor_words_allocated = minor_words_after - minor_words_before in
  require here ~cr ?hide_positions
    (Allocation_limit.is_ok allocation_limit
       ~major_words_allocated ~minor_words_allocated)
    ~if_false_then_print_s:
      (lazy (
         let minor_words_allocated, major_words_allocated =
           if CR.hide_unstable_output cr
           then None, None
           else if major_words_allocated > 0
                || Allocation_limit.show_major_words allocation_limit
           then Some minor_words_allocated, Some major_words_allocated
           else Some minor_words_allocated, None
         in
         [%message
           "allocation exceeded limit"
             (allocation_limit      : Allocation_limit.t)
             (minor_words_allocated : int sexp_option)
             (major_words_allocated : int sexp_option)]));
  x
;;

let require_no_allocation ?cr ?hide_positions here f =
  require_allocation_does_not_exceed ?cr ?hide_positions (Minor_words 0) here f
;;

let print_and_check_comparable_sexps
      (type a) ?cr ?hide_positions here
      (module M : With_comparable with type t = a)
      list =
  let set = M.Set.of_list list in
  let set_sexp = [%sexp (set : M.Set.t)] in
  print_s [%message "Set" ~_:(set_sexp : Sexp.t)];
  let sorted_list_sexp = [%sexp (List.sort list ~compare:M.compare : M.t list)] in
  require ?cr ?hide_positions here
    (Sexp.equal set_sexp sorted_list_sexp)
    ~if_false_then_print_s:
      (lazy [%message
        "set sexp does not match sorted list sexp"
          (set_sexp         : Sexp.t)
          (sorted_list_sexp : Sexp.t)]);
  let alist = List.mapi list ~f:(fun i x -> x, i) in
  let map = M.Map.of_alist_exn alist in
  let map_sexp = [%sexp (map : int M.Map.t)] in
  print_s [%message "Map" ~_:(map_sexp : Sexp.t)];
  let sorted_alist_sexp =
    [%sexp (List.sort alist ~compare:(fun (x, _) (y, _) -> M.compare x y)
            : (M.t * int) list)]
  in
  require ?cr ?hide_positions here
    (Sexp.equal map_sexp sorted_alist_sexp)
    ~if_false_then_print_s:
      (lazy [%message
        "map sexp does not match sorted alist sexp"
          (map_sexp          : Sexp.t)
          (sorted_alist_sexp : Sexp.t)]);
;;

let print_and_check_hashable_sexps
      (type a) ?cr ?hide_positions here
      (module M : With_hashable with type t = a)
      list =
  let hash_set = M.Hash_set.of_list list in
  let hash_set_sexp = [%sexp (hash_set : M.Hash_set.t)] in
  print_s [%message "Hash_set" ~_:(hash_set_sexp : Sexp.t)];
  let sorted_list_sexp = [%sexp (List.sort list ~compare:M.compare : M.t list)] in
  require ?cr ?hide_positions here
    (Sexp.equal hash_set_sexp sorted_list_sexp)
    ~if_false_then_print_s:
      (lazy [%message
        "hash_set sexp does not match sorted list sexp"
          (hash_set_sexp    : Sexp.t)
          (sorted_list_sexp : Sexp.t)]);
  let alist = List.mapi list ~f:(fun i x -> x, i) in
  let table = M.Table.of_alist_exn alist in
  let table_sexp = [%sexp (table : int M.Table.t)] in
  print_s [%message "Table" ~_:(table_sexp : Sexp.t)];
  let sorted_alist_sexp =
    [%sexp (List.sort alist ~compare:(fun (x, _) (y, _) -> M.compare x y)
            : (M.t * int) list)]
  in
  require ?cr ?hide_positions here
    (Sexp.equal table_sexp sorted_alist_sexp)
    ~if_false_then_print_s:
      (lazy [%message
        "table sexp does not match sorted alist sexp"
          (table_sexp        : Sexp.t)
          (sorted_alist_sexp : Sexp.t)]);
;;

let print_and_check_container_sexps (type a) ?cr ?hide_positions here m list =
  let (module M : With_containers with type t = a) = m in
  print_and_check_comparable_sexps ?cr ?hide_positions here (module M) list;
  print_and_check_hashable_sexps   ?cr ?hide_positions here (module M) list;
;;

let quickcheck
      here
      ?cr
      ?hide_positions
      ?seed
      ?sizes
      ?trials
      ?shrinker
      ?shrink_attempts
      ?examples
      ~sexp_of
      ~f
      gen
  =
  match
    Quickcheck.test_or_error
      ?seed
      ?sizes
      ?trials
      ?shrinker
      ?shrink_attempts
      ?examples
      ~sexp_of
      gen
      ~f:(fun elt ->
        let cr_count = ref 0 in
        let original_on_print_cr = !on_print_cr in
        Ref.set_temporarily on_print_cr
          (fun string -> incr cr_count; original_on_print_cr string)
          ~f:(fun () -> f elt);
        if !cr_count > 0
        then Or_error.errorf "printed %d CRs for Quickcheck-generated input" !cr_count
        else Ok ())
  with
  | Ok ()       -> ()
  | Error error -> print_cr here ?cr ?hide_positions [%sexp (error : Error.t)]
;;

module Expect_test_config = struct
  include Expect_test_config

  let run f =
    try
      f ();
    with exn ->
      let backtrace = Backtrace.Exn.most_recent () in
      Ref.set_temporarily Backtrace.elide false ~f:(fun () ->
        print_s [%message
          "\
  A top-level expression in [let%expect] raised -- consider using [show_raise]"
            ~_:(exn : Exn.t)
            (backtrace : Backtrace.t)])
  ;;
end
