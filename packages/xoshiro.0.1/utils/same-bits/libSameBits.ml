let epf = Format.eprintf
let fpf = Format.fprintf

let time_limit = 1.
let iterations_limit = 10_000_000
let batch_size = 1000
let refresh_frequency = 0.1

let prec_time =
  1 + int_of_float (ceil (log10 time_limit))

let prec_iterations =
  1 + int_of_float (ceil (log10 (float_of_int iterations_limit)))

type test_case =
    TestCase :
      string
      * (Format.formatter -> 'a -> unit)
      * string
      * (unit -> 'a)
      * string
      * (unit -> 'a)
      -> test_case

let make_test_case ~name ~pp r_name r s_name s =
  TestCase (name, pp, r_name, r, s_name, s)

let run_test_case ?(name_length=0) (TestCase (name, pp, r_name, r, s_name, s)) =

  let begin_time = Sys.time () in (* not real time but portable *)

  let rec test_batch size nb =
    if size > 0 then
      (
        let rv = r () in
        let sv = s () in
        if not (rv = sv) then
          (
            epf "  fail!@.";
            epf "    Try #%d on %s yield different results!@." nb name;
            let len = max (String.length r_name) (String.length s_name) in
            epf "    - %*s yield %a@." len r_name pp rv;
            epf "    - %*s yield %a@." len s_name pp sv;
            exit 1 (* FIXME: better interface *)
          );
        test_batch (size-1) (nb+1)
      )
    else
      nb
  in

  let rec test_all last_refresh nb =
    let curr_time = Sys.time () in
    let time_spent = curr_time -. begin_time in
    if time_spent > time_limit || nb > iterations_limit then
      (
        epf "\r  %*.2fs  %*d  %*s  OK!@."
          prec_time time_spent
          prec_iterations (nb - 1)
          name_length name
      )
    else if curr_time > last_refresh +. refresh_frequency then
      (
        epf "\r  %*.2fs  %*d  %*s@?"
          prec_time time_spent
          prec_iterations nb
          name_length name;
        let nb = test_batch batch_size nb in
        test_all curr_time nb
      )
    else
      (
        let nb = test_batch batch_size nb in
        test_all last_refresh nb
      )
  in

  test_all 0. 1

let max_name_length =
  List.fold_left
    (fun name_length (TestCase (name, _, _, _, _, _)) ->
       max name_length (String.length name)) 0

let run_test_cases (test_cases : test_case list) =
  let name_length = max 4 (max_name_length test_cases) in
  epf "  %*s  %*s  %*s@."
    (4 + prec_time) "time"
    prec_iterations "#iter"
    name_length "name";
  List.iter (run_test_case ~name_length) test_cases

let run_test_case test_case =
  run_test_cases [test_case]

module type BASIC = MakeRandom.Sig.Basic
module type FULL = MakeRandom.Sig.Full

let test_cases_of_basic r_name (module R : BASIC) s_name (module S : BASIC) =
  [
    TestCase (
      "bits",
      (fun fmt -> fpf fmt "0x%x"),
      r_name,
      (fun () -> R.bits ()),
      s_name,
      (fun () -> S.bits ())
    );
    TestCase (
      "int",
      (fun fmt -> fpf fmt "0x%x"),
      r_name,
      (fun () -> R.int (1 lsl 30 - 1)),
      s_name,
      (fun () -> S.int (1 lsl 30 - 1))
    );
    TestCase (
      "int32",
      (fun fmt -> fpf fmt "0x%lx"),
      r_name,
      (fun () -> R.int32 Int32.max_int),
      s_name,
      (fun () -> S.int32 Int32.max_int)
    );
    TestCase (
      "int64",
      (fun fmt -> fpf fmt "0x%Lx"),
      r_name,
      (fun () -> R.int64 Int64.max_int),
      s_name,
      (fun () -> S.int64 Int64.max_int)
    );
    TestCase (
      "nativeint",
      (fun fmt -> fpf fmt "0x%nx"),
      r_name,
      (fun () -> R.nativeint Nativeint.max_int),
      s_name,
      (fun () -> S.nativeint Nativeint.max_int)
    );
    TestCase (
      "float",
      Format.pp_print_float,
      r_name,
      (fun () -> R.float 1.),
      s_name,
      (fun () -> S.float 1.)
    );
    TestCase (
      "bool",
      Format.pp_print_bool,
      r_name,
      (fun () -> R.bool ()),
      s_name,
      (fun () -> S.bool ())
    );
  ]

type state = S : (module FULL with type State.t = 'a) * 'a -> state

let test_cases_of_state r_name (S ((module R), r_state)) s_name (S ((module S), s_state)) =
  [
    TestCase (
      "State.bits",
      (fun fmt -> fpf fmt "0x%x"),
      r_name,
      (fun () -> R.State.bits r_state),
      s_name,
      (fun () -> S.State.bits s_state)
    );
    TestCase (
      "State.int",
      (fun fmt -> fpf fmt "0x%x"),
      r_name,
      (fun () -> R.State.int r_state (1 lsl 30 - 1)),
      s_name,
      (fun () -> S.State.int s_state (1 lsl 30 - 1))
    );
    TestCase (
      "State.int32",
      (fun fmt -> fpf fmt "0x%lx"),
      r_name,
      (fun () -> R.State.int32 r_state Int32.max_int),
      s_name,
      (fun () -> S.State.int32 s_state Int32.max_int)
    );
    TestCase (
      "State.int64",
      (fun fmt -> fpf fmt "0x%Lx"),
      r_name,
      (fun () -> R.State.int64 r_state Int64.max_int),
      s_name,
      (fun () -> S.State.int64 s_state Int64.max_int)
    );
    TestCase (
      "State.nativeint",
      (fun fmt -> fpf fmt "0x%nx"),
      r_name,
      (fun () -> R.State.nativeint r_state Nativeint.max_int),
      s_name,
      (fun () -> S.State.nativeint s_state Nativeint.max_int)
    );
    TestCase (
      "State.float",
      Format.pp_print_float,
      r_name,
      (fun () -> R.State.float r_state 1.),
      s_name,
      (fun () -> S.State.float s_state 1.)
    );
    TestCase (
      "State.bool",
      Format.pp_print_bool,
      r_name,
      (fun () -> R.State.bool r_state),
      s_name,
      (fun () -> S.State.bool s_state)
    );
  ]

let print_header () =
  epf "========== [ SameBits ] ==========@\n@.";
  epf "time limit: %.2fs@." time_limit;
  epf "iterations limit: %d@." iterations_limit;
  epf "batch size: %d@." batch_size;
  epf "refresh frequency: %fs@." refresh_frequency;
  epf "@."

let run_on_basic r_name (module R : BASIC) s_name (module S : BASIC) =
  print_header ();

  epf "basic tests:@.";
  run_test_cases (test_cases_of_basic r_name (module R) s_name (module S));
  epf "@."

let run_on_full r_name (module R : FULL) s_name (module S : FULL) =
  print_header ();

  epf "basic tests:@.";
  run_test_cases (test_cases_of_basic r_name (module R) s_name (module S));
  epf "@.";

  epf "(saving current state for further tests)@.@.";
  let r_state = R.get_state () in
  let s_state = S.get_state () in

  epf "after re-initialisation with `init`:@.";
  R.init 566631242;
  S.init 566631242;
  run_test_cases (test_cases_of_basic r_name (module R) s_name (module S));
  epf "@.";

  epf "after re-initialisation with `full_init`:@.";
  R.full_init [| 566631242; 1112354; 99999999; 0; 12 |];
  S.full_init [| 566631242; 1112354; 99999999; 0; 12 |];
  run_test_cases (test_cases_of_basic r_name (module R) s_name (module S));
  epf "@.";

  epf "after loading previously-saved state:@.";
  R.set_state r_state;
  S.set_state s_state;
  run_test_cases (test_cases_of_basic r_name (module R) s_name (module S));
  epf "@.";

  epf "still using the same state:@.";
  run_test_cases (test_cases_of_state r_name (S ((module R), r_state)) s_name (S ((module S), s_state)));
  epf "@.";

  epf "using a newly-created state:@.";
  let r_state = R.State.make [| 555789242; 245788956; 1111111; 0; 7 |] in
  let s_state = S.State.make [| 555789242; 245788956; 1111111; 0; 7 |] in
  run_test_cases (test_cases_of_state r_name (S ((module R), r_state)) s_name (S ((module S), s_state)));
  epf "@."
