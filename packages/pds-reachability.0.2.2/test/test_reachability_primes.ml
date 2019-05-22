(**
   This test module performs a series of operations to test the PDA reachability
   functionality in the Odefa analysis library.
*)

open Batteries;;
open Jhupllib;;
open OUnit2;;

let lazy_logger = Logger_utils.make_lazy_logger "Test_reachability";;

open Pds_reachability_types_stack;;

type state =
  | Number of int
  | Count of int
  [@@deriving eq, ord, show, to_yojson]
;;

module Test_state =
struct
  type t = state
  let equal = equal_state
  let compare = compare_state
  let pp = pp_state
  let show = show_state
  let to_yojson = state_to_yojson
end;;

type stack_elt =
  | Bottom of char
  | Prime of int
  [@@deriving eq, ord, show, to_yojson]
;;

module Test_stack_element =
struct
  type t = stack_elt
  let equal = equal_stack_elt
  let compare = compare_stack_elt
  let pp = pp_stack_elt
  let show = show_stack_elt
  let to_yojson = stack_elt_to_yojson
end;;

module Test_spec =
struct
  module State = Test_state
  module Stack_element = Test_stack_element
end;;

module Test_dph =
struct
  module State = Test_state
  module Stack_element = Test_stack_element
  type targeted_dynamic_pop_action =
    | Pop_anything_but of stack_elt
    [@@deriving eq, ord, show, to_yojson]
  ;;
  module Targeted_dynamic_pop_action =
  struct
    type t = targeted_dynamic_pop_action;;
    let equal = equal_targeted_dynamic_pop_action;;
    let compare = compare_targeted_dynamic_pop_action;;
    let pp = pp_targeted_dynamic_pop_action;;
    let show = show_targeted_dynamic_pop_action;;
    let to_yojson = targeted_dynamic_pop_action_to_yojson;;
  end;;
  module Untargeted_dynamic_pop_action =
  struct
    type t = Foo of t
      [@@deriving eq, ord, show, to_yojson]
  end;;
  module Stack_action =
    Stack_action_constructor(Stack_element)(Targeted_dynamic_pop_action)
  ;;
  module Terminus =
    Terminus_constructor(State)(Untargeted_dynamic_pop_action)
  ;;
  let perform_targeted_dynamic_pop element action =
    match action with
    | Pop_anything_but se ->
      if equal_stack_elt element se then
        Enum.empty ()
      else
        Enum.singleton []
  ;;
  let perform_untargeted_dynamic_pop _ _ =
    Enum.empty ()
  ;;
end;;

module Test_reachability =
  (*TODO: there appears to be a problem here with not having untargeted dynamic pops, and also with pretty printing*)
  Pds_reachability.Make
    (Test_spec)
    (Test_dph)
    (Pds_reachability_work_collection_templates.Work_stack)
;;
open Test_reachability.Stack_action.T;;
open Test_reachability.Terminus.T;;
let prime_factor_count_test =
  "prime_factor_count_test" >:: fun _ ->
    let is_prime n =
      let rec div k =
        ( if k <= 1 then true else
          match n mod k with
         | 0 -> false
         | _ -> div (k-1)) in
      (match n with
       | 1 -> false
       | _ -> div (n-1)
      ) in
    let is_factor n k =
      (if n mod k == 0 then true else false) in
    let start = Number 12 in
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge_function
        (*transitions from each Number state to its prime factors (also Numbers)*)
        (fun state ->
           match state with
           | Number n ->
            let less_than_state = List.of_enum (1--n) in
            let primes_less_than_state = List.filter is_prime less_than_state in
            let prime_factors = List.filter (is_factor n) primes_less_than_state in
            let transition_list = List.map (fun k -> ([Push (Prime k)], Static_terminus(Number(n/k)))) prime_factors in
            List.enum transition_list
           | Count _ ->
            Enum.empty ()
        )
      |> Test_reachability.add_edge (Number 1) [] (Count 0)
        (*epsilon transition from Number 1 to Count 0, after original number has been factored*)
      |> Test_reachability.add_edge_function
        (*pop a Prime from the stack and count it*)
        (fun state ->
           match state with
           | Count c ->
             Enum.singleton ([Pop_dynamic_targeted(Test_dph.Pop_anything_but(Bottom '$'))], Static_terminus(Count (c+1)))
           | Number _ ->
             Enum.empty ()
        )
      |> Test_reachability.add_edge_function
        (*pop Bottom stack element*)
        (fun state ->
           match state with
           | Count _ ->
             Enum.singleton ([Pop (Bottom '$')], Static_terminus(state))
           | Number _ ->
             Enum.empty ())
      |> Test_reachability.add_start_state start [Push (Bottom '$')]
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states start [Push (Bottom '$')] analysis in
    assert_equal ~printer:(Pp_utils.pp_to_string @@ Pp_utils.pp_list pp_state) [Count 3] (List.sort compare @@ List.of_enum states)
;;

let tests = "Test_reachability_primes" >:::
           [ prime_factor_count_test
           ]
;;
