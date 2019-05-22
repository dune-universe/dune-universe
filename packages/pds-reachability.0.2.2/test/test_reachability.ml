(**
   This test module performs a series of operations to test the PDA reachability
   functionality in the Odefa analysis library.
*)

open Batteries;;
open Jhupllib;;
open OUnit2;;

let lazy_logger = Logger_utils.make_lazy_logger "Test_reachability";;

open Pds_reachability_types_stack;;

module Test_state =
struct
  type t = int
  let equal = (==)
  let compare = compare
  let pp = Format.pp_print_int
  let show = string_of_int
  let to_yojson n = `Int n
end;;

module Test_stack_element =
struct
  type t = char
  let equal = (==)
  let compare = compare
  let pp fmt c = Format.pp_print_string fmt (String.make 1 c)
  let show c = String.make 1 c
  let to_yojson c = `String (String.make 1 c)
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
    | Double_push
    | Consume_identical_1_of_2
    | Consume_identical_2_of_2 of Stack_element.t
    | Chain_two_push_1_of_2 of Stack_element.t * Stack_element.t
    | Chain_two_push_2_of_2 of Stack_element.t
    | Pop_anything
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
  type untargeted_dynamic_pop_action =
    | Target_condition_on_element_is_A of State.t * State.t
  [@@deriving eq, ord, show, to_yojson]
  ;;
  module Untargeted_dynamic_pop_action =
  struct
    type t = untargeted_dynamic_pop_action;;
    let equal = equal_untargeted_dynamic_pop_action;;
    let compare = compare_untargeted_dynamic_pop_action;;
    let pp = pp_untargeted_dynamic_pop_action;;
    let show = show_untargeted_dynamic_pop_action;;
    let to_yojson = untargeted_dynamic_pop_action_to_yojson;;
  end;;
  module Stack_action =
    Stack_action_constructor(Stack_element)(Targeted_dynamic_pop_action)
  ;;
  module Terminus =
    Terminus_constructor(State)(Untargeted_dynamic_pop_action)
  ;;

  open Stack_action.T;;
  open Terminus.T;;

  let perform_targeted_dynamic_pop element action =
    match action with
    | Double_push -> Enum.singleton [Push(element);Push(element)]
    | Consume_identical_1_of_2 -> Enum.singleton
                                    [Pop_dynamic_targeted(Consume_identical_2_of_2 element)]
    | Consume_identical_2_of_2 element' ->
      if Test_stack_element.compare element element' == 0
      then Enum.singleton []
      else Enum.empty ()
    | Chain_two_push_1_of_2(k1,k2) ->
      Enum.singleton
        [ Pop_dynamic_targeted(Chain_two_push_2_of_2(k2))
        ; Push(k1)
        ; Push(element) ]
    | Chain_two_push_2_of_2 k ->
      Enum.singleton
        [ Push(k)
        ; Push(element) ]
    | Pop_anything ->
      Enum.singleton []
  ;;
  let perform_untargeted_dynamic_pop element action =
    match action with
    | Target_condition_on_element_is_A(s1,s2) ->
      if element == 'a'
      then Enum.singleton ([],Static_terminus s1)
      else Enum.singleton ([],Static_terminus s2)
  ;;
end;;

module Test_reachability =
  Pds_reachability.Make
    (Test_spec)
    (Test_dph)
    (Pds_reachability_work_collection_templates.Work_stack)
;;

open Test_reachability.Stack_action.T;;
open Test_reachability.Terminus.T;;

let immediate_reachability_test =
  "immediate_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Pop 'a'] 1
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.of_enum states) [1]
;;

let immediate_non_reachable_test =
  "immediate_non_reachable_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Pop 'b'] 1
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.of_enum states) []
;;

let two_step_reachability_test =
  "two_step_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Pop 'a'; Push 'b'] 1
      |> Test_reachability.add_edge 1 [Pop 'b'] 2
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.of_enum states) [2]
;;

let cycle_reachability_test =
  "cycle_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Push 'b'] 0
      |> Test_reachability.add_edge 0 [Push 'c'] 1
      |> Test_reachability.add_edge 1 [Pop 'c'] 1
      |> Test_reachability.add_edge 1 [Pop 'b'] 1
      |> Test_reachability.add_edge 1 [Pop 'a'] 2
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.of_enum states) [2]
;;

let edge_function_reachability_test =
  "edge_function_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge_function
        (fun state ->
           if state >= 50 then Enum.empty () else
             Enum.singleton @@ ([Push 'b'], Static_terminus(state + 1)))
      |> Test_reachability.add_edge 50 [Pop 'b'] 50
      |> Test_reachability.add_edge 50 [Pop 'a'] 51
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.of_enum states) [51]
;;

let nondeterminism_reachability_test =
  "nondeterminism_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Pop 'a'] 1
      |> Test_reachability.add_edge 0 [Pop 'a'] 2
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    let expected_states = [1;2] in
    let actual_states = (List.sort compare @@ List.of_enum states) in
    assert_equal expected_states actual_states
;;

let targeted_dynamic_pop_reachability_test =
  "targeted_dynamic_pop_reachability_test" >:: fun _ ->
    (* The following function dynamically duplicates an element on the stack. *)
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0
        [Pop_dynamic_targeted Test_dph.Double_push] 1
      |> Test_reachability.add_edge 1 [Pop 'a'; Pop 'a'] 2
      |> Test_reachability.add_edge 1 [Pop 'a'] 3
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [2]
;;

let targeted_dynamic_pop_nondeterminism_reachability_test =
  "targeted_dynamic_pop_nondeterminism_reachability_test" >:: fun _ ->
    let dyn = Pop_dynamic_targeted Test_dph.Consume_identical_1_of_2 in
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Push 'b'; Push 'c'] 1
      |> Test_reachability.add_edge 1 [dyn] 2
      |> Test_reachability.add_edge 2 [Pop 'a'] 3
      |> Test_reachability.add_edge 0 [Push 'x'; Push 'x'] 4
      |> Test_reachability.add_edge 4 [dyn] 5
      |> Test_reachability.add_edge 5 [Pop 'a'] 6
      |> Test_reachability.add_edge 0 [Push 'y'; Push 'y'] 7
      |> Test_reachability.add_edge 7 [dyn; Pop 'a'] 8
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [6;8]
;;

let untargeted_dynamic_pop_reachability_test =
  "untargeted_dynamic_pop_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Push 'a'] 1
      |> Test_reachability.add_untargeted_dynamic_pop_action
        1 (Test_dph.Target_condition_on_element_is_A(2,3))
      |> Test_reachability.add_edge 2 [Pop 'q'] 8
      |> Test_reachability.add_edge 3 [Pop 'q'] 9
      |> Test_reachability.add_edge 0 [Push 'b'] 11
      |> Test_reachability.add_untargeted_dynamic_pop_action
        11 (Test_dph.Target_condition_on_element_is_A(12,13))
      |> Test_reachability.add_edge 12 [Pop 'q'] 18
      |> Test_reachability.add_edge 13 [Pop 'q'] 19
      |> Test_reachability.add_edge 0 [Push 'a'] 21
      |> Test_reachability.add_edge 0 [Push 'b'] 21
      |> Test_reachability.add_untargeted_dynamic_pop_action
        21 (Test_dph.Target_condition_on_element_is_A(22,23))
      |> Test_reachability.add_edge 22 [Pop 'q'] 28
      |> Test_reachability.add_edge 23 [Pop 'q'] 29
      |> Test_reachability.add_start_state 0 [Push 'q']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states =
      List.sort compare @@ List.of_enum @@
      Test_reachability.get_reachable_states 0 [Push 'q'] analysis
    in
    lazy_logger `trace
      (fun () -> "states: " ^ String_utils.string_of_list string_of_int states);
    assert_equal states [8;19;28;29]
;;

let untargeted_dynamic_pop_function_reachability_test =
  "untargeted_dynamic_pop_function_reachability_test" >:: fun _ ->
    (* We're building this:
       1. (S) -- Push 'q' --> (0)
       2. (0) -- Push 'a', Push 'b', Push 'b' --> (1)
       3. (0) -- Push 'a', Push 'a', Push 'a' --> (1)
       4. (0) -- Push 'b', Push 'b' --> (1)
       5. (0) -- Nop --> (1)
       6. (n) -- Pop 'a' --> (n+1)  for 1 <= n <= 7
       7. (n) -- Pop not 'a' --> (n+2) for 1 <= n <= 7
       8. (n) -- Pop 'q' --> (n+20) for 1 <= n <= 7

       Given the above, we expect the following successful paths:

       (S) ~1~> (0) ~2~> (1) ~7~> (3) ~7~> (5) ~6~> (6) ~7~> (8)
       (S) ~1~> (0) ~2~> (1) ~7~> (3) ~7~> (5) ~6~> (6) ~8~> (26)
       (S) ~1~> (0) ~3~> (1) ~6~> (2) ~6~> (3) ~6~> (4) ~7~> (6)
       (S) ~1~> (0) ~3~> (1) ~6~> (2) ~6~> (3) ~6~> (4) ~8~> (24)
       (S) ~1~> (0) ~4~> (1) ~7~> (3) ~7~> (5) ~7~> (7)
       (S) ~1~> (0) ~4~> (1) ~7~> (3) ~7~> (5) ~8~> (25)
       (S) ~1~> (0) ~5~> (1) ~7~> (3)
       (S) ~1~> (0) ~5~> (1) ~8~> (21)
    *)
    let edge_function state =
      if state >= 1 && state < 8
      then Enum.singleton ([Pop 'q'], Static_terminus(state + 20))
      else Enum.empty ()
    in
    let untargeted_dynamic_pop_action_fn state =
      if state >= 1 && state < 8
      then Enum.singleton
          (Test_dph.Target_condition_on_element_is_A(state+1,state+2))
      else Enum.empty ()
    in
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0 [Push 'a'; Push 'b'; Push 'b'] 1
      |> Test_reachability.add_edge 0 [Push 'a'; Push 'a'; Push 'a'] 1
      |> Test_reachability.add_edge 0 [Push 'b'; Push 'b'] 1
      |> Test_reachability.add_edge 0 [] 1
      |> Test_reachability.add_edge_function edge_function
      |> Test_reachability.add_untargeted_dynamic_pop_action_function
        untargeted_dynamic_pop_action_fn
      |> Test_reachability.add_start_state 0 [Push 'q']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states =
      List.sort compare @@ List.of_enum @@
      Test_reachability.get_reachable_states 0 [Push 'q'] analysis
    in
    lazy_logger `trace
      (fun () -> "states: " ^ String_utils.string_of_list string_of_int states);
    assert_equal states [3;6;7;8;21;24;25;26]
;;

let targeted_dynamic_pop_chain_test =
  "targeted_dynamic_pop_chain_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge 0
        [Pop_dynamic_targeted(
            Test_dph.Chain_two_push_1_of_2('a','b')); Push 'c'] 1
      |> Test_reachability.add_edge 1 [Pop 'c'; Pop 'b'; Pop 'a'; Pop 'x'] 2
      |> Test_reachability.add_start_state 0 [Push 'x']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'x'] analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [2]
;;

let lazy_edge_function_test =
  "lazy_edge_function_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge_function
        (fun state ->
           if state < 10 || state > 99999998 then Enum.empty () else
             List.enum [ ( [ Pop 'a' ]
                         , Static_terminus(state + 1)
                         )
                       ; ( [ Pop 'b' ]
                         , Static_terminus(state + 2)
                         )
                       ]
        )
      |> Test_reachability.add_start_state 0 [Push 'a']
      |> Test_reachability.add_edge 0 [Push 'a'; Push 'a'; Push 'a'] 10
      |> Test_reachability.add_edge 0 [Push 'b'; Push 'b'] 11
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 [Push 'a'] analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [14; 16];
    let (nodes, edges) = Test_reachability.get_size analysis in
    assert_bool "too many nodes" (nodes < 20);
    assert_bool "too many edges" (edges < 100)
;;

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
    let start = 12 in
    let analysis =
      Test_reachability.empty ()
      |> Test_reachability.add_edge_function
        (*transitions from each state to its prime factors*)
        (fun state ->
           if (state > 0) then
             let less_than_state = List.of_enum (1--state) in
             let primes_less_than_state = List.filter is_prime less_than_state in
             let prime_factors =
               List.filter (is_factor state) primes_less_than_state
             in
             let transition_list =
               List.map
                 (fun n -> ([Push 'a'], Static_terminus(state/n))) prime_factors
             in
             List.enum transition_list
           else
             Enum.empty ()
        )
      |> Test_reachability.add_edge 1 [] 0
      |> Test_reachability.add_edge_function
        (fun state ->
           if (state <= 0) then
             Enum.singleton ([Pop 'a'], Static_terminus(state-1))
           else
             Enum.empty ()
        )
      |> Test_reachability.add_edge_function
        (fun state ->
           if state < 0 then
             Enum.singleton ([Pop 'b'], Static_terminus state)
           else
             Enum.empty ())
      |> Test_reachability.add_start_state start [Push 'b']
      |> Test_reachability.fully_close
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
                 String_utils.indent 2 (Test_reachability.show_analysis analysis));
    let states = Test_reachability.get_reachable_states start [Push 'b'] analysis in
    assert_equal ~printer:(Pp_utils.pp_to_string @@ Pp_utils.pp_list Format.pp_print_int) [-3] (List.sort compare @@ List.of_enum states)
;;

let tests = "Test_reachability" >:::
            [ immediate_reachability_test
            ; immediate_non_reachable_test
            ; two_step_reachability_test
            ; cycle_reachability_test
            ; edge_function_reachability_test
            ; nondeterminism_reachability_test
            ; targeted_dynamic_pop_reachability_test
            ; targeted_dynamic_pop_nondeterminism_reachability_test
            ; untargeted_dynamic_pop_reachability_test
            ; untargeted_dynamic_pop_function_reachability_test
            ; lazy_edge_function_test
            ; prime_factor_count_test
            ]
;;
