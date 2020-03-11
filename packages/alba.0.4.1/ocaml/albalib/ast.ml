open Fmlib

module Located = Character_parser.Located

module Position = Character_parser.Position

type range = Position.t * Position.t


module Expression = struct
  type operator = string * Operator.t

  type argument_type =
    | Normal
    | Operand


  type t =
    t0 Located.t

  and t0 =
    | Proposition
    | Any
    | Identifier of string
    | Number of string
    | Char of int
    | String of string
    | Operator of operator
    | Typed of t * t                      (* exp, type *)
    | Application of t * (t * argument_type) list
    | Function of
        formal_argument list
        * t option                        (* result type *)
        * t                               (* defining expression *)
    | Product of formal_argument list * t

  and formal_argument =
    string Located.t * t option


  let make_binary (e1: t) (op: operator Located.t) (e2: t): t =
    let pos_start = Located.start e1
    and pos_end   = Located.end_ e2
    and op_str,_    = Located.value op
    in
    Located.make
      pos_start
      (if op_str = ":" then
         Typed (e1, e2)
       else if op_str = "->" then
           (* e1 -> e2 *)
           let name = Located.map (fun _ -> "_") e1 in
           match Located.value e2 with
           | Product (formal_arguments, result_type) ->
                Product ( (name, Some e1) :: formal_arguments, result_type )
           | _ ->
                Product ([name, Some e1], e2)
       else
         Application (
          Located.map (fun (op_str,_) -> Identifier op_str) op,
          [ e1, Operand;
            e2, Operand]))
      pos_end


  let rec binary
            (e0:t)
            (rest: (operator Located.t * t) list)
          : (t, range * string * string) result
    (* Analyze the precedence and associativity of an operator expresssion

        e0 op1 e1 op2 e2 ... opn en

       where [e0] is given explicitly and the rest is given as a list

        [(op1,e1), (op2,e2), ...]
     *)
    =
    let module Res =
      Monad.Result
        (struct type t = range * string * string end)
    in
    match rest with
    | [] ->
       Ok e0

    | [op, e1] ->
       Ok (make_binary e0 op e1)

    | (op1,e1) :: (op2,e2) :: rest ->
       (* e0 op1 e1 op2 e2 rest *)
       let op1_string, op1_data = Located.value op1
       and op2_string, op2_data = Located.value op2
       in
       let cmp = Operator.compare op1_data op2_data in
       if cmp = 0 then
         match Operator.associativity op1_data with
         | Operator.No ->
            (* Error case: I cannot decide on how to parenthesize *)
            Error ((Located.start e0, Located.end_ e2), op1_string, op2_string)
         | Operator.Left ->
            (* (e1 op1 e2) op2 e2 rest *)
            binary (make_binary e0 op1 e1) ((op2,e2) :: rest)
         | Operator.Right ->
            (* e1 op1 (e2 op2 e2 rest) *)
            Res.map (make_binary e0 op1) (binary e1 ((op2,e2) :: rest))

       else if cmp = +1 then
         (* (e1 op1 e2) op2 e2 rest *)
         binary (make_binary e0 op1 e1) ((op2,e2) :: rest)

       else
         (* e0 op1 (e1 op2 e2 rest1) rest2 *)
         let rest2, rest3 =
           List.split_at
             (fun (op,_) ->
               Operator.precedence (snd (Located.value op))
               <= Operator.precedence op1_data)
             rest
         in
         Res.(binary e1 ((op2,e2) :: rest2)
              >>= fun e ->
              binary (make_binary e0 op1 e) rest3)
end
