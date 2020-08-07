open Fmlib
open Common

open Term


module type GAMMA =
sig
 type t
  val is_valid_index: int -> t -> bool
  val name_of_index: int -> t -> string
  val push_local: string -> Term.typ -> t -> t
end



module Pretty (Gamma: GAMMA) (P: Pretty_printer.SIG) =
  struct
    open Gamma

    type pr_result =
      Operator.t option * P.t

    type print0 = Term.t -> Gamma.t -> P.t

    type print  = Term.t -> Gamma.t -> pr_result


    let pi_info (info: Pi_info.t): string * bool =
        Pi_info.name info,
        Pi_info.is_typed info


    let rec split_pi
              (t:Term.t)
              (c:t)
            : (string * bool * Term.typ * t) list * Term.t * t =
      match t with
      | Pi (tp, t, info) when not (Pi_info.is_arrow info) ->
         let name, is_typed = pi_info info
         in
         let lst, t_inner, c_inner =
           split_pi t (push_local name tp c)
         in
         (name, is_typed, tp, c) :: lst, t_inner, c_inner
      | _ ->
         [], t, c


    let print_sort: Term.Sort.t -> pr_result = function
      | Proposition ->
         None, P.string "Proposition"

      | Any i ->
         let str =
           if i = 0 then
             "Any"
           else
             "Any(" ^ string_of_int i ^ ")"
         in
         None,
         P.string str


    let print_value: Term.Value.t -> pr_result = function
      | Term.Value.Int i ->
         None,
         P.string (string_of_int i)

      | Term.Value.Char i ->
         None,
         P.(char '\'' <+> char (Char.chr i) <+> char '\'')

      | Term.Value.String str ->
         None,
         P.(char '"' <+> string str <+> char '"')

      | Term.Value.Unary _ | Term.Value.Binary _ ->
         None,
         P.(string "<function>")


    let parenthesize
          ((lower,pr): Operator.t option * P.t)
          (is_left: bool)
          (upper: Operator.t)
        : P.t
      =
      if Operator.needs_parens lower is_left upper then
        P.(chain [char '('; pr; char ')'])

      else
        pr


    let two_operands
          (a: Term.t) (b:Term.t) (upper: Operator.t)
          (print: print)
          (c:t)
        : P.t * P.t =
      parenthesize (print a c) true upper,
      parenthesize (print b c) false upper



    let formal_argument
        (name: string)
        (typed: bool)
        (tp: Term.typ)
        (print: print0)
        (c: Gamma.t)
        : P.t
        =
        let open P in
        if typed then
          char '(' <+> string name <+> string ": " <+> print tp c <+> char ')'
        else
          string name





    let print_definition
        (name: string)
        (exp: Term.t)
        (raw_print: print0)
        (c: Gamma.t)
        : P.t
        =
        let open P in
        let rec print exp c =
            match exp with
            | Lambda (tp, exp, info) ->
                let name = Lambda_info.name info in
                group space
                <+> formal_argument
                        name
                        (Lambda_info.is_typed info)
                        tp
                        raw_print
                        c
                <+> print exp (push_local name tp c)
            | _ ->
                (
                    match exp with
                    | Typed (exp, tp) ->
                        char ':'
                        <+> group space <+> raw_print tp c
                        <+> group space <+> string ":="
                        <+> group (
                            nest 4 (space <+> raw_print exp c)
                        )
                    | _ ->
                        group space <+> string ":="
                        <+> group (
                            nest 4 (space <+> raw_print exp c)
                        )
                )
        in
        string name <+> print exp c


    let rec print (t:Term.t) (c:Gamma.t): pr_result =
        let raw_print t c =
          snd (print t c)
        in
        let print_name_type name is_typed tp c =
          let name = if name = "" then P.char '_' else P.string name
          in
          if is_typed then
              P.(char '('
                  <+> name
                  <+> string ": "
                  <+> snd (print tp c)
                  <+> char ')')
          else
              name
        in
        match t with
        | Sort s ->
            print_sort s

        | Value v ->
           print_value v

        | Variable i ->
           None,
           P.string
             (if is_valid_index i c then
                let name = name_of_index i c in
                let len  = String.length name in
                assert (1 <= len);
                let c0 = name.[0] in
                if Operator.is_keyword_operator name then
                    "("  ^ name ^ ")"
                else if Char.is_letter c0
                    || c0 = '_'
                    || (2 <= len && Char.is_digit name.[1])
                    || name = "[]"
                    || name = "()"
                then
                    name
                else
                    "(" ^ name ^ ")"
              else
                "<invalid " ^ string_of_int i ^ ">")

        | Typed (e, tp) ->
            let e_pr, tp_pr = two_operands e tp Operator.colon print c in
            Some Operator.colon,
            P.( group (
                    e_pr <+> char ':'
                    <+> nest 4
                            (space <+> tp_pr)
                )
            )

        | Appl (f, operand2, Binary) ->
            let rec find_operand1 f =
                match f with
                | Appl (f, operand1, Binary) ->
                    Some (f, operand1)
                | Appl (f, _, Implicit ) ->
                    find_operand1 f
                | _ ->
                    None
            in
            let rec find_operator f =
                match f with
                | Appl (f, _, Implicit) ->
                    find_operator f
                | Variable i when is_valid_index i c ->
                    Some i
                | _ ->
                    None
            in
            let res =
                Option.(
                    find_operand1 f >>= fun (f, operand1) ->
                    find_operator f >>= fun operator ->
                    Some (operator, operand1))
            in
            (match res with
            | None ->
                print (Appl (f, operand2, Normal)) c
            | Some (op_idx, operand1) ->
                let op_string = name_of_index op_idx c in
                let op_data = Operator.of_string op_string in
                let a_pr, b_pr =
                    two_operands operand1 operand2 op_data print c
                in
                Some op_data,
                P.(chain [a_pr;
                          group space;
                          string op_string;
                          char ' ';
                          b_pr])
            )

        | Appl (Variable i, arg, Unary) ->
            let op_str = name_of_index i c in
            assert (Operator.is_unary op_str);
            let op_data = Operator.of_string op_str
            in
            Some op_data,
            P.(
                string op_str
                <+> char ' '
                <+>
                parenthesize (print arg c) false op_data
            )

        | Appl (f, _, Implicit) ->
            print f c

        | Appl (f, a, _) ->
            Some Operator.application,
            P.( parenthesize (print f c) true Operator.application
                <+> char ' '
                <+> parenthesize (print a c) false Operator.application )

        | Lambda _ as term ->
            Some Operator.assign,
            print_definition "\\" term raw_print c

        | Pi (tp, rt, info)
            when
                Pi_info.is_arrow info
                || not (Term.has_variable 0 rt)
            ->
               let c_inner = push_local "_" tp c
               and op_data = Operator.of_string "->"
               in
               let tp_pr =
                 parenthesize (print tp c) true op_data
               and rt_pr =
                 parenthesize (print rt c_inner) false op_data
               in
               Some op_data,
               P.(chain [tp_pr;
                         group space;
                         string "->";
                         char ' ';
                         rt_pr])

        | Pi (tp, t, info) ->
            let nme, is_typed = pi_info info in
            let lst, t_inner, c_inner =
                split_pi t (push_local nme tp c) in
            let lst = (nme, is_typed, tp, c) :: lst in
            Some Operator.colon,
            P.(
                group (
                    string "all "
                    <+> nest_relative 0 (
                        list_separated
                            space
                            (List.map
                                (fun (nme, is_typed, tp, c) ->
                                    print_name_type nme is_typed tp c
                                )
                                lst
                            )
                        <+> cut <+> string ": "
                        <+> raw_print t_inner c_inner
                    )
                )
            )

        | Where (name, tp, exp, value) ->
            let open P in
            let rec print_where name tp exp defs c =
                let c = push_local name tp c in
                match exp with
                | Where (name, tp, exp, value) ->
                    print_where
                        name
                        tp
                        exp
                        (print_definition name value raw_print c :: defs)
                        c
                | _ ->
                    raw_print exp c, defs
            in
            let exp, defs =
                print_where
                    name
                    tp
                    exp
                    [print_definition name value raw_print c] c
            in
            Some Operator.where,
            exp <+> group space <+> string "where"
            <+> group (
                nest 4 (space <+> list_separated (line "; ") defs)
            )


    let print (t:Term.t) (c: Gamma.t): P.t =
        snd (print t c)
  end (* Pretty *)







module String_print (Gamma:GAMMA) =
struct
    let string_of_term (t:Term.t) (c: Gamma.t): string =
        let module PP = Pretty_printer.Pretty (String_printer) in
        let module P = Pretty (Gamma) (PP) in
        String_printer.run
            (PP.run 0 70 70 (P.print t c))
end





let string_of_term (t:Term.t) (c: Gamma.t): string =
    let module SP = String_print (Gamma) in
    SP.string_of_term t c
