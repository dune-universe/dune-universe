module Log = Dolog.Log

let bin_op qual_type lhs kind rhs =
  Clang.Ast.node ~qual_type (Clang.Ast.BinaryOperator { lhs; kind; rhs })

let int = Clang.Type.make (BuiltinType Int)

let integer_literal ?location i =
  Clang.Ast.node ?location ~qual_type:int
    (Clang.Ast.IntegerLiteral (Clang.Ast.literal_of_int i))

let fresh_var_counter = ref 0

let fresh_var_name () =
  let index = !fresh_var_counter in
  fresh_var_counter := index + 1;
  Printf.sprintf "__tmp_%d" index

let declare_tmp qual_type =
  let tmp = fresh_var_name () in
  let decl_tmp = Clang.Ast.node ~qual_type (Clang.Ast.Decl [
    Clang.Ast.node ~qual_type (Clang.Ast.Var (Clang.Ast.var tmp qual_type))]) in
  let tmp_var =
    Clang.Ast.node ~qual_type
      (Clang.Ast.DeclRef (Clang.Ast.identifier_name tmp)) in
  tmp_var, decl_tmp

let assign_to_tmp ?qual_type expr =
  let qual_type =
    match qual_type with
    | None -> Clang.Type.of_node expr
    | Some qual_type -> qual_type in
  let tmp_var, decl_tmp = declare_tmp qual_type in
  let assign_tmp = Clang.Ast.node ~qual_type (Clang.Ast.Expr (
    bin_op qual_type tmp_var Assign expr)) in
  tmp_var, [decl_tmp; assign_tmp]

let rec cut_break (stmts : Clang.Ast.stmt list) =
  let cut_tl hd tl =
    match cut_break tl with
    | None -> None
    | Some tl -> Some (hd :: tl) in
  match stmts with
  | [] -> None
  | { desc = Break; _ } :: _ -> Some []
  | { desc = Compound stmts; _ } as hd :: tl ->
      begin
        match cut_break stmts with
        | None -> cut_tl hd tl
        | Some stmts -> Some [{ hd with desc = Compound stmts }]
      end
  | hd :: tl -> cut_tl hd tl

let close_stmts delayed_stmts stmts =
  Free_monoid.fold_right List.cons delayed_stmts stmts

let stmts_of_stmt (stmt : Clang.Ast.stmt) =
  match stmt.desc with
  | Null -> []
  | Compound list -> list
  | _ -> [stmt]

let stmt_of_stmts ?location stmts =
  match stmts with
  | [] -> Clang.Ast.node ?location Clang.Ast.Null
  | [single] -> single
  | list -> Clang.Ast.node ?location (Clang.Ast.Compound list)

let close_stmt ?location delayed_stmts stmts =
  stmt_of_stmts ?location (close_stmts delayed_stmts stmts)

let stmts_of_node (stmt, stmts) =
  stmts_of_stmt stmt |> close_stmts stmts

let close_node ((stmt, _stmts) as node) =
  let location = Clang.Ast.location_of_node stmt in
  stmt_of_stmts ~location (stmts_of_node node)

let rec name_anonymous_fields (decl : Clang.Ast.decl list) =
  match decl with
  | [] -> []
  | { desc = RecordDecl ({ keyword; name = ""; _ } as record_decl); _}
      as record ::
    ({ desc = Field ({ qual_type = { desc =
      Elaborated ({ keyword = keyword';
        named_type = { desc = Record { name = IdentifierName ""; _ }; _ }
          as named_type; _ } as elaborated); _}
      as qual_type; _ } as field_desc); _} as field)
    :: tail when keyword = keyword' ->
    { record with
      desc = Clang.Ast.RecordDecl { record_decl with name = "anon" }} ::
    { field with desc = Field { field_desc with
      qual_type = { qual_type with desc = Elaborated { elaborated with
      named_type = { named_type with desc =
        Record (Clang.Ast.identifier_name "anon") }}}}} ::
    name_anonymous_fields tail
  | hd :: tl -> hd :: name_anonymous_fields tl

(* HACK: some delayed statements seem to have a compound around
   them. *)
let delayed_in_compound = ref false

let make_conditional_operator qual_type delayed_stmts cond
    (then_branch, then_stmts) (else_branch, else_stmts) =
  let tmp_var, decl_tmp = declare_tmp qual_type in
  let assign branch =
    Clang.Ast.node (Clang.Ast.Expr (
      bin_op qual_type tmp_var Assign branch)) in
  let delayed_stmts =
    Free_monoid.plus delayed_stmts
      (Free_monoid.of_list [decl_tmp;
        Clang.Ast.node (Clang.Ast.If {
          init = None;
          condition_variable = None;
          cond;
          then_branch = close_stmt then_stmts [assign then_branch];
          else_branch =
            Some (close_stmt else_stmts [assign else_branch])})]) in
    tmp_var, delayed_stmts

let make_condition delayed_stmts cond =
  make_conditional_operator (Clang.Type.of_node cond) delayed_stmts cond
    (integer_literal 1, Free_monoid.zero) (integer_literal 0, Free_monoid.zero)

let rec condition (expr : Clang.Ast.expr) =
  match expr.desc with
  | BinaryOperator { kind = (LT | GT | LE | GE | EQ | NE | LAnd | LOr); _ }
  | UnaryOperator { kind = LNot; _ } -> true
  | Cast { operand; _ } -> condition operand
  | _ -> false

let is_integer_type (t : Clang.Ast.qual_type) =
  match t.desc with
  | BuiltinType
      (Short | UShort | Int | UInt | Long | LongLong | ULong | ULongLong) ->
      true
  | _ -> false

let rec remove_casts (expr : Clang.Ast.expr) =
  match expr with
  | { desc = Cast { kind = CStyle; operand; _ }; _ } -> remove_casts operand
  | _ -> expr

type env = {
    statement_root : bool;
    assign_rhs : bool;
    in_condition : bool;
  }

let root_env = {
  statement_root = false;
  assign_rhs = false;
  in_condition = false;
}

type accu = Clang.Ast.stmt Free_monoid.t

let plus_with_warning (a : accu) (b : accu) : accu =
  let () =
    match a, b with
    | Zero, _ | _, Zero -> ()
    | _ ->
        let line_number =
          (Clang.Ast.concrete_of_source_location Presumed
             (Clang.Ast.location_of_node (Free_monoid.hd a))).line in
        Log.warn "line %d: left to right eval" line_number in
  Free_monoid.plus a b

module Applicative =
  Traverse.Applicative.Env (struct type t = env end)
    (Traverse.Applicative.Pair
      (Traverse.Applicative.Map)
      (Traverse.Applicative.Reduce (struct
        type t = accu

        let zero = Free_monoid.zero

        let ( + ) = plus_with_warning
      end)))

module rec Visitor : Refl.Visit.VisitorS
with type 'a Applicative.t = 'a Applicative.t = struct
  module Applicative = Applicative

  let visit_compound (stmts : Clang.Ast.stmt list) env
      : Clang.Ast.stmt_desc * accu =
    Compound (List.flatten (List.map (fun (stmt : Clang.Ast.stmt) ->
      (* SplitInitialisers and simplifyDeclStmt *)
      let list =
        match stmt with
        | { desc = Decl list; _ } ->
            list |> List.map @@ fun decl ->
              { stmt with desc = Clang.Ast.Decl [decl] }
        | _ -> [stmt] in
      let list =
        List.flatten (List.map (fun (stmt : Clang.Ast.stmt) ->
        match stmt with
        | { desc = Decl [
            { desc =
                Var ({ var_name; var_type; var_init = Some init; _ } as var); _}
              as decl]; _} as stmt ->
            let decl : Clang.Ast.stmt = { stmt with desc =
              Decl [{ decl with desc = Var { var with var_init = None }}]} in
            let init, init_stmts =
              Visit.visit [%refl: Clang.Ast.expr] []
                init { env with assign_rhs = true } in
            let init : Clang.Ast.stmt = { stmt with desc =
              Expr { stmt with desc = BinaryOperator {
                lhs =
                  Clang.Ast.node ~qual_type:var_type
                    (Clang.Ast.DeclRef (Clang.Ast.identifier_name var_name));
                kind = Assign; rhs = init }}} in
            close_stmts init_stmts [decl; init]
        | { desc = Do { body; cond; _ }; _} ->
            let cond, cond_stmts =
              Visit.visit [%refl: Clang.Ast.expr] []
                cond { env with in_condition = true } in
            let body =
              close_node (Visit.visit [%refl: Clang.Ast.stmt] [] body env) in
            body :: close_stmts cond_stmts [{ stmt with desc =
               Clang.Ast.While { condition_variable = None; cond; body =
                 Clang.Ast.node (Clang.Ast.Compound (body ::
                   close_stmts cond_stmts []))}}]
        | { desc = For { init; cond; inc; body; _ }; _} ->
            let init, init_stmts =
              Visit.visit [%refl: Clang.Ast.stmt option] [] init env in
            let cond, cond_stmts =
              Visit.visit [%refl: Clang.Ast.expr option] []
                cond { env with in_condition = true } in
            let inc, inc_stmts =
              Visit.visit [%refl: Clang.Ast.stmt option] []
                inc env in
            let body =
              close_node (Visit.visit [%refl: Clang.Ast.stmt] [] body env) in
            let init =
              match init with
              | None -> []
              | Some init ->
                  close_stmts init_stmts [init] in
            let cond =
              match cond with
              | None -> integer_literal 1
              | Some cond -> cond in
            let inc =
              match inc with
              | None -> []
              | Some inc ->
                  close_stmts inc_stmts [inc] in
            let location = Clang.Ast.location_of_node stmt in
            init @ close_stmts cond_stmts
              [{ stmt with desc = While {
                condition_variable = None; cond;
                body = stmt_of_stmts ~location (stmts_of_stmt body @ inc @
                  close_stmts cond_stmts [])}}]
        (* assignCond special case!?! *)
        | { desc = Return (Some cond); _ } when condition cond ->
            let cond, delayed_stmts =
              Visit.visit [%refl: Clang.Ast.expr] []
                cond { env with in_condition = true } in
            let cond, delayed_stmts = make_condition delayed_stmts cond in
            let tmp_var, stmts = assign_to_tmp ~qual_type:int cond in
            [close_stmt (Free_monoid.plus delayed_stmts
                (Free_monoid.of_list stmts))
              [{ stmt with desc = Return (Some tmp_var) }]]
        | { desc = Expr ({ desc = Call _; _ }  as expr); _}
              when (Clang.Type.of_node expr).desc <> BuiltinType Void ->
            let expr, delayed_stmts =
              Visit.visit [%refl: Clang.Ast.expr] []
                expr { env with assign_rhs = true } in
            let _tmp_var, stmts = assign_to_tmp expr in
            close_stmts delayed_stmts stmts
        | stmt ->
            delayed_in_compound := false;
            let stmt, delayed_stmts =
              Visit.visit [%refl: Clang.Ast.stmt] [] stmt env in
            if !delayed_in_compound then
              [close_stmt delayed_stmts [stmt]]
            else
              close_stmts delayed_stmts [stmt]) list) in
      list) stmts)), Free_monoid.zero

  let visit_binary_operator env lhs
      (kind : Clang.Ast.binary_operator_kind) rhs : Clang.Ast.expr_desc * accu =
    let lhs_env, rhs_env =
      match kind with
      | Assign ->
          { env with in_condition = false },
          { env with in_condition = false; assign_rhs = true }
      | LAnd | LOr ->
          { env with in_condition = true },
          { env with in_condition = true }
      | _ -> { env with in_condition = false },
          { env with in_condition = false } in
    let lhs, lhs_value =
      Visit.visit [%refl: Clang.Ast.expr] [] lhs lhs_env in
    let rhs, rhs_value =
      Visit.visit [%refl: Clang.Ast.expr] [] rhs rhs_env in
    let delayed_stmts =
      plus_with_warning lhs_value rhs_value in
    match
      match kind with
      | Add | Sub ->
          begin match Clang.Type.of_node lhs with
          | { desc = Pointer pointee_type; _ } -> Some pointee_type
          | _ -> None
          end
      | _ -> None
    with
    | None ->
        BinaryOperator { lhs; kind; rhs }, delayed_stmts
    | Some pointee_type ->
        let size = Clang.Type.get_size_of pointee_type in
        let lhs = remove_casts lhs in
        let rhs =
          if size = 1 then rhs
          else
            { rhs with desc = BinaryOperator {
              lhs = integer_literal size;
              kind = Mul;
              rhs }} in
        BinaryOperator { lhs; kind; rhs }, delayed_stmts

  let visit_unary_operator env (kind : Clang.Ast.unary_operator_kind)
      operand : Clang.Ast.expr_desc * accu =
    let operand_env =
      match kind with
      | LNot -> { env with in_condition = true }
      | _ -> { env with in_condition = false } in
    let operand, operand_value =
      Visit.visit [%refl: Clang.Ast.expr] [] operand operand_env in
    UnaryOperator { kind; operand }, operand_value

  let rec visit_if(cond : Clang.Ast.expr) (then_branch : Clang.Ast.stmt)
      (else_branch : Clang.Ast.stmt option) env
      : Clang.Ast.stmt_desc * accu =
    match cond.desc with
    | BinaryOperator { lhs; kind = LAnd; rhs } ->
        let then_branch =
          let (desc, accu) =
            visit_if rhs then_branch else_branch env in
          close_node (Clang.Ast.node desc, accu) in
        visit_if lhs then_branch else_branch env
    | BinaryOperator { lhs; kind = LOr; rhs } ->
        let else_branch =
          let (desc, accu) =
            visit_if rhs then_branch else_branch env in
          close_node (Clang.Ast.node desc, accu) in
        visit_if lhs then_branch (Some else_branch) env
    | _ ->
        let cond, cond_stmts =
          Visit.visit [%refl: Clang.Ast.expr] []
            cond { env with in_condition = true } in
        let delayed_in_compound_save = !delayed_in_compound in
        let then_branch =
          close_node
            (Visit.visit [%refl: Clang.Ast.stmt] [] then_branch env) in
        let else_branch =
          match else_branch with
          | None -> None
          | Some else_branch ->
              Some (close_node
                (Visit.visit [%refl: Clang.Ast.stmt] [] else_branch env)) in
        delayed_in_compound := delayed_in_compound_save;
        If { init = None; condition_variable = None;
             cond; then_branch; else_branch }, cond_stmts

  let visit_while env cond body : Clang.Ast.stmt_desc * accu =
    let cond, cond_stmts =
      Visit.visit [%refl: Clang.Ast.expr] []
        cond { env with in_condition = true } in
    let body = close_node (Visit.visit [%refl: Clang.Ast.stmt] [] body env) in
    While { condition_variable = None; cond; body }, cond_stmts

  let visit_expr super (expr : Clang.Ast.expr) env
      : Clang.Ast.expr * accu =
    (* removeCast *)
    match expr.desc with
    | Cast { kind = CStyle;
        qual_type = { desc = Pointer { desc = BuiltinType Void; _ }; _};
        operand = { desc = IntegerLiteral (Int 0); _ }} ->
        expr, Free_monoid.zero
    | Cast { kind = CStyle;
        qual_type = integer_type;
        operand = { desc = UnaryOperator {
          kind = AddrOf;
          operand = { desc = Member {
            base = Some { desc = Cast {
              kind = CStyle;
              qual_type = { desc = Pointer { desc =
                Elaborated { named_type; _ }; _ }; _};
              operand = { desc = IntegerLiteral (Int 0); _ }; _}; _};
            arrow = true;
            field = FieldName { desc = {
              name = IdentifierName field_name; _ }; _}}; _}; _}; _}}
      when is_integer_type integer_type ->
        let offset = Clang.type_get_offset_of named_type.cxtype field_name in
        if offset mod 8 <> 0 then
          failwith "Bitfields are not supported!";
        integer_literal (offset / 8), Free_monoid.zero
    | Cast { operand; _ } ->
        Visit.visit [%refl: Clang.Ast.expr] []
          { expr with desc = operand.desc } env
    (* liftConditionals *)
    | ConditionalOperator { cond; then_branch; else_branch } ->
        let then_branch =
          match then_branch with
          | None -> cond
          | Some then_branch -> then_branch in
        let then_branch =
          Visit.visit [%refl: Clang.Ast.expr] []
            then_branch { env with assign_rhs = true } in
        let else_branch =
          Visit.visit [%refl: Clang.Ast.expr] []
            else_branch { env with assign_rhs = true } in
        let qual_type = Clang.Type.of_node expr in
        make_conditional_operator qual_type Free_monoid.zero cond then_branch
          else_branch
    | _ ->
    let (expr : Clang.Ast.expr), delayed_stmts =
      super expr { env with statement_root = false; assign_rhs = false } in
    let make_assign expr lhs =
      if env.statement_root then
        expr, delayed_stmts
      else
        let qual_type = Clang.Type.of_node lhs in
        let delayed_stmts =
          Free_monoid.plus delayed_stmts (Free_monoid.of_item (
            Clang.Ast.node ~qual_type (Clang.Ast.Expr expr))) in
        lhs, delayed_stmts in
    let make_op_assign lhs kind rhs =
      { expr with desc = Clang.Ast.BinaryOperator {
        lhs;
        kind = Assign;
        rhs = { lhs with desc = BinaryOperator { lhs; kind; rhs }}}} in
    match expr.desc with
    (* replaceFunCall *)
    | Call _ ->
        if env.statement_root || env.assign_rhs then
          expr, delayed_stmts
        else
          let tmp_var, stmts = assign_to_tmp expr in
          let delayed_stmts =
            Free_monoid.plus delayed_stmts
              (Free_monoid.of_list stmts) in
          delayed_in_compound := true;
          tmp_var, delayed_stmts

    (* liftAssign (should generalize breakMultiAssign) *)
    | BinaryOperator { lhs; kind = Assign; _ } ->
        make_assign expr lhs

    | BinaryOperator { lhs; kind = MulAssign; rhs } ->
        make_assign (make_op_assign lhs Mul rhs) lhs

    | BinaryOperator { lhs; kind = DivAssign; rhs } ->
        make_assign (make_op_assign lhs Div rhs) lhs

    | BinaryOperator { lhs; kind = RemAssign; rhs } ->
        make_assign (make_op_assign lhs Rem rhs) lhs

    | BinaryOperator { lhs; kind = AddAssign; rhs } ->
        make_assign (make_op_assign lhs Add rhs) lhs

    | BinaryOperator { lhs; kind = SubAssign; rhs } ->
        make_assign (make_op_assign lhs Sub rhs) lhs

    | BinaryOperator { lhs; kind = ShlAssign; rhs } ->
        make_assign (make_op_assign lhs Shl rhs) lhs

    | BinaryOperator { lhs; kind = ShrAssign; rhs } ->
        make_assign (make_op_assign lhs Shr rhs) lhs

    | BinaryOperator { lhs; kind = AndAssign; rhs } ->
        make_assign (make_op_assign lhs And rhs) lhs

    | BinaryOperator { lhs; kind = OrAssign; rhs } ->
        make_assign (make_op_assign lhs Or rhs) lhs

    | BinaryOperator { lhs; kind = XorAssign; rhs } ->
        make_assign (make_op_assign lhs Xor rhs) lhs

    | BinaryOperator { lhs; kind = Sub;
         rhs = { desc = IntegerLiteral (Int 0); _ }} ->
        lhs, Free_monoid.zero

    (* assignCond *)
    | _ when condition expr && not env.in_condition ->
        make_condition delayed_stmts expr

    (* removeCommaBinop *)
    | BinaryOperator { lhs; kind = Comma; rhs } ->
        let _tmp_var, stmts = assign_to_tmp lhs in
        let delayed_stmts =
          Free_monoid.plus delayed_stmts
            (Free_monoid.of_list stmts) in
        rhs, delayed_stmts

    (* preIncrDecr *)
    | UnaryOperator { kind = ((PreInc | PreDec) as kind); operand } ->
        let kind : Clang.Ast.binary_operator_kind =
          match kind with
          | PreInc -> Add
          | PreDec -> Sub
          | _ -> assert false in
        make_op_assign operand kind (integer_literal 1),
        delayed_stmts

    (* postIncrDecr *)
    | UnaryOperator { kind = ((PostInc | PostDec) as kind); operand } ->
        let kind : Clang.Ast.binary_operator_kind =
          match kind with
          | PostInc -> Add
          | PostDec -> Sub
          | _ -> assert false in
        let qual_type = Clang.Type.of_node operand in
        let tmp_var, stmts = assign_to_tmp operand in
        let increment_operand = Clang.Ast.node ~qual_type (Clang.Ast.Expr (
          make_op_assign operand kind (integer_literal 1))) in
        let delayed_stmts =
          plus_with_warning delayed_stmts
            (Free_monoid.of_list (stmts @ [increment_operand])) in
        tmp_var, delayed_stmts

    | _ -> expr, delayed_stmts

  let visit_unary_expr_size_of env expr_or_trait
      : Clang.Ast.expr_desc * accu =
    let expr_or_trait, delayed_stmts =
      Visit.visit [%refl: Clang.Ast.expr_or_type] []
        env expr_or_trait in
    let ty =
      match expr_or_trait with
      | ArgumentExpr e -> Clang.Type.of_node e
      | ArgumentType ty -> ty in
    IntegerLiteral (Clang.Ast.literal_of_int
      (Clang.Type.get_size_of ty)), delayed_stmts

  let visit_switch env cond body : Clang.Ast.stmt_desc * accu =
    let cond, cond_delayed_stmts =
      Visit.visit [%refl: Clang.Ast.expr] [] cond env in
    let body =
      stmts_of_node (Visit.visit [%refl: Clang.Ast.stmt] [] body env) in
    let cond_var, cond_stmts = assign_to_tmp cond in
    let from_previous_var, from_previous_stmts =
      assign_to_tmp (integer_literal 0) in
    let close_case accu current_case accu_current =
      match current_case, accu_current with
      | None, [] -> accu
      | Some current_case, _ -> (current_case, List.rev accu_current) :: accu
      | _ -> failwith "close_case" in
    let rec split_cases accu current_case accu_current
        (stmts : Clang.Ast.stmt list) =
      match stmts with
      | [] -> List.rev (close_case accu current_case accu_current)
      | { desc = Default stmt; _ } :: tl ->
          let accu = close_case accu current_case accu_current in
          split_cases accu (Some None) [] (stmt :: tl)
      | { desc = Case { lhs; body; _ }; _ } :: tl ->
          let accu = close_case accu current_case accu_current in
          split_cases accu (Some (Some lhs)) [] (body :: tl)
      | hd :: tl -> split_cases accu current_case (hd :: accu_current) tl in
    let cases = split_cases [] None [] body in
    let cases = cases |> List.map @@ fun (value, stmts) ->
      let cond =
        match value with
        | Some value -> bin_op int cond_var EQ value
        | None ->
            let values = List.flatten (List.map (fun (value, _) ->
              match value with
              | None -> []
              | Some value -> [bin_op int cond_var NE value]) cases) in
            match values with
            | [] -> failwith "no case in switch"
            | hd :: tl ->
                List.fold_left (fun a b -> bin_op int a LAnd b) hd tl in
      let cond =
        bin_op int from_previous_var LOr cond in
      let assign var value =
        Clang.Ast.node (Clang.Ast.Expr (
          bin_op int var Assign (integer_literal value))) in
      let stmts =
        match cut_break stmts with
        | None -> assign from_previous_var 1 :: stmts
        | Some stmts -> assign from_previous_var 0 :: stmts in
      Clang.Ast.node (Clang.Ast.If { init = None; condition_variable = None;
        cond; then_branch = Clang.Ast.node (Clang.Ast.Compound stmts);
        else_branch = None }) in
    Compound (close_stmts cond_delayed_stmts (
      cond_stmts @ from_previous_stmts @ cases)),
    Free_monoid.zero

  (* nameAnonymousTypes *)
  let visit_record_decl (record_decl : Clang.Ast.record_decl)
      : Clang.Ast.decl_desc * accu =
    RecordDecl
      { record_decl with fields = name_anonymous_fields record_decl.fields },
    Free_monoid.zero

  let hook :
    type a . a Refl.refl -> (a -> env -> a * accu) -> (a -> env -> a * accu) =
  fun refl super x env ->
    match refl with
    | Clang.Ast.Refl_stmt_desc ->
        begin match x with
        | Compound stmts -> visit_compound stmts env
        | If { cond; then_branch; else_branch; _ } ->
            visit_if cond then_branch else_branch env
        | While { cond; body; _ } -> visit_while env cond body
        | Expr _ -> super x { env with statement_root = true }
        | Switch { cond; body; _ } -> visit_switch env cond body
        | _ -> super x env
        end
    | Clang.Ast.Refl_expr_desc ->
        begin match x with
        | BinaryOperator { lhs; kind; rhs } ->
            visit_binary_operator env lhs kind rhs
        | UnaryOperator { kind; operand } ->
            visit_unary_operator env kind operand
        | Call {
            callee = { desc = DeclRef { name = IdentifierName "assert"; _ }; _ }
              as callee;
            args = [cond]} ->
            let cond, cond_stmts =
                Visit.visit [%refl: Clang.Ast.expr] []
                  cond { env with in_condition = true } in
              Call { callee; args = [cond] }, cond_stmts
          | UnaryExpr { kind = SizeOf; argument } ->
              visit_unary_expr_size_of argument env
          | _ -> super x env
        end
    | Clang.Ast.Refl_decl_desc ->
        begin match x with
        | RecordDecl record_decl ->
            visit_record_decl record_decl
        | _ -> super x env
        end
    | Clang.Ast.Refl_expr ->
        visit_expr super x env
    | _ ->
        super x env
end
and Visit : Refl.Visit.VisitS with
type 'a Visitor.t = 'a -> 'a Applicative.t =
    Refl.Visit.Make (Visitor)

(*

let transform = object (self)
  inherit [_] Clangml_visitors.mapreduce as super
  inherit [_] Free_monoid.free_monoid as monoid

  method! visit_Compound env (stmts : Clang.Ast.stmt list) =
    Compound (List.flatten (List.map (fun (stmt : Clang.Ast.stmt) ->
      (* SplitInitialisers and simplifyDeclStmt *)
      let list =
        match stmt with
        | { desc = Decl list; _ } ->
            list |> List.map @@ fun decl ->
              { stmt with desc = Clang.Ast.Decl [decl] }
        | _ -> [stmt] in
      let list =
        List.flatten (List.map (fun (stmt : Clang.Ast.stmt) ->
        match stmt with
        | { desc = Decl [
            { desc =
                Var ({ var_name; var_type; var_init = Some init; _ } as var); _}
              as decl]; _} as stmt ->
            let decl : Clang.Ast.stmt = { stmt with desc =
              Decl [{ decl with desc = Var { var with var_init = None }}]} in
            let init, init_stmts =
              self#visit_expr { env with assign_rhs = true } init in
            let init : Clang.Ast.stmt = { stmt with desc =
              Expr { stmt with desc = BinaryOperator {
                lhs =
                  Clang.Ast.node ~qual_type:var_type
                    (Clang.Ast.DeclRef (Clang.Ast.identifier_name var_name));
                kind = Assign; rhs = init }}} in
            close_stmts init_stmts [decl; init]
        | { desc = Do { body; cond; _ }; _} ->
            let cond, cond_stmts =
              self#visit_expr { env with in_condition = true } cond in
            let body = close_node (self#visit_stmt env body) in
            body :: close_stmts cond_stmts [{ stmt with desc =
               Clang.Ast.While { condition_variable = None; cond; body =
                 Clang.Ast.node (Clang.Ast.Compound (body ::
                   close_stmts cond_stmts []))}}]
        | { desc = For { init; cond; inc; body; _ }; _} ->
            let init, init_stmts = self#visit_option self#visit_stmt env init in
            let cond, cond_stmts =
              self#visit_option self#visit_expr
                { env with in_condition = true } cond in
            let inc, inc_stmts = self#visit_option self#visit_stmt env inc in
            let body = close_node (self#visit_stmt env body) in
            let init =
              match init with
              | None -> []
              | Some init ->
                  close_stmts init_stmts [init] in
            let cond =
              match cond with
              | None -> integer_literal 1
              | Some cond -> cond in
            let inc =
              match inc with
              | None -> []
              | Some inc ->
                  close_stmts inc_stmts [inc] in
            let location = Clang.Ast.location_of_node stmt in
            init @ close_stmts cond_stmts
              [{ stmt with desc = While {
                condition_variable = None; cond;
                body = stmt_of_stmts ~location (stmts_of_stmt body @ inc @
                  close_stmts cond_stmts [])}}]
        (* assignCond special case!?! *)
        | { desc = Return (Some cond); _ } when condition cond ->
            let cond, delayed_stmts =
              self#visit_expr { env with in_condition = true } cond in
            let cond, delayed_stmts = make_condition delayed_stmts cond in
            let tmp_var, stmts = assign_to_tmp ~qual_type:int cond in
            [close_stmt (Free_monoid.plus delayed_stmts
                (Free_monoid.of_list stmts))
              [{ stmt with desc = Return (Some tmp_var) }]]
        | { desc = Expr ({ desc = Call _; _ }  as expr); _}
              when (Clang.Type.of_node expr).desc <> BuiltinType Void ->
            let expr, delayed_stmts =
              self#visit_expr { env with assign_rhs = true } expr in
            let _tmp_var, stmts = assign_to_tmp expr in
            close_stmts delayed_stmts stmts
        | stmt ->
            delayed_in_compound := false;
            let stmt, delayed_stmts = self#visit_stmt env stmt in
            if !delayed_in_compound then
              [close_stmt delayed_stmts [stmt]]
            else
              close_stmts delayed_stmts [stmt]) list) in
      list) stmts)), monoid#zero

  method! visit_Expr env expr =
    super#visit_Expr { env with statement_root = true } expr

  method! visit_BinaryOperator env lhs kind rhs =
    let lhs_env, rhs_env =
      match kind with
      | Assign ->
          { env with in_condition = false },
          { env with in_condition = false; assign_rhs = true }
      | LAnd | LOr ->
          { env with in_condition = true },
          { env with in_condition = true }
      | _ -> { env with in_condition = false },
          { env with in_condition = false } in
    let lhs, lhs_value = self#visit_expr lhs_env lhs in
    let kind, kind_value = self#visit_binary_operator_kind env kind in
    let rhs, rhs_value = self#visit_expr rhs_env rhs in
    let delayed_stmts =
      monoid#plus lhs_value (monoid#plus kind_value rhs_value) in
    match
      match kind with
      | Add | Sub ->
          begin match Clang.Type.of_node lhs with
          | { desc = Pointer pointee_type; _ } -> Some pointee_type
          | _ -> None
          end
      | _ -> None
    with
    | None ->
        BinaryOperator { lhs; kind; rhs }, delayed_stmts
    | Some pointee_type ->
        let size = Clang.Type.get_size_of pointee_type in
        let lhs = remove_casts lhs in
        let rhs =
          if size = 1 then rhs
          else
            { rhs with desc = BinaryOperator {
              lhs = integer_literal size;
              kind = Mul;
              rhs }} in
        BinaryOperator { lhs; kind; rhs }, delayed_stmts

  method! visit_UnaryOperator env kind operand =
    let operand_env =
      match kind with
      | LNot -> { env with in_condition = true }
      | _ -> { env with in_condition = false } in
    let kind, kind_value = self#visit_unary_operator_kind env kind in
    let operand, operand_value = self#visit_expr operand_env operand in
    UnaryOperator { kind; operand }, monoid#plus kind_value operand_value

  method! visit_stmt env stmt =
    let rec simplify_if (cond : Clang.Ast.expr) then_branch else_branch =
      match cond.desc with
      | BinaryOperator { lhs; kind = LAnd; rhs } ->
          let then_branch =
            close_node (simplify_if rhs then_branch else_branch) in
          simplify_if lhs then_branch else_branch
      | BinaryOperator { lhs; kind = LOr; rhs } ->
          let else_branch =
            close_node (simplify_if rhs then_branch else_branch) in
          simplify_if lhs then_branch (Some else_branch)
      | _ ->
          let cond, cond_stmts =
            self#visit_expr { env with in_condition = true } cond in
          let delayed_in_compound_save = !delayed_in_compound in
          let then_branch= close_node (self#visit_stmt env then_branch) in
          let else_branch =
            match else_branch with
            | None -> None
            | Some else_branch ->
                Some (close_node (self#visit_stmt env else_branch)) in
          delayed_in_compound := delayed_in_compound_save;
          { stmt with desc = If { init = None; condition_variable = None;
               cond; then_branch; else_branch }}, cond_stmts in
    match stmt.desc with
    | If { cond; then_branch; else_branch; _ } ->
        simplify_if cond then_branch else_branch
    | _ ->
        super#visit_stmt env stmt

  method! visit_While env _condition_variable cond body =
    let cond, cond_stmts =
      self#visit_expr { env with in_condition = true } cond in
    let body = close_node (self#visit_stmt env body) in
    While { condition_variable = None; cond; body },
    cond_stmts

  method! visit_Call env callee args =
    match callee, args with
    | { desc = DeclRef { name = IdentifierName "assert"; _ }; _ }, [cond] ->
        let cond, cond_stmts =
          self#visit_expr { env with in_condition = true } cond in
        Call { callee; args = [cond] }, cond_stmts
    | _ -> super#visit_Call env callee args

  method! visit_expr env expr =
    (* removeCast *)
    match expr.desc with
    | Cast { kind = CStyle;
        qual_type = { desc = Pointer { desc = BuiltinType Void; _ }; _};
        operand = { desc = IntegerLiteral (Int 0); _ }} ->
        expr, monoid#zero
    | Cast { kind = CStyle;
        qual_type = integer_type;
        operand = { desc = UnaryOperator {
          kind = AddrOf;
          operand = { desc = Member {
            base = Some { desc = Cast {
              kind = CStyle;
              qual_type = { desc = Pointer { desc =
                Elaborated { named_type; _ }; _ }; _};
              operand = { desc = IntegerLiteral (Int 0); _ }; _}; _};
            arrow = true;
            field = FieldName { desc = {
              name = IdentifierName field_name; _ }; _}}; _}; _}; _}}
      when is_integer_type integer_type ->
        let offset = Clang.type_get_offset_of named_type.cxtype field_name in
        if offset mod 8 <> 0 then
          failwith "Bitfields are not supported!";
        integer_literal (offset / 8), monoid#zero
    | Cast { operand; _ } ->
        self#visit_expr env { expr with desc = operand.desc }
    (* liftConditionals *)
    | ConditionalOperator { cond; then_branch; else_branch } ->
        let then_branch =
          match then_branch with
          | None -> cond
          | Some then_branch -> then_branch in
        let then_branch =
          self#visit_expr { env with assign_rhs = true } then_branch in
        let else_branch =
          self#visit_expr { env with assign_rhs = true } else_branch in
        let qual_type = Clang.Type.of_node expr in
        make_conditional_operator qual_type monoid#zero cond then_branch
          else_branch
    | _ ->
    let expr, delayed_stmts =
      super#visit_expr { env with statement_root = false; assign_rhs = false }
        expr in
    let make_assign expr lhs =
      if env.statement_root then
        expr, delayed_stmts
      else
        let qual_type = Clang.Type.of_node lhs in
        let delayed_stmts =
          monoid#plus delayed_stmts (Free_monoid.of_item (
            Clang.Ast.node ~qual_type (Clang.Ast.Expr expr))) in
        lhs, delayed_stmts in
    let make_op_assign lhs kind rhs =
      { expr with desc = Clang.Ast.BinaryOperator {
        lhs;
        kind = Assign;
        rhs = { lhs with desc = BinaryOperator { lhs; kind; rhs }}}} in
    match expr.desc with
    (* replaceFunCall *)
    | Call _ ->
        if env.statement_root || env.assign_rhs then
          expr, delayed_stmts
        else
          let tmp_var, stmts = assign_to_tmp expr in
          let delayed_stmts =
            monoid#plus delayed_stmts
              (Free_monoid.of_list stmts) in
          delayed_in_compound := true;
          tmp_var, delayed_stmts

    (* liftAssign (should generalize breakMultiAssign) *)
    | BinaryOperator { lhs; kind = Assign; _ } ->
        make_assign expr lhs

    | BinaryOperator { lhs; kind = MulAssign; rhs } ->
        make_assign (make_op_assign lhs Mul rhs) lhs

    | BinaryOperator { lhs; kind = DivAssign; rhs } ->
        make_assign (make_op_assign lhs Div rhs) lhs

    | BinaryOperator { lhs; kind = RemAssign; rhs } ->
        make_assign (make_op_assign lhs Rem rhs) lhs

    | BinaryOperator { lhs; kind = AddAssign; rhs } ->
        make_assign (make_op_assign lhs Add rhs) lhs

    | BinaryOperator { lhs; kind = SubAssign; rhs } ->
        make_assign (make_op_assign lhs Sub rhs) lhs

    | BinaryOperator { lhs; kind = ShlAssign; rhs } ->
        make_assign (make_op_assign lhs Shl rhs) lhs

    | BinaryOperator { lhs; kind = ShrAssign; rhs } ->
        make_assign (make_op_assign lhs Shr rhs) lhs

    | BinaryOperator { lhs; kind = AndAssign; rhs } ->
        make_assign (make_op_assign lhs And rhs) lhs

    | BinaryOperator { lhs; kind = OrAssign; rhs } ->
        make_assign (make_op_assign lhs Or rhs) lhs

    | BinaryOperator { lhs; kind = XorAssign; rhs } ->
        make_assign (make_op_assign lhs Xor rhs) lhs

    | BinaryOperator { lhs; kind = Sub;
         rhs = { desc = IntegerLiteral (Int 0); _ }} ->
        lhs, monoid#zero

    (* assignCond *)
    | _ when condition expr && not env.in_condition ->
        make_condition delayed_stmts expr

    (* removeCommaBinop *)
    | BinaryOperator { lhs; kind = Comma; rhs } ->
        let _tmp_var, stmts = assign_to_tmp lhs in
        let delayed_stmts =
          monoid#plus delayed_stmts
            (Free_monoid.of_list stmts) in
        rhs, delayed_stmts

    (* preIncrDecr *)
    | UnaryOperator { kind = ((PreInc | PreDec) as kind); operand } ->
        let kind : Clang.Ast.binary_operator_kind =
          match kind with
          | PreInc -> Add
          | PreDec -> Sub
          | _ -> assert false in
        make_op_assign operand kind (integer_literal 1),
        delayed_stmts

    (* postIncrDecr *)
    | UnaryOperator { kind = ((PostInc | PostDec) as kind); operand } ->
        let kind : Clang.Ast.binary_operator_kind =
          match kind with
          | PostInc -> Add
          | PostDec -> Sub
          | _ -> assert false in
        let qual_type = Clang.Type.of_node operand in
        let tmp_var, stmts = assign_to_tmp operand in
        let increment_operand = Clang.Ast.node ~qual_type (Clang.Ast.Expr (
          make_op_assign operand kind (integer_literal 1))) in
        let delayed_stmts =
          monoid#plus delayed_stmts
            (Free_monoid.of_list (stmts @ [increment_operand])) in
        tmp_var, delayed_stmts

    | _ -> expr, delayed_stmts

  method! visit_UnaryExpr env kind expr_or_trait =
    match kind with
    | SizeOf ->
        begin
          let expr_or_trait, delayed_stmts =
            super#visit_unary_expr_or_type_trait env expr_or_trait in
          let ty =
            match expr_or_trait with
            | ArgumentExpr e -> Clang.Type.of_node e
            | ArgumentType ty -> ty in
          IntegerLiteral (Clang.Ast.literal_of_int
            (Clang.Type.get_size_of ty)), delayed_stmts
        end
    | _ -> super#visit_UnaryExpr env kind expr_or_trait

  method! visit_Switch env _init _condition_variable cond body =
    let cond, cond_delayed_stmts = self#visit_expr env cond in
    let body = stmts_of_node (self#visit_stmt env body) in
    let cond_var, cond_stmts = assign_to_tmp cond in
    let from_previous_var, from_previous_stmts =
      assign_to_tmp (integer_literal 0) in
    let close_case accu current_case accu_current =
      match current_case, accu_current with
      | None, [] -> accu
      | Some current_case, _ -> (current_case, List.rev accu_current) :: accu
      | _ -> failwith "close_case" in
    let rec split_cases accu current_case accu_current
        (stmts : Clang.Ast.stmt list) =
      match stmts with
      | [] -> List.rev (close_case accu current_case accu_current)
      | { desc = Default stmt; _ } :: tl ->
          let accu = close_case accu current_case accu_current in
          split_cases accu (Some None) [] (stmt :: tl)
      | { desc = Case { lhs; body; _ }; _ } :: tl ->
          let accu = close_case accu current_case accu_current in
          split_cases accu (Some (Some lhs)) [] (body :: tl)
      | hd :: tl -> split_cases accu current_case (hd :: accu_current) tl in
    let cases = split_cases [] None [] body in
    let cases = cases |> List.map @@ fun (value, stmts) ->
      let cond =
        match value with
        | Some value -> bin_op int cond_var EQ value
        | None ->
            let values = List.flatten (List.map (fun (value, _) ->
              match value with
              | None -> []
              | Some value -> [bin_op int cond_var NE value]) cases) in
            match values with
            | [] -> failwith "no case in switch"
            | hd :: tl ->
                List.fold_left (fun a b -> bin_op int a LAnd b) hd tl in
      let cond =
        bin_op int from_previous_var LOr cond in
      let assign var value =
        Clang.Ast.node (Clang.Ast.Expr (
          bin_op int var Assign (integer_literal value))) in
      let stmts =
        match cut_break stmts with
        | None -> assign from_previous_var 1 :: stmts
        | Some stmts -> assign from_previous_var 0 :: stmts in
      Clang.Ast.node (Clang.Ast.If { init = None; condition_variable = None;
        cond; then_branch = Clang.Ast.node (Clang.Ast.Compound stmts);
        else_branch = None }) in
    Compound (close_stmts cond_delayed_stmts (
      cond_stmts @ from_previous_stmts @ cases)),
    monoid#zero

  (* nameAnonymousTypes *)
  method! visit_record_decl _env record_decl =
    { record_decl with fields = name_anonymous_fields record_decl.fields },
    monoid#zero
end
*)

let transform_decl decl =
  let (decl, delayed_stmts) =
    Visit.visit [%refl: Clang.Ast.decl] [] decl root_env in
  assert (delayed_stmts = Free_monoid.zero);
  decl
