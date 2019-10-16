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

type transform_env = {
    statement_root : bool;
    assign_rhs : bool;
    in_condition : bool;
  }

let transform_default_env = {
  statement_root = false;
  assign_rhs = false;
  in_condition = false;
}

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
    ({ desc = Field { name; qual_type = { desc =
      Elaborated ({ keyword = keyword';
        named_type = { desc = Record { name = IdentifierName ""; _ }; _ }
          as named_type; _ } as elaborated); _}
      as qual_type; bitwidth; init }; _} as field)
    :: tail when keyword = keyword' ->
    { record with
      desc = Clang.Ast.RecordDecl { record_decl with name = "anon" }} ::
    { field with desc = Field { name;  bitwidth; init;
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

let transform = object (self)
  inherit [_] Clangml_visitors.mapreduce as super
  inherit [_] Free_monoid.free_monoid as monoid

  method! plus a b =
    let () =
      match a, b with
      | Zero, _ | _, Zero -> ()
      | _ ->
          let line_number =
            (Clang.Ast.concrete_of_source_location Presumed
               (Clang.Ast.location_of_node (Free_monoid.hd a))).line in
          Log.warn "line %d: left to right eval" line_number in
    monoid#plus a b

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
    BinaryOperator { lhs; kind; rhs },
    monoid#plus lhs_value (monoid#plus kind_value rhs_value)

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

let transform_decl decl =
  let decl, delayed_stmts = transform#visit_decl transform_default_env decl in
  assert (delayed_stmts = Free_monoid.zero);
  decl
