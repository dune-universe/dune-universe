let check_pattern (quoter : 'obj -> Ppxlib.expression) (x : 'obj)
    (pattern : ('obj, 'a) Pattern.matcher) =
  match pattern ~quoted:(quoter x) x with
  | Ok result -> result
  | Error failure ->
      Format.fprintf Format.err_formatter "%a@."
        Pattern.pp_failure failure;
      failwith "check_pattern_expr"

let check_pattern_expr expr pattern =
  check_pattern (Refl.Lift.Exp.lift [%refl: Clang.Ast.expr] []) expr pattern

let check_pattern_decl decl pattern =
  check_pattern (Refl.Lift.Exp.lift [%refl: Clang.Ast.decl] []) decl pattern

let check_pattern_tu tu pattern =
  check_pattern (Refl.Lift.Exp.lift [%refl: Clang.Ast.translation_unit] []) tu
    pattern

let parse_string ?(command_line_args = []) ?options s =
  let command_line_args =
    Clang.Command_line.language CXX ::
    List.map Clang.Command_line.include_directory
      (Clang.default_include_directories ()) @
    command_line_args in
  let ast = Clang.Ast.parse_string ~command_line_args ?options s in
  let tu = Clang.Ast.cursor_of_node ast |> Clang.cursor_get_translation_unit in
  Clang.format_diagnostics Clang.warning_or_error Format.err_formatter tu
    ~pp:begin fun pp fmt () ->
      Format.fprintf fmt "@[Compiling:@ %s@]%a@." s pp ()
    end;
  assert (not (Clang.has_severity Clang.error tu));
  ast

(* 2.13.6 Boolean literals *)

let () =
  let ast = parse_string {|
    bool f = false;
    bool t = true;
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      bool f = [%bool? f];
      bool t = [%bool? t];
    |}]] in
  check_pattern_expr bindings#f [%pattern? {
    desc = BoolLiteral false }];
  check_pattern_expr bindings#t [%pattern? {
    desc = BoolLiteral true }]


(* 2.13.7 Pointer literals *)

let () =
  let ast = parse_string
      ~command_line_args:[Clang.Command_line.standard Cxx11] {|
    void *p = nullptr;
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      void *p = [%(void *)? p];
    |}]] in
  check_pattern_expr bindings#p [%pattern? {
    desc = NullPtrLiteral }]

(* 5.1.2 This *)

let () =
  let ast = parse_string {|
    class Outer {
      unsigned int sz = sizeof(*this);
      void f() {
        int b[sizeof(*this)];
      }
    };
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      class Outer {
        unsigned int sz = [%int? sz];
        void f() {
          [%decl? d]
        }
      };
    |}]] in
  check_pattern_expr bindings#sz [%pattern? {
    desc = UnaryExpr {
      kind = SizeOf;
      argument = ArgumentExpr { desc = UnaryOperator {
        kind = Deref;
        operand = { desc = This }}}}}];
  check_pattern_decl bindings#d [%pattern? {
    desc = Var {
      var_name = "b";
      var_type = { desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 4 }}}}]

(* 5.1.5 Lambda expressions *)

(* C++ 17: auto in function return type *)

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
      ~command_line_args:[Clang.Command_line.standard Cxx17] {|
      void f() {
        auto x1 = [](int i){ return i; };
        int j;
        auto x3 = [=]()->auto&& { return j; };
      }
    |} in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++17") {|
        void f() {
          auto x1 = [%(void *(*)())? x1];
          int j;
          auto x3 = [%(void *(*)())? x3];
        }
      |}]] in
    check_pattern_expr bindings#x1 [%pattern? {
      desc = Lambda {
        capture_default = CaptureNone;
        captures = [];
        parameters = Some [{
          desc = {
            qual_type = { desc = BuiltinType Int };
            name = "i"; }}];
        result_type = None;
        body = { desc = Compound [{ desc =
          Return (Some { desc = DeclRef ({ name = IdentifierName "i" })})}]}}}];
    check_pattern_expr bindings#x3 [%pattern? {
      desc = Lambda {
        capture_default = ByCopy;
        captures = [{
          capture_kind = ByCopy;
          implicit = true;
          captured_var_name = Some "j";
          pack_expansion = false; }];
        parameters = Some [];
        result_type =
          Some { desc = RValueReference { desc = Auto }};
        body = { desc = Compound [{ desc =
          Return (Some { desc = DeclRef ({ name = IdentifierName "j" })})}]}}}]
  end]

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
        ~command_line_args:[Clang.Command_line.standard Cxx17] "
      template<class... Args>
      void f(Args... args) {
        auto lm = [&, args...] { return g(args...); };
        lm();
      }
    " in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++17") {|
        template<class... Args>
        void f(Args... args) {
          auto lm = [%(void *(*)())? lm];
          lm();
        }
      |}]] in
    check_pattern_expr bindings#lm [%pattern? {
      desc = Lambda {
        capture_default = ByRef;
        captures = [{
          capture_kind = ByCopy;
          implicit = false;
          captured_var_name = Some "args";
          pack_expansion = true; }];
        parameters = None;
        result_type = None;
        body = { desc = Compound [{ desc =
          Return (Some { desc = Call {
            callee = { desc = DeclRef ({ name = IdentifierName "g" })};
            args = [{ desc = PackExpansionExpr {
              desc = DeclRef ({ name = IdentifierName "args" })}}]}})}]}}}]
  end]

(* 5.1.6 Fold expressions *)

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
        ~command_line_args:[Clang.Command_line.standard Cxx17] {|
      template<typename ...Args>
      bool f(Args ...args) {
        return (true && ... && args);
      }
    |} in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++17") {|
        template<typename ...Args>
        bool f(Args ...args) {
          return [%bool? fold];
        }
      |}]] in
    check_pattern_expr bindings#fold [%pattern? {
      desc = Fold {
        lhs = Some { desc = BoolLiteral true };
        operator = LAnd;
        rhs = Some { desc = DeclRef ({ name = IdentifierName "args" }) }}}]
  end]

(* 5.2.8 Type identification *)

let filter_code (ast : Clang.Translation_unit.t) : Clang.Decl.t list =
  ast.desc.items |> List.filter
    (fun (decl : Clang.Decl.t) ->
      (Clang.Ast.concrete_of_source_location Presumed
        (Clang.Ast.location_of_node decl)).filename = "<string>.c")

let () =
  let ast = parse_string {|
    #include <typeinfo>
    class D { public: D() {} };
    D d1;
    const D d2;

    void f() {
      typeid(d1) == typeid(d2);
      typeid(D) == typeid(const D);
      typeid(D) == typeid(d2);
      typeid(D) == typeid(const D&);
    }
  |} in
  match List.rev (filter_code ast) with
  | f :: _ ->
    let bindings =
      check_pattern_decl f [%pattern? [%cpp-decl {|
        void f() {
          [%bool? e1];
          [%bool? e2];
          [%bool? e3];
          [%bool? e4];
        }
      |}]] in
    check_pattern_expr bindings#e1 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef { name = OperatorName EqualEqual }};
        args = [
          { desc = Typeid (ArgumentExpr { desc = DeclRef ({ name = IdentifierName "d1" })})};
          { desc = Typeid (ArgumentExpr { desc = DeclRef ({ name = IdentifierName "d2" })})}]}}];
    check_pattern_expr bindings#e2 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef { name = OperatorName EqualEqual }};
        args = [
          { desc = Typeid (ArgumentType { desc = Record ({ name = IdentifierName "D" })})};
          { desc = Typeid (ArgumentType { desc = Record ({ name = IdentifierName "D" })})}]}}];
    check_pattern_expr bindings#e3 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef { name = OperatorName EqualEqual }};
        args = [
          { desc = Typeid (ArgumentType { desc = Record ({ name = IdentifierName "D" })})};
          { desc = Typeid (ArgumentExpr { desc = DeclRef ({ name = IdentifierName "d2" })})}]}}];
    check_pattern_expr bindings#e4 [%pattern? {
      desc = Call {
        callee = { desc = DeclRef { name = OperatorName EqualEqual }};
        args = [
          { desc = Typeid (ArgumentType { desc = Record ({ name = IdentifierName "D" })})};
          { desc = Typeid (ArgumentType { desc = LValueReference {
              desc = Record ({ name = IdentifierName "D" })}})}]}}]
  | _ -> assert false

(* 5.3.3 Sizeof *)

let () =
  let ast = parse_string
      ~command_line_args:[Clang.Command_line.standard Cxx11] {|
    #include <cstdlib>

    template<class... Types>
    struct count {
      static const std::size_t value = sizeof...(Types);
    };
  |} in
  match List.rev (filter_code ast) with
  | d :: _ ->
    let bindings =
      check_pattern_decl d [%pattern? [%cpp-decl (standard "c++11")
      {|
        #include <cstdlib>
      |} {|
        template<class... Types>
        struct count {
          static const std::size_t value = [%(std::size_t)? sz];
        };
      |}]] in
    check_pattern_expr bindings#sz [%pattern? {
      desc = SizeOfPack ({ name = IdentifierName "Types" })}]
  | _ -> assert false

(* 5.3.4 New *)

let () =
  let ast = parse_string
    ~command_line_args:[Clang.Command_line.standard Cxx11] {|
    void f() {
      new auto(1);
      auto x = new auto('a');
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++11") {|
      void f() {
        [%(void *)? n1];
        auto x = [%(void *)? n2];
      }
    |}]] in
  check_pattern_expr bindings#n1 [%pattern? {
    desc = New {
      qual_type = { desc = Auto };
      init = Some { desc = IntegerLiteral (Int 1) }}}];
  check_pattern_expr bindings#n2 [%pattern? {
    desc = New {
      qual_type = { desc = Auto };
      init = Some { desc = CharacterLiteral { kind = Ascii; value = 97 }}}}]

let () =
  let ast = parse_string {|
    void f() {
      new (int (*[10])());
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++11") {|
      void f() {
        [%(void *)? n];
      }
    |}]] in
  check_pattern_expr bindings#n [%pattern? {
    desc = New {
      qual_type = { desc = Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some {
          non_variadic = [];
          variadic = false; }}}};
      init = None}}]

let () =
  let ast = parse_string {|
    struct T {
      static void* operator new(unsigned long sz);
      static void* operator new(unsigned long sz, int arg, void (*f)());
      static void* operator new[](unsigned long sz);
      static void* operator new[](unsigned long sz, int arg, void (*f)());
    };
    void f() {}
    void g() {
      new T;
      new(2,f) T;
      new T[5];
      new(2,f) T[5];
    }
  |} in
  match List.rev ast.desc.items with
  | d :: _ ->
    let bindings =
      check_pattern_decl d [%pattern? [%cpp-decl {|
        void g() {
          [%(void *)? n1];
          [%(void *)? n2];
          [%(void *)? n3];
          [%(void *)? n4];
        }
      |}]] in
    check_pattern_expr bindings#n1 [%pattern? {
      desc = New {
        placement_args = [];
        qual_type = { desc = Record ({ name = IdentifierName "T" })};
        array_size = None;
        init = None }}];
    check_pattern_expr bindings#n2 [%pattern? {
      desc = New {
        placement_args = [
          { desc = IntegerLiteral (Int 2)};
          { desc = DeclRef ({ name = IdentifierName "f" }) }];
        qual_type = { desc = Record ({ name = IdentifierName "T" })};
        array_size = None;
        init = None }}];
    check_pattern_expr bindings#n3 [%pattern? {
      desc = New {
        placement_args = [];
        qual_type = { desc = Record ({ name = IdentifierName "T" })};
        array_size = Some { desc = IntegerLiteral (Int 5) };
        init = None }}];
    check_pattern_expr bindings#n4 [%pattern? {
      desc = New {
        placement_args = [
          { desc = IntegerLiteral (Int 2)};
          { desc = DeclRef ({ name = IdentifierName "f" }) }];
        qual_type = { desc = Record ({ name = IdentifierName "T" })};
        array_size = Some { desc = IntegerLiteral (Int 5) };
        init = None }}]
  | _ -> assert false

(* 5.5 Pointer-to-member operators *)

let () =
  let ast = parse_string {|
    struct S {
      S() : i(0) { }
      mutable int i;
    };

    void f() {
      const S cs;
      int S::* pm = &S::i;
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      struct S {
        S() : i(0) { }
        mutable int i;
      };

      void f() {
        const S cs;
        [%decl? d]
      }
    |}]] in
  check_pattern_decl bindings#d [%pattern? {
    desc = Var {
      var_type = { desc = MemberPointer {
        class_ = { desc = Record ({ name = IdentifierName "S" })};
        pointee = { desc = BuiltinType Int }}};
      var_name = "pm";
      var_init = Some { desc = UnaryOperator {
        kind = AddrOf;
        operand = {
          desc = DeclRef {
            nested_name_specifier = Some [
              TypeSpec { desc = Record { name = IdentifierName "S" }}];
            name = IdentifierName "i" }}}}}}]

let () =
  let ast = parse_string {|
    struct S {
      void f(int) const;
      S() {};
    };
    void f() {
      const S cs;
      const S* ptr_to_obj = &cs;
      void (S::*ptr_to_mfct)(int) const = &S::f;
      (ptr_to_obj->*ptr_to_mfct)(10);
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      struct S {
        void f(int) const;
        S() {};
      };
      void f() {
        const S cs;
        const S* ptr_to_obj = &cs;
        [%decl? d]
        [%int? e];
      }
    |}]] in
  check_pattern_decl bindings#d [%pattern? {
    desc = Var {
      var_type = { desc = MemberPointer {
        pointee = { desc = FunctionType {
          result = { desc = BuiltinType Void };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = BuiltinType Int }}}]}}};
        class_ = { desc = Record ({ name = IdentifierName "S" })}}};
      var_name = "ptr_to_mfct";
      var_init = Some { desc = UnaryOperator {
        kind = AddrOf;
        operand = {
          desc = DeclRef {
            nested_name_specifier = Some [
              TypeSpec { desc = Record { name = IdentifierName "S" }}];
            name = IdentifierName "f" }}}}}}];
  check_pattern_expr bindings#e [%pattern? {
    desc = Call {
      callee = { desc = BinaryOperator {
        lhs = { desc = DeclRef ({ name = IdentifierName "ptr_to_obj" })};
        kind = PtrMemI;
        rhs = { desc = DeclRef ({ name = IdentifierName "ptr_to_mfct" })}}};
      args = [{ desc = IntegerLiteral (Int 10)}]}}]

(* 5.19 Constant expression *)

let () =
  [%if standard "c++17" available begin
    let ast = parse_string
      ~command_line_args:[Clang.Command_line.standard Cxx17] {|
      int x;

      struct A {
        constexpr A(bool b) : m(b?42:x) { }
        int m;
      };

      constexpr int v = A(true).m;

      constexpr int f2(int k) {
        int x = k;
        return x;
      }

      constexpr int incr(int &n) {
        return ++n;
      }

      constexpr int h(int k) {
        int x = incr(k);
        return x;
      }

      constexpr int y = h(1);
    |} in
    let bindings =
      check_pattern_tu ast [%pattern? [%cpp-tu {|
        int x;
        struct A {
          [%decl? a]
          int m;
        };
        [%decl? v]
        [%decl? f2]
        [%decl? incr]
        [%decl? h]
        [%decl? y]
      |}]] in
    check_pattern_decl bindings#a [%pattern? {
      desc = Constructor {
        class_name = "A";
        parameters = {
          non_variadic = [{ desc = {
            qual_type = { desc = BuiltinType Bool };
            name = "b" }}]};
        initializer_list = [("m", {
          desc = ConditionalOperator {
            cond = { desc = DeclRef ({ name = IdentifierName "b" })};
            then_branch = Some { desc = IntegerLiteral (Int 42)};
            else_branch = { desc = DeclRef ({ name = IdentifierName "x" })}}})];
        body = Some { desc = Compound []};
        constexpr = true; }}];
    check_pattern_decl bindings#v [%pattern? {
      desc = Var {
        var_name = "v";
        var_type = { desc = BuiltinType Int };
        var_init = Some { desc = Member {
          base = Some { desc = Cast {
            kind = Functional;
            qual_type = { desc = Record ({ name = IdentifierName "A" }) };
            operand = { desc = Construct {
              qual_type = { desc = Record ({ name = IdentifierName "A" }) };
              args = [{ desc = BoolLiteral true }]}}}};
          arrow = false;
          field = FieldName { desc = { name = IdentifierName "m" } }}};
        constexpr = true }}];
    check_pattern_decl bindings#f2 [%pattern? {
      desc = Function {
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = BuiltinType Int };
              name = "k" }}]}};
        name = IdentifierName "f2";
        constexpr = true }}];
    check_pattern_decl bindings#incr [%pattern? {
      desc = Function {
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = LValueReference { desc = BuiltinType Int }};
              name = "n" }}]}};
        name = IdentifierName "incr";
        constexpr = true }}];
    check_pattern_decl bindings#h [%pattern? {
      desc = Function {
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some {
            non_variadic = [{ desc = {
              qual_type = { desc = BuiltinType Int };
              name = "k" }}]}};
        name = IdentifierName "h";
        constexpr = true }}];
    check_pattern_decl bindings#y [%pattern? {
      desc = Var {
        var_type = { desc = BuiltinType Int };
        var_name = "y";
        var_init = Some { desc = Call {
          callee = { desc = DeclRef ({ name = IdentifierName "h" })};
          args = [{ desc = IntegerLiteral (Int 1)}]; }};
        constexpr = true }}]
  end]

(* 7.1.7.2 Simple type specifiers *)

let () =
  let ast = parse_string
    ~command_line_args:[Clang.Command_line.standard Cxx11] {|
      const int&& foo();
      int i;
      struct A { double x; };
      const A* a = new A();

      decltype(foo()) x1 = 17;
      decltype(i) x2;
      decltype(a->x) x3;
      decltype((a->x)) x4 = x3;
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu (standard "c++11") {|
      const int&& foo();
      int i;
      struct A { double x; };
      const A* a = new A();

      [%decl? x1]
      [%decl? x2]
      [%decl? x3]
      [%decl? x4]
    |}]] in
  check_pattern_decl bindings#x1 [%pattern? {
    desc = Var {
      var_type = { desc = Decltype { desc = Call {
        callee = { desc = DeclRef ({ name = IdentifierName "foo" })};
        args = []}}}}}];
  check_pattern_decl bindings#x2 [%pattern? {
    desc = Var {
      var_type = { desc = Decltype { desc = DeclRef ({ name = IdentifierName "i" })}}}}];
  check_pattern_decl bindings#x3 [%pattern? {
    desc = Var {
      var_type = { desc = Decltype { desc = Member {
        base = Some { desc = DeclRef ({ name = IdentifierName "a" })};
        arrow = true;
        field = FieldName { desc = { name = IdentifierName "x" } }}}}}}];
  check_pattern_decl bindings#x4 [%pattern? {
    desc = Var {
      var_type = { desc = Decltype { desc = Member {
        base = Some { desc = DeclRef ({ name = IdentifierName "a" })};
        arrow = true;
        field = FieldName { desc = { name = IdentifierName "x" } }}}}}}]

(* 12.1 Constructors *)

(*
let () =
  let ast = parse_string {|
      complex zz = complex(1, 2.3);
      cprint( complex(7.8,1.2) );
  |} in
*)

(* 12.3.2 Conversion functions *)

let () =
  let ast = parse_string
    ~options:{ Clang.Ast.Options.default with ignore_implicit_cast = false } {|
    struct X {
      operator int();
    };
    void f(X a) {
      int i = int(a);
      i = (int)a;
      i = a;
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      struct X {
        [%decl? int]
      };
      void f(X a) {
        int i = [%int? i1];
        i = [%int? i2];
        i = [%int? i3];
      }
    |}]] in
  check_pattern_decl bindings#int [%pattern? {
    desc = CXXMethod {
      function_decl = {
        function_type = {
          result = { desc = BuiltinType Int };
          parameters = Some { non_variadic = [] }};
        name = ConversionFunctionName { desc = BuiltinType Int };
        body = None; }}}];
  check_pattern_expr bindings#i1 [%pattern? {
    desc = Cast {
      kind = Functional;
      qual_type = { desc = BuiltinType Int };
      operand = { desc = Cast {
        kind = Implicit;
        qual_type = { desc = BuiltinType Int };
        operand = { desc = Call {
          callee = {
            desc = Member {
              base = Some { desc = DeclRef ({ name = IdentifierName "a" })};
              arrow = false;
              field = FieldName { desc = { name = ConversionFunctionName
                { desc = BuiltinType Int }}}}}}}}}}}];
  check_pattern_expr bindings#i2 [%pattern? {
    desc = Cast {
      kind = CStyle;
      qual_type = { desc = BuiltinType Int };
      operand = { desc = Cast {
        kind = Implicit;
        qual_type = { desc = BuiltinType Int };
        operand = { desc = Call {
          callee = {
            desc = Member {
              base = Some { desc = DeclRef ({ name = IdentifierName "a" })};
              arrow = false;
              field = FieldName { desc = { name = ConversionFunctionName
                { desc = BuiltinType Int }}}}}}}}}}}];
  check_pattern_expr bindings#i3 [%pattern? {
    desc = Cast {
      kind = Implicit;
      qual_type = { desc = BuiltinType Int };
      operand = { desc = Call {
        callee = {
          desc = Member {
            base = Some { desc = DeclRef ({ name = IdentifierName "a" })};
            arrow = false;
            field = FieldName { desc = { name = ConversionFunctionName
              { desc = BuiltinType Int }}}}}}}}}]

(* 14.1 Template parameters *)

let () =
  let ast = parse_string {|
    class T { public: T(int i) {} /* ... */ };
    int i;

    template<class T, T i> void f(T t) {
      T t1 = i; // template-parameters T and i
      ::T t2 = ::i; // global namespace members T and i
    }
  |} in
  let bindings =
    check_pattern_tu ast [%pattern? [%cpp-tu {|
      class T { public: T(int i) {} /* ... */ };
      int i;

      [%decl? f]
    |}]] in
  check_pattern_decl bindings#f [%pattern? {
    desc = TemplateDecl {
      parameters = { list = [
        { desc = {
            parameter_name = "T";
            parameter_kind = Class { default = None }}};
        { desc = {
            parameter_name = "i";
            parameter_kind = NonType {
              parameter_type = { desc = TemplateTypeParm "T" };
              default = None }}}]};
      decl = { desc = Function {
        function_type = {
          result = { desc = BuiltinType Void };
          parameters = Some { non_variadic = [{ desc = {
            name = "t";
            qual_type = { desc = TemplateTypeParm "T" }}}]}};
        name = IdentifierName "f";
        body = Some { desc = Compound [
          { desc = Decl [{ desc = Var {
              var_type = { desc = TemplateTypeParm "T" };
              var_name = "t1";
              var_init = Some { desc = DeclRef ({ name = IdentifierName "i" })}}}]};
          { desc = Decl [{ desc = Var {
              var_type = { desc = Elaborated {
                named_type = { desc = Record ({ name = IdentifierName "T" }) }}};
              var_name = "t2";
              var_init = Some { desc = Construct {
                qual_type = { desc = Elaborated {
                  named_type = { desc = Record ({ name = IdentifierName "T" })}}};
                  args = [{ desc = Construct {
                    qual_type = { desc = Elaborated {
                      named_type = { desc = Record ({ name = IdentifierName "T" })}}};
                    args = [{desc = DeclRef ({ name = IdentifierName "i" })}]}}]}}}}]}]}}}}}]
