open ExtLib
open Algo

let f () =
  let a1 = { Cudf.default_package with Cudf.package = "a"; version = 1 } in
  let b1 =
    { Cudf.default_package with
      Cudf.package = "b";
      version = 1;
      depends = [[("c", Some (`Gt, 1))]]
    }
  in
  let c1 =
    { Cudf.default_package with
      Cudf.package = "c";
      version = 1;
      installed = true;
      keep = `Keep_package;
      conflicts = [("c", None)]
    }
  in
  let c2 =
    { Cudf.default_package with
      Cudf.package = "c";
      version = 2;
      conflicts = [("c", None)]
    }
  in
  let universe = Cudf.load_universe [a1; b1; c1; c2] in
  let d = Depsolver.edos_install universe b1 in
  Diagnostic.fprintf
    Format.err_formatter
    ~success:true
    ~failure:true
    ~explain:true
    d

;;
f ()
