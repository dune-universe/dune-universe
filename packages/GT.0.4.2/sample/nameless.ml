@type ('name, 'lam) lam =
  [ `App of 'lam * 'lam
  | `Var of 'name
  ] with show

let index xs v =
  let rec helper i = function
    | [] -> failwith "No such variable introduced"
    | h :: _ when h = v -> i
    | _ :: tl -> helper (1+i) tl
  in
  helper 0 xs

class [ 'lam , 'nless ] lam_to_nameless
  (flam : string list -> 'lam -> 'nless) =
object
  inherit
  [ string list , string , int
  , string list , 'lam , 'nless
  , string list , 'lam , 'nless ] lam_t
  method c_App env _ l r =
    `App (flam env l, flam env r)
  method c_Var env _ x = `Var (index env x)
end

@type ('name , 'lam) abs = [ `Abs of 'name * 'lam ] with show

class [ 'lam , 'nless ] abs_to_nameless
  (flam : string list -> 'lam -> 'nless) =
object
  inherit [ string list, string, int
          , string list, 'lam, 'nless
          , string list, 'lam, 'nless ] abs_t
  method c_Abs env _ name term =
   `Abs (flam (name :: env) term)
end

@type ('name, 'lam) term =
  [ ('name, 'lam) lam
  | ('name, 'lam) abs ] with show

@type named = (GT.string, named) term with show
@type nameless = [ (GT.int , nameless) lam | `Abs of nameless ] with show

class to_nameless (f : string list -> named -> nameless) =
object
  inherit [ string list , named , nameless ] named_t
  inherit [ named , nameless ] lam_to_nameless f
  inherit [ named , nameless ] abs_to_nameless f
end

let to_nameless term =
  GT.transform(named)
    (fun fself -> new to_nameless fself)
    []
    term

let () =
  Format.printf "%s\n%!" (GT.show nameless (to_nameless (`Abs ("x", `Var "x"))));
  Format.printf "%s\n%!" (GT.show nameless (to_nameless (`Abs ("x", `Abs ("y", `Var "x"))) ) )
