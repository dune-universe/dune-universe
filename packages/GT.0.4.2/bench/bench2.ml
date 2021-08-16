let () =
  Format.set_margin 1000;
  Format.set_max_indent 0

module MySamples = struct
  type t = Benchmark.t = {
    wall : Base.Float.t;
    utime : Base.Float.t;
    stime : Base.Float.t;
    cutime : Base.Float.t;
    cstime : Base.Float.t;
    iters : Base.Int64.t;
  }

  let sexp_of_t { wall } = Base.sexp_of_float wall

  (* Base.sexp_of_int64 iters *)

  open Base

  type samples = (string * t list) list [@@deriving sexp_of]

  let save res filename =
    let ch = Stdlib.open_out filename in
    Stdlib.Printf.fprintf ch "%s\n"
      (Base.Sexp.to_string_hum @@ sexp_of_samples res);
    Stdlib.close_out ch
end

module Lambda = struct
  type t = Var of string | App of t * t | Abs of string * t

  type ('i, 'a) strat = {
    var : 'self -> 'i -> string -> 'a;
    app : 'self -> 'i -> t -> t -> 'a;
    abs : 'self -> 'i -> string -> t -> 'a;
  }
    constraint 'self = ('i, 'a) strat

  let apply_strat st inh = function
    | Var s -> st.var st inh s
    | App (l, r) -> st.app st inh l r
    | Abs (s, x) -> st.abs st inh s x

  module Id = struct
    let name = "lambda copying"

    let create n =
      let rec helper acc m =
        if m < n then helper (Abs ("x", acc)) (m + 1) else acc
      in
      helper (Var "x") 0

    module D = struct
      let rec f inh = function
        | Var s -> Var s
        | App (l, r) -> App (f inh l, f inh r)
        | Abs (x, b) -> Abs (x, f inh b)
    end

    module M = struct
      let work =
        {
          var = (fun _ _ s -> Var s);
          app = (fun st i l r -> App (apply_strat st i l, apply_strat st i r));
          abs = (fun st i s b -> Abs (s, apply_strat st i b));
        }

      let f = apply_strat work
    end

    module V = struct
      let o =
        object (self)
          method c_App inh l r = App (self#gcata inh l, self#gcata inh r)

          method c_Abs inh x b = Abs (x, self#gcata inh b)

          method c_Var _ x = Var x

          method gcata inh =
            function
            | Var x -> self#c_Var inh x
            | App (l, r) -> self#c_App inh l r
            | Abs (l, r) -> self#c_Abs inh l r
        end

      let f inh xs = o#gcata inh xs
    end

    module G = struct
      let o fself =
        object
          method c_App inh l r = App (fself inh l, fself inh r)

          method c_Abs inh x b = Abs (x, fself inh b)

          method c_Var _ x = Var x
        end

      let gcata self inh = function
        | Var x -> self#c_Var inh x
        | App (l, r) -> self#c_App inh l r
        | Abs (l, r) -> self#c_Abs inh l r

      let f inh xs =
        let rec obj = lazy (o fself)
        and fself () x = gcata (Lazy.force obj) inh x in
        fself inh xs
    end
  end

  module Iter = struct
    let name = "lambda iteration"

    let create n = Id.create n

    module D = struct
      let rec f inh = function
        | Var _ -> ()
        | App (l, r) -> f (f inh r) l
        | Abs (_, b) -> f inh b
    end

    module M = struct
      let work =
        {
          var = (fun _ inh _ -> inh);
          app = (fun st i l r -> apply_strat st (apply_strat st i r) l);
          abs = (fun st i _ b -> apply_strat st i b);
        }

      let f = apply_strat work
    end

    module V = struct
      let o =
        object (self)
          method c_App inh l r = self#gcata (self#gcata inh r) l

          method c_Abs inh _ b = self#gcata inh b

          method c_Var _ _ = ()

          method gcata inh =
            function
            | Var x -> self#c_Var inh x
            | App (l, r) -> self#c_App inh l r
            | Abs (l, r) -> self#c_Abs inh l r
        end

      let f inh xs = o#gcata inh xs
    end

    module G = struct
      let o fself =
        object
          method c_App inh l r = fself (fself inh r) l

          method c_Abs inh _ b = fself inh b

          method c_Var _ _ = ()
        end

      let gcata self inh = function
        | Var x -> self#c_Var inh x
        | App (l, r) -> self#c_App inh l r
        | Abs (l, r) -> self#c_Abs inh l r

      let f inh xs =
        let rec obj = lazy (o fself)
        and fself () x = gcata (Lazy.force obj) inh x in
        fself inh xs
    end
  end

  module PP = struct
    let name = "lambda formatting"

    let create n = Id.create n

    module D = struct
      let rec f ppf = function
        | Var s -> Format.fprintf ppf "%s" s
        | App (l, r) -> Format.fprintf ppf "(%a %a)" f l f r
        | Abs (s, b) -> Format.fprintf ppf "(\\ %s -> %a)" s f b
    end

    module M = struct
      let work =
        {
          var = (fun _ ppf s -> Format.fprintf ppf "%s" s);
          app =
            (fun st ppf l r ->
              Format.fprintf ppf "(%a %a)" (apply_strat st) l (apply_strat st) r);
          abs =
            (fun st ppf s b ->
              Format.fprintf ppf "(\\ %s -> %a)" s (apply_strat st) b);
        }

      let f = apply_strat work
    end

    module V = struct
      let o =
        object (self)
          method c_Var ppf s = Format.fprintf ppf "%s" s

          method c_App ppf l r =
            Format.fprintf ppf "(%a %a)" self#gcata l self#gcata r

          method c_Abs ppf x b =
            Format.fprintf ppf "(\\ %s -> %a)" x self#gcata b

          method gcata ppf =
            function
            | Var x -> self#c_Var ppf x
            | App (l, r) -> self#c_App ppf l r
            | Abs (l, r) -> self#c_Abs ppf l r
        end

      let f inh xs = o#gcata inh xs
    end

    module G = struct
      let o fself =
        object
          method c_Var ppf s = Format.fprintf ppf "%s" s

          method c_App ppf l r = Format.fprintf ppf "(%a %a)" fself l fself r

          method c_Abs ppf x b = Format.fprintf ppf "(\\ %s -> %a)" x fself b
        end

      let gcata self ppf = function
        | Var x -> self#c_Var ppf x
        | App (l, r) -> self#c_App ppf l r
        | Abs (l, r) -> self#c_Abs ppf l r

      let f ppf xs =
        let rec obj = lazy (o fself)
        and fself ppf x = gcata (Lazy.force obj) ppf x in
        fself ppf xs
    end
  end

  let rec subst x ~by = function
    | Var z when z = x -> by
    | Var _ as ans -> ans
    | App (l, r) -> App (subst x ~by l, subst x ~by r)
    | Abs (y, _) as lam when x = y -> lam
    | Abs (y, b) -> Abs (y, subst x ~by b)

  module Eval = struct
    let name = "eval"

    let create =
      let id = Abs ("x", Var "x") in
      let rec helper acc = function
        | 0 -> acc
        | n -> helper (App (id, acc)) (n - 1)
      in
      helper id

    module D = struct
      let rec eval = function
        | (Var _ as lam) | (Abs (_, _) as lam) -> lam
        | App (f, x) -> (
            match eval f with
            | Abs (v, b) -> eval @@ subst v ~by:x b
            | f2 -> App (f2, x))

      let f () = eval
    end

    module M = struct
      let work =
        {
          var = (fun _ _ s -> Var s);
          abs = (fun _ _ s b -> Abs (s, b));
          app =
            (fun st _ f x ->
              match apply_strat st () f with
              | Abs (v, b) -> apply_strat st () @@ subst v ~by:x b
              | f2 -> App (f2, x));
        }

      let f = apply_strat work
    end

    module V = struct
      let o =
        object (self)
          method c_Var _ s = Var s

          method c_Abs _ x b = Abs (x, b)

          method c_App ppf f x =
            match self#gcata ppf f with
            | Abs (v, b) -> self#gcata ppf @@ subst v ~by:x b
            | f2 -> App (f2, x)

          method gcata ppf =
            function
            | Var x -> self#c_Var ppf x
            | App (l, r) -> self#c_App ppf l r
            | Abs (l, r) -> self#c_Abs ppf l r
        end

      let f inh xs = o#gcata inh xs
    end

    module G = struct
      let o fself =
        object
          method c_Var _ s = Var s

          method c_Abs _ x b = Abs (x, b)

          method c_App ppf f x =
            match fself ppf f with
            | Abs (v, b) -> fself ppf @@ subst v ~by:x b
            | f2 -> App (f2, x)
        end

      let gcata self ppf = function
        | Var x -> self#c_Var ppf x
        | App (l, r) -> self#c_App ppf l r
        | Abs (l, r) -> self#c_Abs ppf l r

      let f ppf xs =
        let rec obj = lazy (o fself)
        and fself ppf x = gcata (Lazy.force obj) ppf x in
        fself ppf xs
    end
  end
end

open Benchmark

let timeout = 1

let repeat = 20

let style = Auto

let style = Nil

let confidence = 0.95

let sizes = [ 100; 200; 300; 500; 700; 900; 1000 ]

let __ () =
  let module M = Lambda.Iter in
  sizes
  |> List.iter (fun n ->
         let xs = M.create n in
         let wrap f () =
           (* Gc.major ();
              Gc.minor ();
              Gc.compact (); *)
           let _ = f () xs in
           ()
         in
         let res =
           throughputN ~repeat ~style timeout
             [
               (Printf.sprintf "%s_D_%d" M.name n, wrap M.D.f, ());
               (Printf.sprintf "%s_M_%d" M.name n, wrap M.M.f, ());
               (Printf.sprintf "%s_V_%d" M.name n, wrap M.V.f, ());
               (Printf.sprintf "%s_G_%d" M.name n, wrap M.G.f, ());
             ]
         in
         print_newline ();
         tabulate ~confidence res)

let __ () =
  let module M = Lambda.Id in
  sizes
  |> List.iter (fun n ->
         let xs = M.create n in
         let wrap f () =
           (* Gc.major ();
              Gc.minor ();
              Gc.compact (); *)
           let _ = f () xs in
           ()
         in
         let res =
           throughputN ~repeat ~style timeout
             [
               (Printf.sprintf "%s_D_%d" M.name n, wrap M.D.f, ());
               (Printf.sprintf "%s_M_%d" M.name n, wrap M.M.f, ());
               (Printf.sprintf "%s_V_%d" M.name n, wrap M.V.f, ());
               (Printf.sprintf "%s_G_%d" M.name n, wrap M.G.f, ());
             ]
         in
         print_newline ();
         let () = MySamples.save res (Printf.sprintf "%s_%d.sexp" M.name n) in
         tabulate ~confidence res)

let () =
  let module M = Lambda.PP in
  sizes
  |> List.iter (fun n ->
         let xs = M.create n in
         let b = Buffer.create 2000 in
         let wrap f () =
           Buffer.clear b;
           let ppf = Format.formatter_of_buffer b in
           let _ = f ppf xs in
           let () = Format.pp_print_flush ppf () in
           ()
         in
         let res =
           throughputN ~repeat ~style timeout
             [
               (Printf.sprintf "%s_D_%d" M.name n, wrap M.D.f, ());
               (Printf.sprintf "%s_M_%d" M.name n, wrap M.M.f, ());
               (Printf.sprintf "%s_V_%d" M.name n, wrap M.V.f, ());
               (Printf.sprintf "%s_G_%d" M.name n, wrap M.G.f, ());
             ]
         in
         print_newline ();
         let () = MySamples.save res (Printf.sprintf "%s_%d.sexp" M.name n) in
         tabulate ~confidence res)

let () =
  let module M = Lambda.Eval in
  sizes
  |> List.iter (fun n ->
         let xs = M.create n in
         let wrap f () =
           (* Gc.major ();
              Gc.minor ();
              Gc.compact (); *)
           let _ = f () xs in
           ()
         in
         let res =
           throughputN ~repeat ~style timeout
             [
               (Printf.sprintf "%s_D_%d" M.name n, wrap M.D.f, ());
               (Printf.sprintf "%s_M_%d" M.name n, wrap M.M.f, ());
               (Printf.sprintf "%s_V_%d" M.name n, wrap M.V.f, ());
               (Printf.sprintf "%s_G_%d" M.name n, wrap M.G.f, ());
             ]
         in
         let () = MySamples.save res (Printf.sprintf "%s_%d.sexp" M.name n) in

         print_newline ();
         tabulate ~confidence res)
