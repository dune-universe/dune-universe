open Clim

let n = ref 0

let foo = object(self)
  inherit [_] cli
  method entrypoint () = Fmt.pr "%i@." !n

  initializer
    self#set (value @@ opt int !n ["i"]) n;
    self#arg (value @@ opt string "pouet" ["s"]) (Fmt.pr "%s@.")
end

let _ = foo#run
