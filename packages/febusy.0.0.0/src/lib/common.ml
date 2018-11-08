include Nonstd
module String = Sosa.Native_string

let dbg fmt = ksprintf (eprintf "  BuildDBG: %s\n%!") fmt

module Error = struct
  type t = String of string | Exn of exn * string

  let string fmt = ksprintf (fun s -> Error (String s)) fmt

  let exn e fmt = ksprintf (fun s -> Error (Exn (e, s))) fmt

  let catch_exn f fmt = try Ok (f ()) with e -> exn e fmt

  let to_string = function
    | String s -> s
    | Exn (e, s) -> sprintf "Exception %S @ %s" (Printexc.to_string e) s

  let throw = function Ok o -> o | Error e -> failwith (to_string e)
end
