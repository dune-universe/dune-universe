module Pervasives = struct
  let failwithf fmt = Printf.kprintf failwith fmt
  let invalid_argf fmt = Printf.kprintf invalid_arg fmt
  let raisef f fmt = Printf.kprintf (fun s -> raise (f s)) fmt
  
  exception Finally of exn * exn
  
  (* CR jfuruse: looks lousy... *)
  let protect f v ~(finally : 'a -> unit) =
    let res =
      try f v
      with exn ->
        (try finally v with final_exn -> raise (Finally (exn, final_exn)));
        raise exn
    in
    finally v;
    res
  
  let protect_with f v ~finally =
    let res =
      try f v
      with exn ->
        (try finally v with final_exn -> raise (Finally (exn, final_exn)));
        raise exn
    in
    res, finally v
  
  let catch f v = try Ok (f v) with e -> Error (`Exn e)
  
  let try_ignore f v = try f v with _ -> ()
  
  let try_or f g v = try f v with _ -> g v
  
  let try_bool f v = try f v; true with _ -> false
  
  let protect_ f = protect f ()
  let protect_with_ f = protect_with f ()
  let catch_ f = catch f ()
  let try_ignore_ f = try_ignore f ()
  let try_or_ f g = try_or f g ()
  let try_bool_ f = try_bool f ()
  
  let tee f v ~handler = try f v with e -> handler e; raise e

  type 'a return = { return : 'jump . 'a -> 'jump }
                   
  let with_return (type a) f : a =
    let module M = struct
      exception Return of a
    end in
    try f { return = fun a -> raise (M.Return a) } with M.Return a -> a  
end

include Pervasives
  
(* Printexc 

   Printexc has a very bad name. Printexc for exn ?
*)
let to_string        = Printexc.to_string
let format ppf t     = Format.pp_print_string ppf (Printexc.to_string t)
let print_backtrace  = Printexc.print_backtrace
let get_backtrace    = Printexc.get_backtrace
let register_printer = Printexc.register_printer
