open Printf

let id x = x
let tuck l x = l := x :: !l
let fail ?exn fmt = ksprintf (fun s -> failwith (match exn with None -> s | Some exn -> sprintf "%s : %s" s (Printexc.to_string exn))) fmt
let printfn fmt = ksprintf print_endline fmt
let eprintfn fmt = ksprintf prerr_endline fmt
