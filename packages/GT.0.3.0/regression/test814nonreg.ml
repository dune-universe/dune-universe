open Printf

module Y = struct
  type 'a s = SS of 'a
  and t = GT.int s
  and u = GT.float s
  [@@deriving gt ~options:{show}]

  let () =
    let () = printf "%s\n%!" @@ GT.(show s) (sprintf "%S") (SS "asdf") in
    let () = printf "%s\n%!" @@ GT.(show s) (sprintf "%b") (SS true) in
    let () = printf "%s\n%!" @@ GT.(show t) (SS 42) in
    let () = printf "%s\n%!" @@ GT.(show u) (SS 3.1415) in
    ()

  class ['a,'extra_s] show_s_t2 (fa,_,_) = object
    inherit  [unit,'a,string,unit,'extra_s,string] s_t
    method c_SS () _ x = Printf.sprintf "THE '%a'" fa x
  end
  let show_s2 call fa () subj =
    GT.transform_gc gcata_s (new show_s_t2 call fa) () subj


  let show = Fix_show_s.fixv (fun f ->
      { call = fun (type a) (sym : a Ishow_s.i) : a ->
            match sym with
            | Ishow_s.S -> show_s2 f
            | Ishow_s.U -> show_u_0  f
            | Ishow_s.T -> show_t_0  f
      })

  let show_s fa = show.call Ishow_s.S (GT.lift fa) ()

  let () =
    printf "After overriding a method\n%!";
    printf "%s\n%!" @@ show_s (sprintf "%s") (SS "ZZZZZZZ")

end
