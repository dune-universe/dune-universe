let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp
               
let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))

let my_reporter ~app ppf = 
  let new_report src level ~over k msgf =
    let k _ = over (); k () in
    match level with
    | Logs.App -> msgf @@ fun ?header ?tags fmt ->
                          let _ = tags in (* in order to get rid of the tags unused variable warning *)
                          Fmt.kpf k ppf ("%a@[" ^^ fmt ^^ "@]@.") Logs_fmt.pp_header (level, header)
    | Logs.Error -> msgf @@ fun ?header ?tags fmt -> 
                            let _ = tags in (* in order to get rid of the tags unused variable warning *)
                            Fmt.kpf k ppf ("%a @[" ^^ fmt ^^ "@]@.") Logs_fmt.pp_header (level, header)
    | _ -> 
       let p1,p2 = if src = Logs.default then app,"" else app^"/",(Logs.Src.name src) in
       let with_src h tags k ppf fmt =
         let stamp = match tags with
           | None -> None
           | Some tags -> Logs.Tag.find stamp_tag tags
         in
         match stamp with
         | None -> Fmt.kpf k ppf ("%a%a: %a @[" ^^ fmt ^^ "@]@.")  Fmt.(styled `Magenta string) p1 Fmt.(styled `Magenta string) p2 Logs_fmt.pp_header (level, h)
         | Some s ->  
            Fmt.kpf k ppf ("%a%a: %a[%0+4.0fus] @[" ^^ fmt ^^ "@]@.")  Fmt.(styled `Magenta string) p1 Fmt.(styled `Magenta string) p2 Logs_fmt.pp_header (level, h) (Mtime.Span.to_us s)
       in
       msgf @@ fun ?header ?tags fmt -> with_src header tags k ppf fmt in
  { Logs.report = new_report }
    
    
let setup_log ~app style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (my_reporter ~app (Format.std_formatter));  
  ()
    
             
let set_level ~app ?(colored=true) l =
  let tt =
    match colored with
    | true -> Some `Ansi_tty
    | false -> Some `None in
  setup_log ~app tt (Some l) 
