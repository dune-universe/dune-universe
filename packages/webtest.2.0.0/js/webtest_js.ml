open Js_of_ocaml

module Runner = struct
  let paint color string = match color with
    | `Red -> "\027[31m" ^ string ^ "\027[0m"
    | `Green -> "\027[32m" ^ string ^ "\027[0m"
    | `Yellow -> "\027[33m" ^ string ^ "\027[0m"
    | _ -> string

  let colored_string_of_result r =
    let s = Webtest.Suite.string_of_result r in
    let c = match r with
    | Webtest.Suite.Error _ -> `Yellow
    | Webtest.Suite.Fail _ -> `Red
    | Webtest.Suite.Pass -> `Green
    in
    paint c s

  let show v =
    Printf.kprintf
      (fun b -> Firebug.console##info (Js.string b))
      v

  let run ?(with_colors=true) suite =
    let open Webtest.Suite in
    let open Webtest.Utils in
    run
      suite (fun {log=_; outcomes} ->
        show "Tests results:";
        let raw_summary = summarise_raw outcomes in
        List.iter
          (fun {label; result; time_s} ->
            let sresult =
              if with_colors
              then colored_string_of_result result
              else string_of_result result
            in
            show "Test %s ... %s (took %.4fs)" label sresult time_s)
          outcomes;
        let test_result =
          if raw_summary.passed
          then paint `Green "Pass"
          else paint `Red "Failed" in
        show "";
        show
          "Test result: %s. %d in total; %d passed; %d failed; %d errored."
          test_result raw_summary.total raw_summary.passes
          raw_summary.failures raw_summary.errors;
        exit (if raw_summary.passed then 0 else 1))

  let install_webtest suite =
    let webtest = Js.Unsafe.obj [||] in
    webtest##.finished := Js._false;
    webtest##.log := Js.string "";
    webtest##.passed := Js._false;
    webtest##.run := Js.wrap_callback
      (fun () ->
        let open Webtest in
        Utils.run suite (fun {Utils.log; outcomes} ->
          let {Utils.report; passed} = Utils.summarise outcomes in
          webtest##.log := Js.string ((String.concat "\n" log) ^ "\n" ^ report);
          webtest##.passed := if passed then Js._true else Js._false;
          webtest##.finished := Js._true));
    Js.Unsafe.global##.webtest := webtest

  let setup suite =
    let module Html = Dom_html in
    Html.window##.onload := Html.handler
      (fun _ ->
        let () = install_webtest suite in
        Js._false)
end
