open Wseg
open Base
open Stdio

let test dict max str=
  let cand= Dict.candidates dict str max in
  begin
    printf "candidates of %s\n" str;
    cand |> Dict.dispCands;
    let cand1= cand |> MMSEG.rule1 in
    printf "rule1\n";
    Dict.dispCands cand1;
    let cand2= cand1 |> MMSEG.rule2 in
    printf "rule2\n";
    Dict.dispCands cand2;
    let cand3= cand2 |> MMSEG.rule3 in
    printf "rule3\n";
    Dict.dispCands cand3;
    let cand4= cand3 |> MMSEG.rule4 in
    printf "rule4\n";
    Dict.dispCands cand4;
    match MMSEG.rule_final cand4 with
    | Some chunk-> printf "result: [%s]\n" (Dict.result_of_cand chunk)
    | None-> ()
  end

let rex= Pcre.regexp "^([^\t]+)\t([^\t]+)"

let get_entry s=
  let ss= Pcre.exec ~rex s |> Pcre.get_substrings in
  (ss.(1), Float.of_string ss.(2))

let lines_to_entries= List.map ~f:get_entry

let ()=
  let charEntries=
    (In_channel.read_lines "char.dic")
      |> lines_to_entries
      |> Dict.buildEntries
  and wordEntries=
    (In_channel.read_lines "word.dic")
      |> lines_to_entries
      |> Dict.buildEntries
  in
  let wordDict= Dict.buildIndex (List.append charEntries wordEntries) in
  test wordDict 4 "研究生命起源";
  Out_channel.newline stdout;
  test wordDict 4 "主要是因为"

