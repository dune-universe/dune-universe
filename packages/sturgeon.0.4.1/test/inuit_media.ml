open Sturgeon
open Session
open Inuit.Cursor
open Inuit_widget

let word = Str.regexp "[-_a-zA-Z][-_a-zA-Z][-_a-zA-Z]+"

let extract_words str =
  let rec search acc n =
    match Str.search_forward word str n with
    | exception Not_found -> acc
    | _ ->
      search (Str.matched_string str :: acc) (Str.match_end ())
  in
  List.rev (search [] 0)

let () =
  Sturgeon_recipes_command.text_command @@ fun ~args:_ shell ->
  let k = Stui.create_cursor shell ~name:"spotifouille" in
  text k " Recherche: ";
  let k' = ref null in
  let _ = Edit.make k ~on_change:(fun t ->
      clear !k';
      text !k' (String.concat ", " (extract_words (Edit.state t)));
    ) in
  text k "\n Mots-clés: ";
  k' := sub k;
  text k "\n Volume ";
  let _ = Slider.make k in
  text k "\n ";
  let _ = Check.make k in
  text k " Répéter ";
  let _ = Check.make k in
  text k " Lecture aléatoire ";
  ()

