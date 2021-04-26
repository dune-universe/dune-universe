module G = Gamestate

let game_test () =
  let b = G.make ~cols:10 ~rows:10 in
  let b = G.play b 1 8 G.P2 in
  let b = G.play b 2 8 G.P2 in
  let b = G.play b 3 8 G.P2 in
  let b = G.play b 4 8 G.P2 in

  let b = G.play b 0 1 G.P1 in
  let b = G.play b 0 2 G.P1 in
  let b = G.play b 0 3 G.P1 in
  let b = G.play b 0 4 G.P1 in
  Format.printf "%a\n" Display.pp b ;
  match G.search_winning b with
  | None -> Format.printf "No winning position found\n%!"
  | Some (G.P1, c, r) -> Format.printf "P1 wins at %d x %d\n%!" c r
  | Some (G.P2, c, r) -> Format.printf "P2 wins at %d x %d\n%!" c r

module Policy = Mcts.MCTS (struct
  type terminal = G.cell * G.t

  type nonterminal = G.cell * G.t

  type state = Terminal of terminal | Nonterminal of nonterminal

  type action = int * int

  let actions (_, state) = Array.of_list (G.playable_positions state)

  let next_player = function G.P1 -> G.P2 | G.P2 -> G.P1

  let next (player, state) (x, y) =
    let b = G.play state x y player in
    match G.search_winning state with
    | None -> Nonterminal (next_player player, b)
    | Some (player, _, _) -> Terminal (player, b)

  let reward (player, _) = match player with G.P1 -> ~-.1. | G.P2 -> 1.

  let exploration_depth = `Unbounded

  let exploration_kernel = `Uniform

  let pp_action fmtr (x, y) = Format.fprintf fmtr "(%d, %d)" x y

  let pp_terminal _fmtr (_winner, _board) = ()

  let pp_nonterminal _fmtr (_winner, _board) = ()
end)

let game_loop () =
  let initial_state = G.make ~cols:10 ~rows:10 in
  let rng_state = Random.State.make [| 0x1337; 0x533D |] in
  let () = Format.printf "Testing MCTS@." in
  let rec loop state =
    Format.printf "%a@." Display.pp state ;
    Format.printf "please input play position: col row = ?@." ;
    let scanner = Scanf.Scanning.from_channel stdin in
    let (col, row) = Scanf.bscanf scanner "%d %d" (fun x y -> (x, y)) in
    let state = G.play state col row G.P1 in
    let (col, row) = Policy.policy ~playouts:100000 (G.P2, state) rng_state in
    let state = G.play state col row G.P2 in
    loop state
  in
  loop initial_state
