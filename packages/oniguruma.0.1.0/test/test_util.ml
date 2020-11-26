let check_out_of_bounds regs idx =
  (try ignore (Oniguruma.Region.capture_beg regs idx); assert false with
   | Invalid_argument _ -> ());
  (try ignore (Oniguruma.Region.capture_end regs idx); assert false with
   | Invalid_argument _ -> ())

let check_against regs exp_regs =
  let num_regs = Oniguruma.Region.length regs in
  check_out_of_bounds regs (-1);
  check_out_of_bounds regs num_regs;
  let rec loop i num_regs exp_regs = match num_regs, exp_regs with
    | 0, [] -> ()
    | 0, _ :: _ -> assert false
    | _, [] -> assert false
    | n, (exp_beg, exp_end) :: exp_regs ->
      assert (Oniguruma.Region.capture_beg regs i = exp_beg);
      assert (Oniguruma.Region.capture_end regs i = exp_end);
      loop (i + 1) (n - 1) exp_regs
  in loop 0 num_regs exp_regs

let test_search coptions soptions enc pat str exp_regs =
  match Oniguruma.create pat coptions enc Oniguruma.Syntax.default with
  | Error err ->
    prerr_endline pat;
    prerr_endline str;
    prerr_endline err;
    assert false
  | Ok r ->
    match Oniguruma.search r str 0 (String.length str) soptions with
    | None ->
      prerr_endline pat;
      prerr_endline str;
      assert false
    | Some region ->
      check_against region exp_regs

let neg_test_search coptions soptions enc pat str =
  match Oniguruma.create pat coptions enc Oniguruma.Syntax.default with
  | Error err ->
    prerr_endline pat;
    prerr_endline str;
    prerr_endline err;
    assert false
  | Ok r ->
    match Oniguruma.search r str 0 (String.length str) soptions with
    | None -> ()
    | Some _ -> assert false

let test_search_out_of_bounds coptions soptions enc pat str s_beg s_end =
  match Oniguruma.create pat coptions enc Oniguruma.Syntax.default with
  | Error err ->
    prerr_endline pat;
    prerr_endline str;
    prerr_endline err;
    assert false
  | Ok r ->
    match Oniguruma.search r str s_beg s_end soptions with
    | None -> ()
    | Some _ -> assert false

let test_match coptions soptions enc pat n str exp_regs =
  match Oniguruma.create pat coptions enc Oniguruma.Syntax.default with
  | Error err ->
    prerr_endline pat;
    prerr_endline str;
    prerr_endline err;
    assert false
  | Ok r ->
    match Oniguruma.match_ r str n soptions with
    | None ->
      prerr_endline pat;
      prerr_endline str;
      assert false
    | Some region ->
      check_against region exp_regs

let neg_test_match coptions soptions enc pat n str =
  match Oniguruma.create pat coptions enc Oniguruma.Syntax.default with
  | Error err ->
    prerr_endline pat;
    prerr_endline str;
    prerr_endline err;
    assert false
  | Ok r ->
    match Oniguruma.match_ r str n soptions with
    | None -> ()
    | Some _ -> assert false

let test_match_out_of_bounds coptions soptions enc pat str pos =
  match Oniguruma.create pat coptions enc Oniguruma.Syntax.default with
  | Error err ->
    prerr_endline pat;
    prerr_endline str;
    prerr_endline err;
    assert false
  | Ok r ->
    match Oniguruma.match_ r str pos soptions with
    | None -> ()
    | Some _ -> assert false
