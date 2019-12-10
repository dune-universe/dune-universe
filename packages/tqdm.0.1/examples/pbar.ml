open Base
module T = Tqdm.Tqdm

let () =
  Stdio.printf "Starting pbar...\n%!";
  T.with_bar 100 ~f:(fun tqdm ->
      for v = 1 to 100 do
        Unix.sleepf 0.1;
        T.update tqdm v
      done);
  List.iter [ T.Style.Ascii; Line; Circle; Braille; Vertical ] ~f:(fun style ->
      T.with_bar ~options:{ T.Options.default with style } 100 ~f:(fun tqdm ->
          for v = 1 to 100 do
            Unix.sleepf 0.02;
            T.update tqdm v
          done))
