module T = Tqdm.Tqdm

let niters = 10_000_000_000

let () =
  T.with_bar niters ~f:(fun tqdm ->
      for v = 1 to niters do
        T.update tqdm v
      done)
