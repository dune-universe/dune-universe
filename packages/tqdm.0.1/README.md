# ocaml-tqdm
__ocaml-tqdm__ is a progress bar library for OCaml. The implementation is
based on [Python tqdm library](https://tqdm.github.io).

Below is an example of how to use this library:
```ocaml
module T = Tqdm.Tqdm
T.with_bar 100 ~f:(fun tqdm ->
  for v = 1 to 100 do
    Unix.sleepf 0.1;
    T.update tqdm v
done);
```

![][Screenshot]

  [Screenshot]: https://raw.githubusercontent.com/LaurentMazare/ocaml-tqdm/master/images/utop.gif
