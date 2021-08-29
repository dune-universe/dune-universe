let coin = Fin.Float.coin ~bias:0.2

let () =
  Format.printf "biased coin:\n%a@." Fin.Float.pp_fin_mes (Fin.as_measure coin)

let bin = Fin.Float.binomial coin 7

let _ =
  Format.printf
    "binomial law on {0; ...; 6}:\n%a@."
    Fin.Float.pp_fin_mes
    (Fin.as_measure bin)
