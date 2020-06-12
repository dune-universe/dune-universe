open TestGettext

let e = Printf.eprintf

let () =
  e (f_ "%ld") 1l;
  e (f_ "%d") 1;
  e (f_ "%s") "abcd";
  e (f_ "%a") (fun chn e -> Printf.fprintf chn "%s" e) "ancd";
  e (fn_ "%d category" "%d categories" 1) 1
