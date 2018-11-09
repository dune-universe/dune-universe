(rule (targets KOI8_R.ml)
      (deps (:gen ../gen/generate.exe) maps/KOI8-R.TXT)
      (action (run %{gen} maps/KOI8-R.TXT KOI8_R.ml)))

(rule (targets KOI8_U.ml)
      (deps (:gen ../gen/generate.exe) maps/KOI8-U.TXT)
      (action (run %{gen} maps/KOI8-U.TXT KOI8_U.ml)))

