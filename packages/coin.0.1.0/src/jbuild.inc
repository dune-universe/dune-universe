(rule ((targets (KOI8_R.ml))
       (deps (maps/KOI8-R.TXT))
       (action (run ${exe:../gen/generate.exe} maps/KOI8-R.TXT KOI8_R.ml))))

(rule ((targets (KOI8_U.ml))
       (deps (maps/KOI8-U.TXT))
       (action (run ${exe:../gen/generate.exe} maps/KOI8-U.TXT KOI8_U.ml))))

