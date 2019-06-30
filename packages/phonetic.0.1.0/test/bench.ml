let bench name n fn =
  ignore @@ Benchmark.latency1 ~name n (List.iter fn) Test_utils.data

let () =
  bench "DoubleMetaphone.double_metaphone" 1000L
    (fun x -> ignore @@ Phonetic.DoubleMetaphone.double_metaphone x.input)
; bench "Soundex.soundex" 20000L
    (fun x -> ignore @@ Phonetic.Soundex.soundex x.input)
