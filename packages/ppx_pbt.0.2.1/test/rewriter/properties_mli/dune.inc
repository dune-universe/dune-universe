
(rule
 (targets add.expected.format)
 (deps (:pp pp.exe) (:input add.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets add.actual)
 (deps (:pp pp.exe) (:input_ml add.ml) (:input_mli add.mli))
 (action (bash "
     ./%{pp} --intf %{input_mli} &&
     ./%{pp} --impl %{input_ml} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual add.actual) (:expected add.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name add)
 (modules add)
 (preprocess (pps ppx_pbt)))
