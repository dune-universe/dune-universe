
(rule
 (targets test_associative.expected.format)
 (deps (:pp pp.exe) (:input test_associative.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_associative.actual)
 (deps (:pp pp.exe) (:input test_associative.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_associative.actual) (:expected test_associative.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_associative)
 (modules test_associative)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_cap.expected.format)
 (deps (:pp pp.exe) (:input test_cap.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_cap.actual)
 (deps (:pp pp.exe) (:input test_cap.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_cap.actual) (:expected test_cap.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_cap)
 (modules test_cap)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_commutative.expected.format)
 (deps (:pp pp.exe) (:input test_commutative.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_commutative.actual)
 (deps (:pp pp.exe) (:input test_commutative.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_commutative.actual) (:expected test_commutative.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_commutative)
 (modules test_commutative)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_local_arb.expected.format)
 (deps (:pp pp.exe) (:input test_local_arb.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_local_arb.actual)
 (deps (:pp pp.exe) (:input test_local_arb.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_local_arb.actual) (:expected test_local_arb.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_local_arb)
 (modules test_local_arb)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_neutral.expected.format)
 (deps (:pp pp.exe) (:input test_neutral.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_neutral.actual)
 (deps (:pp pp.exe) (:input test_neutral.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_neutral.actual) (:expected test_neutral.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_neutral)
 (modules test_neutral)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_oracle.expected.format)
 (deps (:pp pp.exe) (:input test_oracle.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_oracle.actual)
 (deps (:pp pp.exe) (:input test_oracle.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_oracle.actual) (:expected test_oracle.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_oracle)
 (modules test_oracle)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_roundtrip_dataencoding.expected.format)
 (deps (:pp pp.exe) (:input test_roundtrip_dataencoding.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_roundtrip_dataencoding.actual)
 (deps (:pp pp.exe) (:input test_roundtrip_dataencoding.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_roundtrip_dataencoding.actual) (:expected test_roundtrip_dataencoding.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_roundtrip_dataencoding)
 (modules test_roundtrip_dataencoding)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_saturing.expected.format)
 (deps (:pp pp.exe) (:input test_saturing.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_saturing.actual)
 (deps (:pp pp.exe) (:input test_saturing.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_saturing.actual) (:expected test_saturing.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_saturing)
 (modules test_saturing)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_saturing_roundtrip.expected.format)
 (deps (:pp pp.exe) (:input test_saturing_roundtrip.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_saturing_roundtrip.actual)
 (deps (:pp pp.exe) (:input test_saturing_roundtrip.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_saturing_roundtrip.actual) (:expected test_saturing_roundtrip.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_saturing_roundtrip)
 (modules test_saturing_roundtrip)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_unknown_property.expected.format)
 (deps (:pp pp.exe) (:input test_unknown_property.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_unknown_property.actual)
 (deps (:pp pp.exe) (:input test_unknown_property.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_unknown_property.actual) (:expected test_unknown_property.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_unknown_property)
 (modules test_unknown_property)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))
