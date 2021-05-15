
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
 (targets test_eq.expected.format)
 (deps (:pp pp.exe) (:input test_eq.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_eq.actual)
 (deps (:pp pp.exe) (:input test_eq.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_eq.actual) (:expected test_eq.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_eq)
 (modules test_eq)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_local_gen.expected.format)
 (deps (:pp pp.exe) (:input test_local_gen.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_local_gen.actual)
 (deps (:pp pp.exe) (:input test_local_gen.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_local_gen.actual) (:expected test_local_gen.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_local_gen)
 (modules test_local_gen)
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
