## 113.43.00

- Typically when we use jsonrep, the complete workflow involves
  getting some json string from the outside, parsing it with
  `Json.Json_io.json_of_string`, and then piping the json into the
  `t_of_json` that we get from jsonrep.

  For a value expected to be float, its external string may very well
  be something like "20", which will become `Json_type.Int 20` after
  parsed by `Json_io`, and then this json int will raise because we
  are expecting a float.

  This feature does a simple fix to have the `float_of_json` logic in
  jsonrep take json int as well.

  We also updated the test modules to
  1) include the longer round trip starting from strings
  2) use more modern test idioms

## 113.24.00

- Switched to ppx.

- Kill the nonrec rewrite done by typerep. It is no longer needed since
  4.02.2, we kept it only for compatibility with the camlp4 code.

## 112.17.00

- typerep_extended now use core_kernel
