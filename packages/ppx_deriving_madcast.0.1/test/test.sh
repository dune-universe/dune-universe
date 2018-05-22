#!/bin/sh

set -eu


compile_errs=0
compile_err () {
  compile_errs=$((compile_errs + 1))
}

runtime_errs=0
runtime_err () {
  runtime_errs=$((runtime_errs + 1))
}

false_positives=0
false_positive () {
  false_positives=$((false_positives + 1))
}

successes=0
succeed () {
  printf '.'
  successes=$((successes + 1))
}

compile () {
  ../_build/default/.ppx/ppx_deriving_madcast/ppx.exe \
    "$1" \
    -o "${1%.ml}.pp.ml" \
  && ocamlfind ocamlc "${1%.ml}.pp.ml" -o "${1%.ml}.exe"
}


## Run the tests #############################################################

# Init
rm -rf _build
mkdir _build
cp positive/*.ml _build

# Positive tests
echo "Running positive tests…"
for file in _build/*.ml
do
  target="${file%.ml}.exe"
  if compile "$file" "$target"; then
    if ./"$target" > /dev/null; then
      succeed
    else
      runtime_err
    fi
  else
    compile_err
  fi
done
printf '\n'

# Negative tests
echo "Running negative tests…"
for file in negative/*.ml
do
  if compile "$file" /dev/null 2>/dev/null; then
    false_positive
  else
    succeed
  fi
done
printf '\n'


## Display a summary #########################################################

total=$((successes + compile_errs + runtime_errs + false_positives))
echo "Ran $total tests"

if [ "$successes" -eq "$total" ]; then
  echo "All tests passed successfully! \033[32m✓\033[0m"
  exit 0
else
  echo " - $successes tests passed successfully"
fi

if [ "$compile_errs" -ne 0 ]; then
  echo " - there are $compile_errs compile errors"
fi

if [ "$runtime_errs" -ne 0 ]; then
  echo " - there are $runtime_errs runtime errors"
fi

if [ "$false_positives" -ne 0 ]; then
  echo " - there are $false_positives false positives"
fi

exit 1
