#!/bin/bash

set -e -u -o pipefail
source_libname=$1; shift
debug_libname=$1; shift
rewriting=("$@"); shift

function is-main-module {
    [ "$1" = "$source_libname" ]
}

q='"'
lib=../src
tmp="$(mktemp --tmpdir incremental_debugXXXXXX)"
(
  echo module ${debug_libname^}__ = struct
  for module in $(cat $lib/$source_libname.pack-order); do
    if [ -f $lib/$module.ml ] && [ -f $lib/$module.mli ]; then
      if is-main-module $module; then echo "only main module without .mli is supported"; exit 1; fi
      echo module ${module^} : sig
      echo '#1' $q$module.mli$q
      cat $lib/$module.mli
      echo end = struct
      echo '#1' $q$module.ml$q
      cat $lib/$module.ml
      echo end
    elif [ -f $lib/$module.ml ]; then
      if ! is-main-module $module; then
        echo module ${module^} = struct
        echo '#1' $q$module.ml$q
        cat $lib/$module.ml
        echo end
      else
        echo end
        echo open ${debug_libname^}__
        echo '#1' $q$module.ml$q
        cat $lib/$module.ml
      fi
    elif [ -f $lib/$module.mli ]; then
      if is-main-module $module; then echo "main module needs .ml"; exit 1; fi
      echo module rec ${module^} : sig
      echo '#1' $q$module.mli$q
      cat $lib/$module.mli
      echo end = ${module^}
    fi
    echo
  done
  echo 'let () = assert Import.debug'
) | "${rewriting[@]}" > "$tmp"
mv "$tmp" $debug_libname.ml
chmod -w $debug_libname.ml
