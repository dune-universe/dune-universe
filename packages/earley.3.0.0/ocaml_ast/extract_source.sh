#!/bin/bash

# URL to fetch OCaml from.
OCAML_URL="https://github.com/ocaml/ocaml/archive/4.10.0.tar.gz"

# Created source directory.
SRC_DIR="src"

# Files to copy from the ocaml source tree.
COPIED=(
  "utils/arg_helper.ml"
  "utils/arg_helper.mli"
  "utils/build_path_prefix_map.ml"
  "utils/build_path_prefix_map.mli"
  "utils/identifiable.ml"
  "utils/identifiable.mli"
  "utils/misc.ml"
  "utils/misc.mli"
  "utils/numbers.ml"
  "utils/numbers.mli"
  "utils/profile.ml"
  "utils/profile.mli"
  "utils/terminfo.ml"
  "utils/terminfo.mli"
  "utils/warnings.ml"
  "utils/warnings.mli"
  "stdlib/int.ml"
  "stdlib/int.mli"
  "stdlib/list.ml"
  "stdlib/list.mli"
  "stdlib/option.ml"
  "stdlib/option.mli"
  "parsing/ast_helper.ml"
  "parsing/ast_helper.mli"
  "parsing/ast_iterator.ml"
  "parsing/ast_iterator.mli"
  "parsing/asttypes.mli"
  "parsing/attr_helper.ml"
  "parsing/attr_helper.mli"
  "parsing/docstrings.ml"
  "parsing/docstrings.mli"
  "parsing/location.ml"
  "parsing/location.mli"
  "parsing/longident.ml"
  "parsing/longident.mli"
  "parsing/parsetree.mli"
  "parsing/pprintast.ml"
  "parsing/pprintast.mli"
  "parsing/printast.ml"
  "parsing/printast.mli"
  "parsing/syntaxerr.ml"
  "parsing/syntaxerr.mli"
)

TARBALL=$(basename ${OCAML_URL})
OCAML_DIR="ocaml"

# Checking that the [$OCAML_DIR] directory does not exist yet.
if [[ -d ${OCAML_DIR} ]]; then
  echo "The directory ${OCAML_DIR} already exists. Delete it and re-run."
  exit 1
fi

# Checking that the [$OCAML_DIR] directory does not exist yet.
if [[ -d ${SRC_DIR} ]]; then
  echo "The directory ${SRC_DIR} already exists. Delete it and re-run."
  exit 1
fi

# Download the [$TARBALL] if necessary.
if [[ ! -f ${TARBALL} ]]; then
  echo "Source origin: ${OCAML_URL}"
  echo -n "Downloading tarball ... "
  wget -q ${OCAML_URL}
  echo "[OK]"
fi

# Unpacking the archive to the [$OCAML_DIR] directory.
echo -n "Unpacking tarball  ... "
mkdir ${OCAML_DIR}
tar -xzf ${TARBALL} -C ${OCAML_DIR} --strip-components=1
echo "[OK]"

# Creating the new [$SRC_DIR] directory.
mkdir ${SRC_DIR}

# Extracting the necessary source files from the [$OCAML_DIR] directory.
echo -n "Extracting files   ... "
for FILE in ${COPIED[@]}; do
  cp ${OCAML_DIR}/${FILE} ${SRC_DIR}
done
echo "[OK]"

# No need for the [$OCAML_DIR] any nore, cleaning up.
rm -r ${OCAML_DIR}

# Applying patches.
echo -n "Applying patches   ... "
for PATCH in $(find patch -name "*.patch"); do
  BASE=$(basename ${PATCH})
  FILE="${SRC_DIR}/${BASE%.patch}"
  patch ${FILE} -s -i ${PATCH} -o ${FILE}.tmp
  mv ${FILE}.tmp ${FILE}
done
echo "[OK]"

# It only remains to generate some files.
echo -n "Generating files   ... "

# Generate minimal version of [$OCAML_DIR/util/clflags.ml]
cat > $SRC_DIR/clflags.ml <<- "EOF"
let unsafe : bool ref = ref false
let absname : bool ref = ref false
let applicative_functors : bool ref = ref true
let error_style : Misc.Error_style.setting option ref = ref None
EOF

# Generate the dune file.
cat > $SRC_DIR/dune <<- "EOF"
(library
 (name ocaml_ast)
 (flags (:standard -w -9-20))
 (modules :standard)
 (modules_without_implementation asttypes parsetree)
 (wrapped false)
 (libraries stdlib-shims))
EOF

echo "[OK]"
