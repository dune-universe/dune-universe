let s1 = "No field 'install', but a field 'remove': install instructions \
          probably part of 'build'. Use the 'install' field or a .install \
          file"

let x =
  cond 40 `Warning
    "Package uses flags that aren't recognised by earlier versions in \
     OPAM 1.2 branch. At the moment, you should use a tag \"flags:foo\" \
     instead for compatibility"
    ~detail:alpha_flags
    (alpha_flags <> [])

let s2 = "bla bla
 bli bli \
blo"

let s3 = "\
"

let s4 = " \
"

let s5 = "  \
\
"

let s6 = "
"

let s7 = "  
"

let c1 = '
'

let x1 = f x '
' y
z

let zz = "\

s \
 \
 "
