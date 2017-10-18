
let getenv nix dos =
  try Sys.getenv nix
  with Not_found -> Sys.getenv dos

let opam =
  "opam-version: \"1.2\"
maintainer:  \"" ^ getenv "USER" "%USERNAME%" ^ "\"
authors:     [\"\"]
license:     \"\"

homepage:    \"\"
dev-repo:    \"\"
bug-reports: \"\"
doc:         \"\"

build: [ [ \"jbuilder\" \"subst\" {pinned} ]
         [ \"jbuilder\" \"build\" \"-p\" name \"-j\" jobs ] ]

depends: [ 
    
]
"

let jbuild_exe name =
  "(jbuild_version 1)

(executable
  ((name main)
   (package " ^ name ^ ")
   (public_name " ^ name ^ ")
   (libraries ())))"

let jbuild_lib name =
  "(jbuild_version 1)
(library
  (name " ^ name ^ ")
  (public_name " ^ name ^ ")
  (synopsis \"\")
  (libraries ()))"

let pkg =
  "#use \"topfind\"\n#require \"topkg-jbuilder.auto\""

let merlin =
  "S src\nB _build"

let main =
  "let () = print_endline \"Hello, world!\""
