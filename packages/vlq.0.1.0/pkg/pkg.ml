#use "topfind"
#require "topkg-jbuilder"

let () = Topkg_jbuilder.describe ~licenses:[ Topkg.Pkg.std_file "LICENSE" ] ()
