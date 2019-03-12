open Core

let register_transformation (module M: S.Component) = M.register ()

let register_transformations =
    List.iter
    ~f:register_transformation

let () = register_transformations [ (module Router_ext) ]
