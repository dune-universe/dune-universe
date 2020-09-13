open Apron

include Domain.Make(struct
  type t = Oct.t
  let man = Oct.manager_alloc ()
end)

(* apron octagons do not feature the generator representation,
   so we convert them to polyhedra to handle generator-related operations *)

let to_generator_array oct =
  let pol = to_poly oct in
  Apol.to_generator_array pol

let to_generator_list oct =
  to_generator_array oct
  |> G.array_to_list

let of_generator_array (env : Environment.t) g =
  let gen_to_oct (g:Generator1.t) : t =
    let oct = ref (top env) in
    Generatorext.iter (fun coeff var ->
        let linexpr = Linexpr1.make env in
        Linexpr1.set_cst linexpr coeff;
        oct := assign_linexpr !oct var linexpr
      ) g;
    !oct
  in
  let gens = G.array_to_list g |> List.rev_map gen_to_oct in
  List.fold_left join (bottom env) gens

let of_generator_list (e : Environment.t) (g : Generator1.t list) =
  G.array_of_list g |> of_generator_array e

let to_vertices2D oct =
  let pol = to_poly oct in
  Apol.to_vertices2D pol
