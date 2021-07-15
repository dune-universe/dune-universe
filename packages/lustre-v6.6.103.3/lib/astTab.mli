(* Time-stamp: <modified the 29/08/2019 (at 14:53) by Erwan Jahier> *)

(** Tabulated version of the parse tree.

  - créée à partir de la liste des pack/modeles
  - s'occupe de l'instanciation (purement syntaxique) des modeles
  - crée pour chaque pack provided la liste ``brute'' des noms d'items
  exportés. Cette liste sera importante pour traiter les "use" lors de
  la création des tables de symboles de chaque pack
*)

type t

val create : AstV6.pack_or_model list -> t


(** accès aux infos *)
val pack_body_env : t -> Lv6Id.pack_name -> AstTabSymbol.t

(** A package may have no provided part *)
val pack_prov_env : t -> Lv6Id.pack_name -> AstTabSymbol.t option

(** Liste des noms de packs *)
val pack_list : t -> Lv6Id.pack_name list 

(** For debug.  *)
val dump : t -> unit

