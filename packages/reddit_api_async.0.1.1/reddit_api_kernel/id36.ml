open! Core_kernel
include Id36_intf

module M = struct
  type t = Int63.t [@@deriving compare, hash, bin_io]

  let of_int63 = ident
  let to_int63 = ident
  let base = Int63.of_int 36

  let to_string i =
    let rec of_int i acc =
      match Int63.equal i Int63.zero with
      | true ->
        (match acc with
        | [] -> "0"
        | _ -> String.of_char_list acc)
      | false ->
        let current_place = Option.value_exn (Int63.to_int Int63.O.(i % base)) in
        let character =
          let char_from_base base offset = Char.to_int base + offset |> Char.of_int_exn in
          match current_place < 10 with
          | true -> char_from_base '0' current_place
          | false -> char_from_base 'a' (current_place - 10)
        in
        of_int Int63.O.(i / base) (character :: acc)
    in
    of_int i []
  ;;

  let of_string t : t =
    let convert_char c =
      let convert_to_offset base_char = Char.to_int c - Char.to_int base_char in
      Int63.of_int
        (match Char.is_alpha c with
        | true -> convert_to_offset 'a' + 10
        | false -> convert_to_offset '0')
    in
    String.fold t ~init:Int63.zero ~f:(fun acc c ->
        Int63.O.((acc * base) + convert_char c))
  ;;

  let sexp_of_t t = to_string t |> sexp_of_string
  let t_of_sexp sexp = String.t_of_sexp sexp |> of_string
  let module_name = "Id36"
end

include M
include Identifiable.Make (M)
