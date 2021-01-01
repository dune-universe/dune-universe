type ab = A | B of cd
and cd = C | D of ab

module Default = struct


end


module Attempt1 = struct
  open Printf

  class show_abcd ((fab : unit -> ab -> string), (fcd: unit -> cd -> string)) = object
    method c_A () (_:ab) = "A"
    method c_B () (_:ab) (cd: cd) : string = sprintf "C (%s)" (fcd () cd)
    method c_C () (_:cd) = "C"
    method c_D () (_:cd) (ab: ab) = sprintf "D (%s)" (fab () ab)
  end

  let gcata_ab tr i = function
    | A -> tr#c_A i A
    | B cd -> tr#c_B i (B cd) cd
  let gcata_cd tr i = function
    | C -> tr#c_C i C
    | D ab -> tr#c_D i (D ab) ab

  let fix_decl constr =
    let rec trait_ab inh subj =
      gcata_ab (constr (trait_ab, trait_cd)) inh subj
    and trait_cd inh subj = gcata_cd (constr (trait_ab, trait_cd)) inh subj in
    (trait_ab, gcata_ab)
      (* Тут по сути мы экономим только на том, что объект один, а не два *)

end



module Attempt2 = struct
  open Printf

  let gcata_ab tr i = function
    | A -> tr#c_A i A
    | B cd -> tr#c_B i (B cd) cd
  let gcata_cd tr i = function
    | C -> tr#c_C i C
    | D ab -> tr#c_D i (D ab) ab

  class show_abcd = object(tr)
    method c_A () (_:ab) = "A"
    method c_B () (_:ab) (cd: cd) : string = sprintf "C (%s)" (tr#gcata_cd () cd)
    method c_C () (_:cd) = "C"
    method c_D () (_:cd) (ab: ab) = sprintf "D (%s)" (tr#gcata_ab () ab)

    method gcata_ab = gcata_ab tr
    method gcata_cd = gcata_cd tr
  end


  let fix_abcd constr =
    let rec trait_ab inh subj =
      gcata_ab constr  inh subj
    and trait_cd inh subj = gcata_cd constr inh subj in
    (trait_ab, trait_cd)
      (* Тут по сути мы экономим только на том, что объект один, а не два *)

  let (show_ab, show_cd) = fix_abcd (new show_abcd)

  (* or more precisely *)
  let show_ab () x = gcata_ab (new show_abcd) () x
end
