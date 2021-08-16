module T1 = struct
  @type ('a, 'b) a = B of ('a, 'b) b | D of 'a
  (* and ('a, 'b) c = ('a, 'b) b *)
  and ('a, 'b) b = A of ('a, 'b) a | C of 'a * 'b | E
  with show
end

(* module T2 = struct
 *   (\* Lacmus test about nonregular types:
 *      leaving mutally recursive classes here strikes regularity restriction *\)
 *   @type mmm = GT.int
 *   and uuu = GT.bool
 *   and www = GT.string
 *   and 'a class_infos = 'a
 *   and class_description = GT.int class_infos
 *   and zzz = GT.char
 *   and class_declaration = GT.string class_infos
 *   with fmt
 * end
 * 
 * (\*
 * type ('a, 'b) a = B of ('a, 'b) b | D of 'a (\* | F of ('a, 'b) a *\)
 * and  ('a, 'b) b = A of ('a * 'a, 'b) a | C of 'a * 'b | E
 *    doesn't work because of non-regualrity
 * *\)
 * open GT
 * module T3 = struct
 *   (\* this is a test about order of functions in _stub classes *\)
 *   @type core_type = CT
 *   and class_type = XXX
 *   and class_declaration = float class_infos
 *   and 'a class_infos = core_type list
 *     with fmt
 * end *)
