#load "q_MLast.cmo";;

open Pa_gt.Plugin

let _ =
  register "show" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh         = T.id "unit"; 
          syn         = T.id "string";
          proper_args = List.map (fun (Variable (_, a)) -> a) d.type_args; 
          arg_img     = (fun _ -> T.id "string")
        }, 
        (fun env constr -> 
           let concat x y = E.app [E.lid "^"; x; y] in
           concat 
             (snd 
               (List.fold_left 
                  (fun (first, expr as acc) arg ->
                     let append e = 
                       false, concat expr (if first then e else concat (E.str ", ") e)
                     in
                     match arg with                     
                     | arg, Arbitrary ctyp -> 
                        (match ctyp with
                         | <:ctyp< $lid:tname$ >> -> 
                           (match tname with
                            | "int"    -> append (E.app [E.lid "string_of_int"; E.lid arg])
                            | "string" -> append (E.lid arg)
                            | _        -> acc
                           )
                         | _ ->  acc
			)
		     | arg, _ -> 
                        append (E.app [E.fx (E.lid arg); E.unit])
                  )         
                  (true, E.str (constr.constr ^ " ("))
                  constr.args 
               )
             )
             (E.str ")")
        )
       )
    )
