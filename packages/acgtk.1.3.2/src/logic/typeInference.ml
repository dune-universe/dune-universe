open UtilsLib.Utils
open Lambda

module Value =
struct

  type t=int
  type value = Lambda.stype
  let unfold stype _ =
    match stype with
    | Lambda.Atom _ -> None
    | Lambda.LFun (a,b) -> Some (1,[a;b])
    | Lambda.Fun (a,b) -> Some (1,[a;b])
    | _ -> failwith "Bug: No type inference on these types"
      

end



module UF=VarUnionFind.UF(Value)
  
module Type =
struct

  exception Not_typable

  type typing_env={l_level:int;
		   nl_level:int; 
		   lvar_typing:int IntMap.t;
		   nlvar_typing:int IntMap.t;
		   const_typing:(int*int) IntMap.t;
		   (* maps the occurrence position, which is unique,
		      to a pair consisting of the type variable and the
		      constant identifier *)
		   cst_nbr:int;
		   type_equations:UF.t;}
    
  let empty_env= {l_level=0;
		  nl_level=0;
		  lvar_typing=IntMap.empty;
		  nlvar_typing=IntMap.empty;
		  const_typing=IntMap.empty;
		  cst_nbr=0;
		  type_equations=UF.empty;}
    
  let type_equation_log prefix eq =
    log_iteration
      (fun s -> LOG "%s%s" prefix s LEVEL TRACE)
      (UF.to_string eq)

      
  let rec inference_aux level t ty_var env =
    let prefix=String.make (level*3) ' ' in
    LOG "%sType inference of %s (currently %d). Equations are:" prefix (Lambda.raw_to_string t) ty_var LEVEL TRACE ;
    type_equation_log prefix env.type_equations;
    let ty,new_env =
      match t with
      | Lambda.Var i ->
	(try
	   let ty_in_env=IntMap.find (env.nl_level-i-1) env.nlvar_typing in
	   LOG "%sAdding an equation (variable found in the environment) %d<-->%d" prefix ty_var ty_in_env LEVEL TRACE ;
	   let new_eq=UF.union ty_var ty_in_env env.type_equations in
	   ty_var,{env with type_equations=new_eq}
	 with
	 | Not_found -> 
	   let new_var,new_eq=UF.generate_new_var env.type_equations in
	   LOG "%sAdding a new variable %d and an equation" prefix new_var LEVEL TRACE ;
	   new_var,{env with nlvar_typing=IntMap.add i new_var env.nlvar_typing; type_equations=new_eq})
      | Lambda.LVar i -> 
	(try
	   let ty_in_env=IntMap.find (env.l_level-i-1) env.lvar_typing in
	   LOG "%sAdding an equation (Lvariable found in the environment) %d<-->%d" prefix ty_var ty_in_env LEVEL TRACE ;
	   let new_eq=UF.union ty_var ty_in_env env.type_equations in
	   ty_var,{env with type_equations=new_eq}
	 with
	 | Not_found -> 
	   let new_var,new_eq=UF.generate_new_var env.type_equations in
	   LOG "%sAdding a new Lvariable %d and an equation" prefix new_var LEVEL TRACE ;
	   new_var,{env with lvar_typing=IntMap.add i new_var env.lvar_typing; type_equations=new_eq})
      | Lambda.Const i ->
	(* Each occurence of a constants is considered as a new free
	   variables *) 
	let new_var,new_eq=UF.generate_new_var env.type_equations in
	let new_eq=UF.union ty_var new_var new_eq in
	new_var,{env with type_equations=new_eq;const_typing=IntMap.add (env.cst_nbr+1) (new_var,i) env.const_typing;cst_nbr=env.cst_nbr+1}
      | Lambda.DConst _ -> failwith "Bug: there should not remain any defined constant when  computing the principal type"
      | Lambda.Abs (x,t) ->
	LOG "%sType inference of an abstraction:" prefix LEVEL TRACE;
	let alpha,new_eq=UF.generate_new_var env.type_equations in
	LOG "%sAdded a variable at %d. Equations are:" prefix alpha LEVEL TRACE ;
	let () = type_equation_log prefix new_eq in
	let beta,new_eq=UF.generate_new_var new_eq in
	LOG "%sAdded a variable at %d. Equations are:" prefix beta LEVEL TRACE ;
	let () = type_equation_log prefix new_eq in
	let new_const,new_eq=UF.generate_new_constr new_eq (1,[alpha;beta]) in
	LOG "%sAdded new const at %d. Equations are:" prefix new_const LEVEL TRACE ;
	let () = type_equation_log prefix new_eq in
	LOG "%sPreparing a Union %d %d." prefix ty_var new_const LEVEL TRACE ;
	let new_eq=UF.union ty_var new_const new_eq in
	LOG "%sAdded a varibale at %d. Equations are:" prefix beta LEVEL TRACE ;
	type_equation_log prefix new_eq;
	let _,new_env=inference_aux (level+1) t beta {env with nl_level=env.nl_level+1;nlvar_typing=IntMap.add env.nl_level alpha env.nlvar_typing;type_equations=new_eq} in
	let is_cyclic,new_eq=UF.cyclic ty_var new_env.type_equations in
	ty_var,{env with type_equations=new_eq;const_typing=new_env.const_typing;cst_nbr=new_env.cst_nbr}
      | Lambda.LAbs (x,t) ->
	LOG "%sType inference of a linear abstraction:" prefix LEVEL TRACE;
	let alpha,new_eq=UF.generate_new_var env.type_equations in
	let beta,new_eq=UF.generate_new_var new_eq in
	let new_const,new_eq=UF.generate_new_constr new_eq (1,[alpha;beta]) in
	let new_eq=UF.union ty_var new_const new_eq in
	let _,new_env=inference_aux (level+1) t beta {env with l_level=env.l_level+1;lvar_typing=IntMap.add env.l_level alpha env.lvar_typing;type_equations=new_eq} in
	let is_cyclic,new_eq=UF.cyclic ty_var new_env.type_equations in
	ty_var,{env with type_equations=new_eq;const_typing=new_env.const_typing;cst_nbr=new_env.cst_nbr}
(*	ty_var,{new_env with type_equations=new_eq;lvar_typing=env.lvar_typing} *)
(*	ty_var,{new_env with type_equations=new_eq} *)
      | Lambda.App (t,u) ->
	let u_type,new_eq=UF.generate_new_var env.type_equations in
	let t_type,new_eq=UF.generate_new_constr new_eq (1,[u_type;ty_var]) in
	LOG "%sType inference of the parameter in an application:" prefix LEVEL TRACE;
	let u_type,new_env=inference_aux (level+1) u u_type {env with type_equations=new_eq} in 
	LOG "%sType inference of the functor in an application:" prefix LEVEL TRACE;
	let t_type,new_env=inference_aux (level+1) t t_type new_env in 
	ty_var,new_env
      | _ -> failwith "Bug: No principal typing algorithm for these types" in
    let is_cyclic,new_eq=UF.cyclic ty new_env.type_equations in
    if is_cyclic then
      raise Not_typable
    else
      ty,{new_env with type_equations=new_eq}
	
	
  let rec build_type i type_eq =
    let (i,v),type_eq = UF.find i type_eq in
    match v with
    | UF.Link_to j when j=i -> Lambda.Atom(-i)
    | UF.Link_to _ -> failwith "Bug: when UF.find returns a Link_to, it should be a Link_to itself"
    | UF.Value _ ->  failwith "Bug: when performing type inference for principal typing, no type constant should appear"
    | UF.Constr (c,[alpha;beta]) ->
      let alpha'=build_type alpha type_eq in
      let beta'=build_type beta type_eq in
      Lambda.Fun(alpha',beta')
    | UF.Constr _ -> failwith "Bug: when performing type inference for principal typing, the only allowd type construction is the arrow"
      
  let inference t =
    try
      let vars=UF.empty in
      let ty,vars=UF.generate_new_var vars in
      let ty,env=inference_aux 0 t ty {empty_env with type_equations=vars} in
      build_type ty env.type_equations,IntMap.map (fun (ty,i) -> Lambda.Const i,build_type ty env.type_equations) env.const_typing
    with
    | UF.Union_Failure -> raise Not_typable
end
