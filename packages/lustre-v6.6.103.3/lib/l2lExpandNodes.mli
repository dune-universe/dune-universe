(* Time-stamp: <modified the 02/10/2014 (at 10:52) by Erwan Jahier> *)

(** Expand user nodes



   Expand nodes:
   ------------

   if n is a node defined as follows

     node n(x,y) returns(a,b);
     var v1,v2;
     let
       v1 = x+y;
       v2 = x*y;
       a = v1*x;
       b = v2*y;
     tel
   
   equations such as :

      o1,o2 = n(i1, i2);

   becomes
   
       h1 = i1+i2;
       h2 = i1*i2;
       o1 = h1*i1;
       o2 = h2*i2;
     
   where h1 and h2 are fresh local vars

   In other terms, we need to 
   - create a fresh local var for each local var of n
   - take all the equations of n, and substitute
      - the inputs parameters by intput args
      - the outputs parameters by lhs list
      - the local vars by the fresh local var names


  nb : to simplify, for equations like

      f.a = n(t[1], 3);
   
    we first create fresh vars for "f.a", "t[1]", and "3"

      _v1 = t[1];
      _v2 = 3;
      f.a = _v3;
      _v3 = n(_v1,_v2);
   
    and then we apply the transformation

   Expand assertions:
   -----------------

   In order to deal with assertions on nodes, i.e., of the form

     assert (n(i1,i2));

   we first transform it into
   
     assert_var = n(i1,i2);
     assert(assert_var);

   where assert_var is a fresh bool local var, and we apply the transformation
   on the first equation.
 *)


(** nb : it performs no fixpoint, so nested calls won't be expanded
    entirely unless L2lSplit.doit has been call before. 
    
    I could remove that restriction by adding a fixpoint somewhere, 
    but is it worth bothering ?

    the first arf is the list of node to not expand
*)
val doit : Lic.node_key list -> LicPrg.t -> LicPrg.t
