(* Time-stamp: <modified the 18/08/2017 (at 11:00) by Erwan Jahier> *)

(** Check that there is no dependancy loop between equations. 
 
This check is also done during the lic2soc translation, when ordering
equations. But in the ec mode, no soc is generated, and no such check
is done by ec tools either (exexe, ec2c, etc.).

Hence it is necessary to duplicate the work (done in ActionsDeps)
here.  Note that in ec mode, structs and arrays have been expanded,
which makes things easier.

XXX : Should I rather use Lic2soc.f to check loops?  

Yes. So this module is not used anymore.
*)

exception Error of (Lxm.t * string * LicPrg.t)

val doit :  LicPrg.t -> unit
