open Exenum_internals
       
let tester exen ?from ?upto ?verbose_period ?tos ~len f =
 Tester.gen_tester Lwt_io.print ?tos Lwt.bind Lwt.return_unit (Exenum.get_exen exen) ?from ?upto ?verbose_period ~len f
