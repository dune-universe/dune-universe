let writer_callback a d = Buffer.add_string a d ; String.length d

let init_conn url =
  let r = Buffer.create 16384 and c = Curl.init () in
  Curl.set_timeout c 1200 ;
  (*
  Curl.set_sslverifypeer c false;
  Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
  *)
  Printf.printf "11\n" ;
  Curl.set_writefunction c (writer_callback r) ;
  (*
  Curl.set_tcpnodelay c true;
  *)
  Curl.set_verbose c true ;
  (* Curl.set_post c false; *)
  Curl.set_upload c true ;
  Curl.set_url c url ;
  (r, c)

let counter = ref 0

let reader file maxBytes =
  let buffer = String.create maxBytes in
  let readBytes = input file buffer 0 maxBytes in
  if readBytes = 0 then ""
  else (
    counter := !counter + readBytes ;
    String.sub buffer 0 readBytes )

let put location file =
  Curl.global_init Curl.CURLINIT_GLOBALNOTHING ;
  let (r, conn) = init_conn location in
  Printf.printf "1\n" ;
  let fileContent = open_in file in
  Curl.set_readfunction conn (reader fileContent) ;
  Printf.printf "2\n" ;
  Curl.perform conn ;
  Printf.printf "Uploaded %d bytes\n" !counter ;
  let rc = Curl.get_responsecode conn in
  Curl.cleanup conn ;
  Curl.global_cleanup () ;
  (rc, Buffer.contents r)

(*
SERVER=cudf-solvers.irill.org
if bzip2 -c $1 | curl -f --data-binary @- http://$SERVER/cudf.bz2?criteria="$3" | bunzip2 -c > $2;
then exit 0
else echo FAIL > $2
fi
*)

let _ =
  let server = "cudf-solvers.irill.org" in
  let file = "/tmp/example.cudf.bz2" in
  let criteria = "-removed,-notuptodate,-changed" in
  let url =
    Printf.sprintf "http://%s/cudf.bz2?criteria=\"%s\"" server criteria
  in
  Printf.printf "%s\n" url ;
  let (r, c) = put url file in
  Printf.printf "%d -> %s\n" r c
