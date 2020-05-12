exception Invalid_sentence;;
type mag_var = float * Coord.ew;;

type gpgga = {
  time: float;
  coord: Coord.t;
  quality: int;
  sat_n: int;
  hdop: float;
  alt: float;
  geoid_height: float;
  station_id: string;
};;

type gpgll = {
  time: float;
  coord: Coord.t;
  status: bool;
};;

type gprmc = {
  time: float;
  coord: Coord.t;
  sog: float;
  cmg: float;
  mag_var: mag_var;
  status: bool;
};;

type sat = {
  prn: int;
  elev_dgr: int;
  azimuth: int;
  snr_db: int;
};;

type gpgsv = {
  msg_n: int;
  msg_i: int;
  sv_n: int;
  sats: sat list;
};;

type gpgsa = {
  auto: bool;
  fix: int;
  prns: int list;
  pdop: float;
  hdop: float;
  vdop: float;
};;

type t = GPGLL of gpgll | GPGGA of gpgga | GPRMC of gprmc | GPGSV of gpgsv | GPGSA of gpgsa;;

let to_string s = match s with 
| GPGLL s -> Printf.sprintf "GPGLL(%s)" (Coord.to_string s.coord);
| GPGGA s -> Printf.sprintf "GPGGA(%s)" (Coord.to_string s.coord);
| GPRMC s -> Printf.sprintf "GPRMC(%s)" (Coord.to_string s.coord);
| GPGSV s -> Printf.sprintf "GPGSV()"
| GPGSA s -> Printf.sprintf "GPGSA()"
;;

let time_to_unix t =
  let tm = Unix.time () |> Unix.localtime in
  let hour = t / 10000 in
  let min = (t / 100) - (hour * 100) in
  let sec = t - (hour * 10000) - (min * 100) in 
  { tm with tm_hour= hour; tm_min= min; tm_sec= sec } |> Unix.mktime |> fst
;;

let datetime_to_unix d t = 
  let tm = time_to_unix t |> Unix.localtime in
  let day = d / 10000 in
  let month = d / 100 - (day * 100) in
  let year = d - (day * 10000) - (month * 100) in 
  { tm with tm_mday= day; tm_mon= month; tm_year= 2000 + year } |> Unix.mktime |> fst
;;