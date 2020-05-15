exception Invalid_sentence;;
type mag_var = float * Coord.ew;;
type mag_dev = float * Coord.ew;;

type gga = {
  time: float;
  coord: Coord.t;
  quality: int;
  sat_n: int;
  hdop: float;
  alt: float;
  geoid_height: float;
  station_id: string;
};;

type gll = {
  time: float;
  coord: Coord.t;
  status: bool;
};;

type rmc = {
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

type gsv = {
  msg_n: int;
  msg_i: int;
  sv_n: int;
  sats: sat list;
};;

type gsa = {
  auto: bool;
  fix: int;
  prns: int list;
  pdop: float;
  hdop: float;
  vdop: float;
};;

type hdg = {
	hdg: float;
	mag_dev: mag_dev;
	mag_var: mag_var;
};;

type zda = {
	time: float;
	tz: int;
}

type t = GLL of gll | GGA of gga | RMC of rmc | GSV of gsv | GSA of gsa | HDT of float | HDM of float 
	| HDG of hdg | ZDA of zda;;

let to_string s = match s with 
| GLL s -> Printf.sprintf "GLL(%s)" (Coord.to_string s.coord);
| GGA s -> Printf.sprintf "GGA(%s)" (Coord.to_string s.coord);
| RMC s -> Printf.sprintf "RMC(%s)" (Coord.to_string s.coord);
| GSV _ -> Printf.sprintf "GSV()"
| GSA _ -> Printf.sprintf "GSA()"
| HDT s -> Printf.sprintf "HDT(%f)" s
| HDM s -> Printf.sprintf "HDM(%f)" s
| HDG s -> Printf.sprintf "HDG(%f)" s.hdg
| ZDA s -> Printf.sprintf "HDG(%f, %d)" s.time s.tz
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

let datetime_to_unix2 day month year t = 
  let tm = time_to_unix t |> Unix.localtime in
  { tm with tm_mday= day; tm_mon= month; tm_year= year } |> Unix.mktime |> fst
;;