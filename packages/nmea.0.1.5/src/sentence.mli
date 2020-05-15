exception Invalid_sentence
type mag_var = float * Coord.ew
type mag_dev = float * Coord.ew

type gga = {
  time: float;
  coord: Coord.t;
  quality: int;
  sat_n: int;
  hdop: float;
  alt: float;
  geoid_height: float;
  station_id: string;
}

type gll = {
  time: float;
  coord: Coord.t;
  status: bool;
}

type rmc = {
  time: float;
  coord: Coord.t;
  sog: float;
  cmg: float;
  mag_var: mag_var;
  status: bool;
}

type sat = {
  prn: int;
  elev_dgr: int;
  azimuth: int;
  snr_db: int;
}

type gsv = {
  msg_n: int;
  msg_i: int;
  sv_n: int;
  sats: sat list;
}

type gsa = {
  auto: bool;
  fix: int;
  prns: int list;
  pdop: float;
  hdop: float;
  vdop: float;
}

type hdg = {
	hdg: float;
	mag_dev: mag_dev;
	mag_var: mag_var;
}

type zda = {
	time: float;
	tz: int;
}

type t = GLL of gll | GGA of gga | RMC of rmc | GSV of gsv | GSA of gsa | HDT of float | HDM of float 
	| HDG of hdg | ZDA of zda
(** sentence type *)

val to_string: t -> string
(** [to_string s] returns an human readable string for the given sentence *)

val time_to_unix: int -> float
(** [time_to_unix t] transforms nmea time format to unixtime *)

val datetime_to_unix: int -> int -> float
(** [datetime_to_unix d t] transforms nmea datetime format to unixtime *)

val datetime_to_unix2: int -> int -> int -> int -> float
(** [datetime_to_unix2 day month year t] transforms nmea datetime format to unixtime *)
