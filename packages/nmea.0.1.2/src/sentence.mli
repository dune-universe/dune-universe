exception Invalid_sentence
type mag_var = float * Coord.ew

type gpgga = {
  time: float;
  coord: Coord.t;
  quality: int;
  sat_n: int;
  hdop: float;
  alt: float;
  geoid_height: float;
  station_id: string;
}

type gpgll = {
  time: float;
  coord: Coord.t;
  status: bool;
}

type gprmc = {
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

type gpgsv = {
  msg_n: int;
  msg_i: int;
  sv_n: int;
  sats: sat list;
}

type gpgsa = {
  auto: bool;
  fix: int;
  prns: int list;
  pdop: float;
  hdop: float;
  vdop: float;
}

type t = GPGLL of gpgll | GPGGA of gpgga | GPRMC of gprmc | GPGSV of gpgsv | GPGSA of gpgsa
(** sentence type *)

val to_string: t -> string
(** [to_string s] returns an human readable string for the given sentence *)

val time_to_unix: int -> float
(** [time_to_unix t] transforms nmea time format to unixtime *)

val datetime_to_unix: int -> int -> float
(** [datetime_to_unix d t] transforms nmea datetime format to unixtime *)
