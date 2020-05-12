%token <int> NAT
%token <float> REAL
%token <int> HEX
%token <string> ID

%token <Coord.ns> NS
%token <Coord.ew> EW
%token <string> UNIT

%token GPGGA GPRMC GPGLL GPGSV GPGSA
%token COMMA STAR SLASH
%token SPREFIX EOL

%start <Sentence.t> sentence

%%

sentence:
  | SPREFIX GPGGA COMMA nmea_gpgga_sentence EOL   { Sentence.GPGGA $4 }
  | SPREFIX GPRMC COMMA nmea_gprmc_sentence EOL   { Sentence.GPRMC $4 }
  | SPREFIX GPGLL COMMA nmea_gpgll_sentence EOL   { Sentence.GPGLL $4 }
  | SPREFIX GPGSV COMMA nmea_gpgsv_sentence EOL   { Sentence.GPGSV $4 }
  | SPREFIX GPGSA COMMA nmea_gpgsa_sentence EOL   { Sentence.GPGSA $4 }



nmea_gpgsa_sentence:
/* "$GPGSA,A,3,01,02,03,04,05,06,07,08,09,10,11,12,1.0,1.0,1.0*30"; */
  | UNIT COMMA NAT COMMA
    NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA 
    REAL COMMA REAL COMMA REAL checksum
    { Sentence.({
      auto = ($1 = "A");
      fix = $3;
      prns = [$5;$7;$9;$11;$13;$15;$17;$19;$21;$23;$25;$27];
      pdop = $29;
      hdop = $31;
      vdop = $33;
    })}


sat_info: 
  | NAT COMMA NAT COMMA NAT COMMA NAT { (Sentence.({ prn = $1; elev_dgr = $3; azimuth = $5; snr_db = $7; })) }
  | NAT COMMA NAT COMMA NAT COMMA     { (Sentence.({ prn = $1; elev_dgr = $3; azimuth = $5; snr_db = 0;  })) }

nmea_gpgsv_sentence:
  | NAT COMMA NAT COMMA NAT COMMA sat_info COMMA sat_info COMMA sat_info COMMA sat_info checksum
    { Sentence.({
        msg_n = $1;
        msg_i = $3;
        sv_n = $5;
        sats = [ $7; $9; $11 ];
    })}
  | NAT COMMA NAT COMMA NAT COMMA sat_info COMMA sat_info COMMA sat_info COMMA COMMA COMMA COMMA checksum
    { Sentence.({
        msg_n = $1;
        msg_i = $3;
        sv_n = $5;
        sats = [ $7; $9 ];
    })}
  | NAT COMMA NAT COMMA NAT COMMA sat_info COMMA sat_info COMMA COMMA COMMA COMMA COMMA COMMA COMMA COMMA checksum
    { Sentence.({
        msg_n = $1;
        msg_i = $3;
        sv_n = $5;
        sats = [ $7 ];
    })}


/* $GPGLL,4916.45,N,12311.12,W,225444,A*32 */
/* $GPGLL,,,,,082031.00,V,N*42 */
nmea_gpgll_sentence:
  /* coord       time       status*/
  | coords COMMA REAL COMMA UNIT checksum
    { Sentence.({
        time = Sentence.time_to_unix @@ int_of_float $3;
        status = $5 = "A";
        coord = $1;
    })}

nmea_gpgga_sentence:
  /* "$GPGGA,134658.00,5106.9792,N,11402.3003,W,2,09,1.0,1048.47,M,-16.27,M,08,AAAA*60" */
  /* time      coord        quality   sat_n     hdop       alt        M       geoid      M       -          - */
  | REAL COMMA coords COMMA NAT COMMA NAT COMMA REAL COMMA REAL COMMA UNIT COMMA REAL COMMA UNIT COMMA NAT COMMA ID checksum
    { Sentence.({
        time = Sentence.time_to_unix @@ int_of_float $1;
        coord = $3;
        quality = $5;
        sat_n = $7;
        hdop = $9;
        alt = $11;
        geoid_height = $15;
        station_id = "";
    })}
  /* "$GPGGA,100412.326,5231.139,N,01324.930,E,1,12,1.0,0.0,M,0.0,M,,*6F" */
  | REAL COMMA coords COMMA NAT COMMA NAT COMMA REAL COMMA REAL COMMA UNIT COMMA REAL COMMA UNIT COMMA COMMA checksum
    { Sentence.({
        time = Sentence.time_to_unix @@ int_of_float $1;
        coord = $3;
        quality = $5;
        sat_n = $7;
        hdop = $9;
        alt = $11;
        geoid_height = $15;
        station_id = "";
    })}
  /* "$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47" */
  | NAT COMMA coords COMMA NAT COMMA NAT COMMA REAL COMMA REAL COMMA UNIT COMMA REAL COMMA UNIT COMMA COMMA checksum
    { Sentence.({
        time = Sentence.time_to_unix $1;
        coord = $3;
        quality = $5;
        sat_n = $7;
        hdop = $9;
        alt = $11;
        geoid_height = $15;
        station_id = "";
    })}
  /* "$GPGGA,083224.00,,,,,0,00,99.99,,,,,,*69" */
  | REAL COMMA coords COMMA NAT COMMA NAT COMMA REAL COMMA COMMA COMMA COMMA COMMA COMMA checksum
    { Sentence.({
        time = Sentence.time_to_unix @@ int_of_float $1;
        coord = $3;
        quality = $5;
        sat_n = $7;
        hdop = 0.0;
        alt = 0.0;
        geoid_height = 0.0;
        station_id = "";
    })}

/* $GPRMC,083344.00,V,,,,,,,090520,,,N*7B */
nmea_gprmc_sentence:
  | REAL COMMA UNIT COMMA coords COMMA REAL COMMA REAL COMMA NAT COMMA REAL COMMA EW checksum
    { Sentence.({
        time = Sentence.datetime_to_unix $11 @@ int_of_float $1;
        status = $3 = "A";
        coord = $5;
        sog = $7;
        cmg = $9;
        mag_var = Coord.parse_lng $13 $15;
    })}
  | REAL COMMA UNIT COMMA coords COMMA COMMA COMMA NAT COMMA COMMA COMMA checksum
    { Sentence.({
        time = Sentence.datetime_to_unix $9 (int_of_float $1);
        status = $3 = "A";
        coord = $5;
        sog = 0.0;
        cmg = 0.0;
        mag_var = (0.0, E);
    })}


checksum:
  | COMMA? NS? STAR HEX  { $4 }
  | COMMA? NS? STAR NAT  { int_of_string (Printf.sprintf "0x%d" $4) }

coords:
  | REAL COMMA NS COMMA REAL COMMA EW
    { (Coord.parse_lat $1 $3),
      (Coord.parse_lng $5 $7) }
  | COMMA COMMA COMMA
    { (0.0, N), (0.0, W) }