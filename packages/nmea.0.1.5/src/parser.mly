%token <int> NAT
%token <float> REAL
%token <int> HEX
%token <string> ID
// %token <string> TALKER

%token <Coord.ns> NS
%token <Coord.ew> EW
%token <string> UNIT

%token GGA RMC GLL GSV GSA GP
%token ZDA
%token VDM VDO
%token HDT HDM HDG
%token COMMA STAR SLASH
%token SPREFIX APREFIX EOL

%start <Sentence.t> sentence

%%

sentence:
  | SPREFIX GGA COMMA nmea_gga_sentence EOL    { Sentence.GGA $4 }
  | SPREFIX RMC COMMA nmea_rmc_sentence EOL    { Sentence.RMC $4 }
  | SPREFIX GLL COMMA nmea_gll_sentence EOL    { Sentence.GLL $4 }
  | SPREFIX GSV COMMA nmea_gsv_sentence EOL    { Sentence.GSV $4 }
  | SPREFIX GSA COMMA nmea_gsa_sentence EOL    { Sentence.GSA $4 }
  | SPREFIX HDT COMMA nmea_hdtm_sentence EOL   { Sentence.HDT $4 }
  | SPREFIX HDM COMMA nmea_hdtm_sentence EOL   { Sentence.HDM $4 }
  | SPREFIX HDG COMMA nmea_hdg_sentence EOL    { Sentence.HDG $4 }
  | SPREFIX ZDA COMMA nmea_zda_sentence EOL    { Sentence.ZDA $4 }

//   | APREFIX AI VDM COMMA nmea_vdm_sentence EOL   { Sentence.AIVDM $4 }
//   | APREFIX AI VDO COMMA nmea_vdo_sentence EOL   { Sentence.AIVDO $4 }

// nmea_avidm_sentence:


// nmea_avido_sentence:
	// !AIVDO,1,1,,,B39i>1000nTu;gQAlBj:wwS5kP06,0*5D



// "$GPZDA,172809.456,12,07,1996,00,00*45"
nmea_zda_sentence:
  | REAL COMMA NAT COMMA NAT COMMA NAT COMMA NAT COMMA NAT checksum 
	{ Sentence.({
		tz = $3;
		time = Sentence.datetime_to_unix2 $5 $7 $9 @@ int_of_float $1;
	})}

// $HCHDT,271.1,T*2C
nmea_hdtm_sentence:
  | REAL COMMA UNIT checksum 
	{ $1 }

// $HCHDG,85.5,0.0,E,0.0,E*77
// HCHDG,000.00,,,,*77
nmea_hdg_sentence:
  | REAL COMMA REAL COMMA EW COMMA REAL COMMA EW checksum 
	{ Sentence.({
	  hdg = $1;
	  mag_dev = Coord.parse_lng $3 $5;
      mag_var = Coord.parse_lng $7 $9;
	})}
  | REAL COMMA COMMA COMMA COMMA checksum 
	{ Sentence.({
	  hdg = $1;
	  mag_dev = (0.0, E);
      mag_var = (0.0, E);
	})}


nmea_gsa_sentence:
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

nmea_gsv_sentence:
  | NAT COMMA NAT COMMA NAT COMMA sat_info COMMA sat_info COMMA sat_info COMMA sat_info checksum
    { Sentence.({
        msg_n = $1;
        msg_i = $3;
        sv_n = $5;
        sats = [ $7; $9; $11; $13 ];
    })}
  | NAT COMMA NAT COMMA NAT COMMA sat_info COMMA sat_info COMMA sat_info COMMA? COMMA? COMMA? COMMA? checksum
    { Sentence.({
        msg_n = $1;
        msg_i = $3;
        sv_n = $5;
        sats = [ $7; $9; $11 ];
    })}
  | NAT COMMA NAT COMMA NAT COMMA sat_info COMMA sat_info COMMA? COMMA? COMMA? COMMA? COMMA? COMMA? COMMA? COMMA? checksum
    { Sentence.({
        msg_n = $1;
        msg_i = $3;
        sv_n = $5;
        sats = [ $7; $9 ];
    })}
  | NAT COMMA NAT COMMA NAT COMMA sat_info COMMA?  COMMA? COMMA? COMMA? COMMA? COMMA? COMMA? COMMA? COMMA? checksum
    { Sentence.({
        msg_n = $1;
        msg_i = $3;
        sv_n = $5;
        sats = [ $7; ];
    })}


/* $GPGLL,4916.45,N,12311.12,W,225444,A*32 */
/* $GPGLL,,,,,082031.00,V,N*42 */
/* $GPGLL,3913.09137,N,00908.43818,E,075602.00,A,A*6C */
nmea_gll_sentence:
  /* coord       time       status*/
  | coords COMMA REAL COMMA UNIT checksum
    { Sentence.({
        time = Sentence.time_to_unix @@ int_of_float $3;
        status = $5 = "A";
        coord = $1;
    })}
  | coords COMMA REAL COMMA UNIT COMMA UNIT checksum
    { Sentence.({
        time = Sentence.time_to_unix @@ int_of_float $3;
        status = $5 = "A";
        coord = $1;
    })}

nmea_gga_sentence:
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
nmea_rmc_sentence:
  | REAL COMMA UNIT COMMA coords COMMA REAL COMMA REAL COMMA NAT COMMA REAL COMMA EW checksum
    { Sentence.({
        time = Sentence.datetime_to_unix $11 @@ int_of_float $1;
        status = $3 = "A";
        coord = $5;
        sog = $7;
        cmg = $9;
        mag_var = Coord.parse_lng $13 $15;
    })}
	/* $GPRMC,082538.00,A,3913.08527,N,00908.43682,E,0.807,,120520,,,A*75 */
  | REAL COMMA UNIT COMMA coords COMMA REAL COMMA COMMA NAT COMMA COMMA COMMA UNIT checksum
    { Sentence.({
        time = Sentence.datetime_to_unix $10 @@ int_of_float $1;
        status = $3 = "A";
        coord = $5;
        sog = $7;
        cmg = 0.0;
        mag_var = (0.0, E);
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