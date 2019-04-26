[%%cenum
  type attr_t =
    | ORIGIN [@id 1]
    | AS_PATH
    | NEXT_HOP
    | MED
    | LOCAL_PREF
    | ATOMIC_AGGR
    | AGGREGATOR
    | COMMUNITY
    | MP_REACH_NLRI [@id 14]
    | MP_UNREACH_NLRI
    | EXT_COMMUNITIES
    | AS4_PATH
  [@@uint8_t]
]

[%%cenum
  type origin =
    | IGP
    | EGP
    | INCOMPLETE
  [@@uint8_t]
]

[%%cstruct
  type ft = {
    flags: uint8_t;
    tc: uint8_t;
    len: uint8_t;
  }
  [@@big_endian]
]

[%%cstruct
  type fte = {
    flags: uint8_t;
    tc: uint8_t;
    len: uint16_t
  }
  [@@big_endian]
]

[%%cenum
  type aspt =
    | AS_SET [@id 1]
    | AS_SEQ
  [@@uint8_t]
]

[%%cstruct
  type asp = {
    t: uint8_t;
    n: uint8_t;
  }
  [@@big_endian]
]

[%%cstruct
  type h = {
     marker: uint8_t; [@len 16]
     len: uint16_t;
     typ: uint8_t;
   }
  [@@big_endian]
]

[%%cenum
  type tc =
    | OPEN [@id 1]
    | UPDATE
    | NOTIFICATION
    | KEEPALIVE
  [@@uint8_t]
]

[%%cenum
  type cc =
    | MP_EXT                      [@id 1]
    | ROUTE_REFRESH
    | OUTBOUND_ROUTE_FILTERING
    | MULTIPLE_ROUTES_DESTINATION
    | EXT_HEXTHOP_ENC
    | GRACEFUL_RESTART            [@id 64]
    | AS4_SUPPORT
    | ENHANCED_REFRESH            [@id 70]
  [@@uint8_t]
]

[%%cstruct
  type mp_ext = {
    afi: uint16_t;
    safi: uint16_t;
  }
  [@@big_endian]
]

[%%cenum
  type oc =
    | RESERVED [@id 0]
    | AUTHENTICATION
    | CAPABILITY
  [@@uint8_t]
]

[%%cstruct
  type opent = {
    version: uint8_t;
    local_asn: uint16_t;
    hold_time: uint16_t;
    local_id: uint32_t;
    opt_len: uint8_t;
  }
  [@@big_endian]
]

[%%cenum
  type message_header_error_t =
    | CONNECTION_NOT_SYNCHRONIZED [@id 1]
    | BAD_MESSAGE_LENGTH
    | BAD_MESSAGE_TYPE
  [@@uint8_t]
]

[%%cenum
  type open_message_error_t =
    | UNSPECIFIC [@id 0]
    | UNSUPPORTED_VERSION_NUMBER
    | BAD_PEER_AS
    | BAD_BGP_IDENTIFIER
    | UNSUPPORTED_OPTIONAL_PARAMETER
    | UNACCEPTABLE_HOLD_TIME
  [@@uint8_t]
]

[%%cenum
  type update_message_error_t =
    | MALFORMED_ATTRIBUTE_LIST [@id 1]
    | UNRECOGNIZED_WELLKNOWN_ATTRIBUTE
    | MISSING_WELLKNOWN_ATTRIBUTE
    | ATTRIBUTE_FLAGS_ERROR
    | ATTRIBUTE_LENGTH_ERROR
    | INVALID_ORIGIN_ATTRIBUTE
    | INVALID_NEXT_HOP_ATTRIBUTE [@id 8]
    | OPTIONAL_ATTRIBUTE_ERROR
    | INVALID_NETWORK_FIELD
    | MALFORMED_AS_PATH
  [@@uint8_t]
]

[%%cenum
  type error_t =
    | MESSAGE_HEADER_ERROR [@id 1]
    | OPEN_MESSAGE_ERROR
    | UPDATE_MESSAGE_ERROR
    | HOLD_TIMER_EXPIRED
    | FINITE_STATE_MACHINE_ERROR
    | CEASE
  [@@uint8_t]
]

[%%cstruct
  type err = {
    ec: uint8_t;
    sec: uint8_t;
  }
  [@@big_endian]
]
