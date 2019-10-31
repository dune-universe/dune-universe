[%%cstruct
type ethernet = {
    dst: uint8_t        [@len 6];
    src: uint8_t        [@len 6];
    ethertype: uint16_t;
  } [@@big_endian]
]

let ethertype_to_int = function
  | `ARP -> 0x0806
  | `IPv4 -> 0x0800
  | `IPv6 -> 0x86dd

let int_to_ethertype = function
  | 0x0806 -> Some `ARP
  | 0x0800 -> Some `IPv4
  | 0x86dd -> Some `IPv6
  | _ -> None
