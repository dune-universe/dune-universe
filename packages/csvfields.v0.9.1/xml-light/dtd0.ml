
type dtd_child = Types.dtd_child =
	| DTDTag of string
	| DTDPCData
	| DTDOptional of dtd_child
	| DTDZeroOrMore of dtd_child
	| DTDOneOrMore of dtd_child
	| DTDChoice of dtd_child list
	| DTDChildren of dtd_child list

type dtd_element_type = Types.dtd_element_type =
	| DTDEmpty
	| DTDAny
	| DTDChild of dtd_child

type dtd_attr_default = Types.dtd_attr_default =
	| DTDDefault of string
	| DTDRequired
	| DTDImplied
	| DTDFixed of string

type dtd_attr_type = Types.dtd_attr_type =
	| DTDCData
	| DTDNMToken
	| DTDEnum of string list
	| DTDID
	| DTDIDRef

type dtd_item = Types.dtd_item =
	| DTDAttribute of string * string * dtd_attr_type * dtd_attr_default
	| DTDElement of string * dtd_element_type

type dtd = dtd_item list

