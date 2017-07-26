(**
   This is the CRC16 algorithm used by Redis Cluster to hash keys.
   Implementation according to CCITT standards.

   This is actually the XMODEM CRC 16 algorithm, using the
   following parameters:

   Name                       : "XMODEM", also known as "ZMODEM", "CRC-16/ACORN"
   Width                      : 16 bit
   Poly                       : 1021 (That is actually x^16 + x^12 + x^5 + 1)
   Initialization             : 0000
   Reflect Input byte         : False
   Reflect Output CRC         : False
   Xor constant to output CRC : 0000
   Output for "123456789"     : 31C3
*)
val crc16 : string -> int
