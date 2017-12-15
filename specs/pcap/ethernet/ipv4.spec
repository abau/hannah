expect-value [4 -> "version", 4 -> "internet-header-length"] uint8;
expect-value [6 -> "DSCP", 2 -> "ECN"] uint8 hex;
expect-value "total-length" uint16;
expect-value "identification" uint16 hex;
expect-value [ 1  -> "reserved"
             , 1  -> "do-not-fragment"
             , 1  -> "more-fragments"
             , 13 -> "fragment-offset"
             ] uint16 hex;
expect-value "time-to-live" uint8;
expect-enum "protocol" uint8 [ 0x06 -> "TCP" ];
expect-value "header-checksum" uint16 hex;
expect-value "ip-source" uint8 of-length 4;
expect-value "ip-destination" uint8 of-length 4;

if ("internet-header-length" > 5)
{
  let "options-length" = ("internet-header-length" - 5) * 4;
  expect-data "options" of-length "options-length";
}

if ("protocol" == 0x06)
{
  try ["pcap/ethernet/ipv4/tcp"];
}
