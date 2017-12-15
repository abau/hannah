expect-value "mac-destination" uint8 of-length 6 hex;
expect-value "mac-source" uint8 of-length 6 hex;
expect-enum "ethertype" uint16 hex [ 0x0800 -> "IPv4" ];

if ("ethertype" == 0x0800)
{
  try ["pcap/ethernet/ipv4"];
}
