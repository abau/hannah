let "ethernet-start" = file-position;
expect-value "mac-destination" uint8 of-length 6 hex;
expect-value "mac-source" uint8 of-length 6 hex;
expect-enum "ethertype" uint16 hex [ 0x0800 -> "IPv4"
                                   , 0x0806 -> "ARP"
                                   ];
if ("ethertype" == 0x0800)
{
  try ["pcap/ethernet/ipv4"];
}
else if ("ethertype" == 0x0806)
{
  try ["pcap/ethernet/arp"];
}
let "ethernet-padding" = "captured-size" - (file-position - "ethernet-start");
if ("ethernet-padding" > 0)
{
  expect-data "padding" of-length "ethernet-padding";
}
