expect-value "port-source" uint16;
expect-value "port-destination" uint16;
expect-value "length" uint16;
expect-value "checksum" uint16 hex;

let "payload-length" = "length" - 8;
if ("payload-length" > 0)
{
  try [ "pcap/ethernet/ipv4/udp/dns"
      , "pcap/ethernet/ipv4/udp/dhcp"
      ];
}
