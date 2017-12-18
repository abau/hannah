expect-value "port-source" uint16;
expect-value "port-destination" uint16;
expect-value "sequence-number" uint32;
expect-value "acknowledgement-number" uint32;
expect-value [ 4 -> "data-offset"
             , 3 -> "reserved"
             , 1 -> "nonce-sum"
             , 1 -> "congestion-window-reduced"
             , 1 -> "explicit-congestion-notification"
             , 1 -> "urgent"
             , 1 -> "acknowledgement"
             , 1 -> "push"
             , 1 -> "reset"
             , 1 -> "synchronize"
             , 1 -> "fin"
             ] uint16;
expect-value "window-size" uint16;
expect-value "checksum" uint16 hex;
expect-value "urgent-pointer" uint16;

if ("data-offset" > 5)
{
  let "options-length" = ("data-offset" - 5) * 4;
  expect-data "options" of-length "options-length";
}

let "payload-length" = "total-length" - (4 * "internet-header-length") - (4 * "data-offset");
if ("payload-length" > 0)
{
  try ["pcap/ethernet/ipv4/tcp/ascii"];
}
