expect-value "operation" uint8 [ 1 -> "Request"
                               , 2 -> "Reply"
                               ];
expect-value "hardware-type" uint8 [ 1 -> "Ethernet" ];
expect-value "hardware-address-length" uint8;
expect-value "hops" uint8;
expect-value "transaction-id" uint32 hex;
expect-value "seconds-elapsed" uint16;
expect-value [ 1 -> "broadcast" ] uint16;

expect-value "client-ip-address" uint8 of-length 4;
expect-value "your-client-ip-address" uint8 of-length 4;
expect-value "next-server-ip-address" uint8 of-length 4;
expect-value "relay-agent-ip-address" uint8 of-length 4;
expect-value "client-hardware-adress" uint8 of-length "hardware-address-length" hex;
let "client-hardware-address-padding-length" = 16 - "hardware-address-length";
expect-value "client-hardware-adress-padding" uint8 of-length "client-hardware-address-padding-length" hex;
expect-ascii "server-host-name" of-length 64;
expect-ascii "boot-file-name" of-length 128;
expect-const uint32 0x63825363;

sequence "options"
{
  expect-value "code" uint8 [ 0   -> "Pad"
                            , 1   -> "Subnet mask"
                            , 2   -> "Time offset"
                            , 3   -> "Router"
                            , 4   -> "Time server"
                            , 5   -> "Name server"
                            , 6   -> "Domain name server"
                            , 7   -> "Log server"
                            , 8   -> "Cookie server"
                            , 9   -> "LPR server"
                            , 10  -> "Impress server"
                            , 11  -> "Resource location server"
                            , 12  -> "Host name"
                            , 13  -> "Boot file size"
                            , 14  -> "Merit dump file"
                            , 15  -> "Domain name"
                            , 16  -> "Swap server"
                            , 17  -> "Root path"
                            , 18  -> "Extension path"
                            , 19  -> "IP forwarding"
                            , 20  -> "Non-local source routing"
                            , 21  -> "Policy filter"
                            , 22  -> "Maximum datagram reassembly size"
                            , 23  -> "Default IP time-to-live"
                            , 24  -> "Path MTU aging timeout"
                            , 25  -> "Path MTU plateau table"
                            , 26  -> "Interface MTU"
                            , 27  -> "All subnets are local"
                            , 28  -> "Broadcast address"
                            , 29  -> "Perform mask discovery"
                            , 30  -> "Mask supplier"
                            , 31  -> "Perform router discovery"
                            , 32  -> "Router solicitation address"
                            , 33  -> "Static route"
                            , 34  -> "Trailer encapsulation option"
                            , 35  -> "ARP cache timeout"
                            , 36  -> "Ethernet encapsulation"
                            , 37  -> "TCP default TTL"
                            , 38  -> "TCP keepalive interval"
                            , 39  -> "TCP keepalive garbage"
                            , 40  -> "Network information service domain"
                            , 41  -> "Network information servers"
                            , 42  -> "Network time protocol servers"
                            , 43  -> "Vendor specific information"
                            , 44  -> "Netbios over TCP/IP name server"
                            , 45  -> "Netbios over TCP/IP datagram distribution server"
                            , 46  -> "Netbios over TCP/IP node type"
                            , 47  -> "Netbios over TCP/IP scope"
                            , 48  -> "X window system font server"
                            , 49  -> "X window system display manager"
                            , 64  -> "Network information service+ domain"
                            , 65  -> "Network information service+ servers"
                            , 68  -> "Mobile IP home agent"
                            , 69  -> "Simple mail transport protocol (SMTP) server"
                            , 70  -> "Post office protocol (POP3) server"
                            , 71  -> "Network news transport protocol (NNTP) server"
                            , 72  -> "Default world wide web (WWW) server"
                            , 73  -> "Default finger server"
                            , 74  -> "Default internet relay chat (IRC) server"
                            , 75  -> "Streettalk server"
                            , 76  -> "Streettalk directory assistance (STDA) server"
                            , 50  -> "Requested IP address"
                            , 51  -> "IP address lease time"
                            , 52  -> "Option overload"
                            , 53  -> "DHCP message type"
                            , 54  -> "Server identifier"
                            , 55  -> "Parameter request list"
                            , 56  -> "Message"
                            , 57  -> "Maximum DHCP message size"
                            , 58  -> "Renewal time value"
                            , 59  -> "Rebinding time value"
                            , 60  -> "Vendor class identifier"
                            , 61  -> "Client-identifier"
                            , 66  -> "TFTP server name"
                            , 67  -> "Bootfile name"
                            , 255 -> "End"
                            ];
  if ("code" > 0 && "code" < 255)
  {
    expect-value "length" uint8;
    expect-data "data" of-length "length";
  }
} while ("code" != 255);

let "padding-length" = "payload-length" - file-position;
if ("padding-length" > 0)
{
  expect-data "padding" of-length "padding-length";
}
