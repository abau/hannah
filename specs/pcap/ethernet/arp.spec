expect-value "hardware-type" uint16 [ 1 -> "Ethernet" ];
expect-value "protocol-type" uint16 hex [ 0x0800 -> "IPv4" ];
expect-value "hardware-length" uint8;
expect-value "protocol-length" uint8;
expect-value "operation" uint16 [ 1 -> "Request" 
                                , 2 -> "Reply"
                                , 3 -> "Request reverse"
                                , 4 -> "Reply reverse"
                                ];
expect-value "sender-hardware-adress" uint8 of-length "hardware-length" hex;
expect-value "sender-protocol-adress" uint8 of-length "protocol-length";
expect-value "target-hardware-adress" uint8 of-length "hardware-length" hex;
expect-value "target-protocol-adress" uint8 of-length "protocol-length";
