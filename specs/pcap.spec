expect-enum "magic" uint32 hex
  [ 0xa1b2c3d4  # same byte order - microsecond resolution
  , 0xa1b23d4d  # same byte order - nanosecond resolution
  , 0xd4c3b2a1  # different byte order - microsecond resolution
  , 0x4d3cb2a1  # different byte order - nanosecond resolution
  ];
if ("magic" == 0xd4c3b2a1 || "magic" == 0x4d3cb2a1)
{
  byte-order-swap;
}
expect-value "major-version" uint16;
expect-value "minor-version" uint16;
expect-value "this-timezone" uint32;
expect-const uint32 0;
expect-value "snapshot-length" uint32;
expect-value "data-link-type" uint32
  [ 0   -> "Null"
  , 1   -> "Ethernet"
  , 101 -> "Raw IP"
  ];
sequence "packet" {
  expect-value "timestamp-seconds" uint32;
  expect-value "timestamp-microseconds" uint32;
  expect-value "captured-size" uint32;
  expect-value "actual-size" uint32;

  byte-order-big-endian;

  if ("data-link-type" == 1)
  {
    try ["pcap/ethernet"];
  }
  else
  {
    expect-data "data" of-length "captured-size";
  }
}
