expect-enum uint32 "magic" hex
  [ 0xa1b2c3d4  # same byte order - microsecond resolution
  , 0xa1b23d4d  # same byte order - nanosecond resolution
  , 0xd4c3b2a1  # different byte order - microsecond resolution
  , 0x4d3cb2a1  # different byte order - nanosecond resolution
  ];
if ("magic" == 0xd4c3b2a1 || "magic" == 0x4d3cb2a1)
{
  byte-order-swap
}
else if ("magic" == 0xd4c3b2a1 || "magic" == 0x4d3cb2a1)
{
  byte-order-swap
}
expect-value uint16 "major-version";
expect-value uint16 "minor-version";
expect-value uint32 "this-timezone";
expect-const uint32 0;
expect-value uint32 "snapshot-length";
expect-value uint32 "data-link-type"
  [ 0   -> "Null"
  , 1   -> "Ethernet"
  , 101 -> "Raw IP"
  ];
sequence "packet" {
  expect-value uint32 "timestamp-seconds";
  expect-value uint32 "timestamp-microseconds";
  expect-value uint32 "captured-size";
  expect-value uint32 "actual-size";
  expect-data "data" of-length "captured-size";
}
