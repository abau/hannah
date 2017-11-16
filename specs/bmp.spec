expect-const uint8 0x42;                          # B
expect-const uint8 0x4d;                          # M

expect-value "size" uint32;
expect-value "reserved" uint32;
expect-value "image-data-offset" uint32;

expect-const uint32 40;                           # size of info header
expect-value "width" uint32;
expect-value "height" uint32;
expect-const uint16 1;                            # number of color planes
expect-const uint16 24;                           # bits per pixel
expect-const uint32 0;                            # no compression
expect-value "image-data-size" uint32;            # image data size
expect-value "horizontal-pixel-per-meter" int32;
expect-value "vertical-pixel-per-meter" int32;
expect-const uint32 0;                            # no color palette
expect-const uint32 0;                            # no important colors

sequence "row" of-length "height"
{
  sequence "pixel" of-length "width"
  {
    expect-value "color" uint8 of-length 3;
  }
  expect-const uint16 0;                          # padding
}
