expect-value "identification" uint16;
expect-value [ 1 -> "reply"
             , 4 -> "flags"
             , 1 -> "authoritative-answer"
             , 1 -> "truncated"
             , 1 -> "recursion-desired"
             , 1 -> "recursion-available"
             , 1 -> "reserved"
             , 1 -> "authenticated-data"
             , 1 -> "checking-disabled"
             , 4 -> "return-code"
             ] uint16 hex;
expect-value "question-count" uint16;
expect-value "answer-count" uint16;
expect-value "authority-count" uint16;
expect-value "additional-count" uint16;

if ("question-count" > 0)
{
  sequence "questions" of-length "question-count"
  {
    sequence "labels"
    {
      expect-value "label-length" uint8;
      if ("label-length" > 0)
      {
        if ("label-length" <= 63)
        {
          expect-ascii "ascii" of-length "label-length";
        }
        else
        {
          expect-value "pointer" uint8 hex;
        }
      }
    } while ("label-length" != 0 && "label-length" <= 63);
    expect-value "type" uint16 [ 1   -> "A"
                               , 2   -> "NS"
                               , 5   -> "CNAME"
                               , 6   -> "SOA"
                               , 11  -> "WKS"
                               , 12  -> "PTR"
                               , 15  -> "MX"
                               , 28  -> "AAAA"
                               , 33  -> "SRV"
                               , 255 -> "*"
                               ];
    expect-value "class" uint16 [ 1 -> "Internet" ];
  }
}
if ("answer-count" > 0)
{
  sequence "answers" of-length "answer-count"
  {
    sequence "labels"
    {
      expect-value "label-length" uint8;
      if ("label-length" > 0)
      {
        if ("label-length" <= 63)
        {
          expect-ascii "ascii" of-length "label-length";
        }
        else
        {
          expect-value "pointer" uint8 hex;
        }
      }
    } while ("label-length" != 0 && "label-length" <= 63);
    expect-value "type" uint16 [ 1   -> "A"
                               , 2   -> "NS"
                               , 5   -> "CNAME"
                               , 6   -> "SOA"
                               , 11  -> "WKS"
                               , 12  -> "PTR"
                               , 15  -> "MX"
                               , 28  -> "AAAA"
                               , 33  -> "SRV"
                               , 255 -> "*"
                               ];
    expect-value "class" uint16 [ 1 -> "Internet" ];
    expect-value "time-to-live" uint32;
    expect-value "resource-data-length" uint16;
    expect-data "resource-data" of-length "resource-data-length";
  }
}
if ("authority-count" > 0)
{
  sequence "authorities" of-length "authority-count"
  {
    sequence "labels"
    {
      expect-value "label-length" uint8;
      if ("label-length" > 0)
      {
        if ("label-length" <= 63)
        {
          expect-ascii "ascii" of-length "label-length";
        }
        else
        {
          expect-value "pointer" uint8 hex;
        }
      }
    } while ("label-length" != 0 && "label-length" <= 63);
    expect-value "type" uint16 [ 1   -> "A"
                               , 2   -> "NS"
                               , 5   -> "CNAME"
                               , 6   -> "SOA"
                               , 11  -> "WKS"
                               , 12  -> "PTR"
                               , 15  -> "MX"
                               , 28  -> "AAAA"
                               , 33  -> "SRV"
                               , 255 -> "*"
                               ];
    expect-value "class" uint16 [ 1 -> "Internet" ];
    expect-value "time-to-live" uint32;
    expect-value "resource-data-length" uint16;
    expect-data "resource-data" of-length "resource-data-length";
  }
}
if ("additional-count" > 0)
{
  sequence "additional-info" of-length "additional-count"
  {
    sequence "labels"
    {
      expect-value "label-length" uint8;
      if ("label-length" > 0)
      {
        if ("label-length" <= 63)
        {
          expect-ascii "ascii" of-length "label-length";
        }
        else
        {
          expect-value "pointer" uint8 hex;
        }
      }
    } while ("label-length" != 0 && "label-length" <= 63);
    expect-value "type" uint16 [ 1   -> "A"
                               , 2   -> "NS"
                               , 5   -> "CNAME"
                               , 6   -> "SOA"
                               , 11  -> "WKS"
                               , 12  -> "PTR"
                               , 15  -> "MX"
                               , 28  -> "AAAA"
                               , 33  -> "SRV"
                               , 255 -> "*"
                               ];
    expect-value "class" uint16 [ 1 -> "Internet" ];
    expect-value "time-to-live" uint32;
    expect-value "resource-data-length" uint16;
    expect-data "resource-data" of-length "resource-data-length";
  }
}
