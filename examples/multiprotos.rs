use power_porotbuf::protobuf;

protobuf! {
    syntax = "proto3";

    package scope1;

    message Message1 {
        string content
    }

    message IpAddress {
      oneof version {
        fixed32 ipv4;
        Ipv6 ipv6;
      }
    }

    message Ipv6 {
      uint32 ab;
      uint32 cd;
      uint32 ef;
      uint32 gh;
    }
}

protobuf! {
    message MessageinAnotherScope {
        optional scope1.Message1 inner;
        Enumeration1 inner2;
    }

    enum Enumeration1 {
      A;
      B;
      C;
      D;
    }
}

fn main() {}
