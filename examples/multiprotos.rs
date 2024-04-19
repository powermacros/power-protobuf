use power_porotbuf::protobuf;

protobuf! {
    syntax = "proto3";

    package scope1;

    message Message1 {
        string content
    }

    message IpAddress {
        oneof version {
          fixed32 ipv4 = 1;
          Ipv6 ipv6 = 2;
        }
      }

      message Ipv6 {
        uint32 ab = 1;
        uint32 cd = 2;
        uint32 ef = 3;
        uint32 gh = 4;
      }
}

protobuf! {
    message MessageinAnotherScope {
        scope1.Message1 inner;
        self.scope1.Message1 inner2;
    }
}

fn main() {}
