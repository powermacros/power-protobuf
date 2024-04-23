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

// protobuf! {
//     message MessageinAnotherScope {
//         optional scope1.Message1 inner;
//         X x;
//     }

//     enum X {
//       A;
//       B;
//       C;
//       D;
//     }
// }

fn main() {}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(
    Clone,
    PartialEq,
    prost :: Message,
    serde :: Deserialize,
    serde ::
Serialize,
)]
pub struct MessageinAnotherScope {
    #[prost(message, optional, tag = "1")]
    pub inner: Option<crate::scope1::Message1>,
    #[prost(enumeration = "X", tag = "2")]
    pub x: i32,
}
#[derive(
    Clone,
    Copy,
    Debug,
    Eq,
    PartialEq,
    PartialOrd,
    Ord,
    Hash,
    prost ::
Enumeration,
    serde :: Deserialize,
    serde :: Serialize,
)]
#[repr(i32)]
pub enum X {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
}
impl X {
    #[doc = r" String value of the enum field names used in the ProtoBuf definition."]
    #[doc = r""]
    #[doc = r" The values are not transformed in any way and thus are considered stable"]
    #[doc = r"(if the ProtoBuf definition does not change) and safe for programmatic use."]
    pub fn as_str_name(&self) -> &'static str {
        match self {
            Self::A => "A",
            Self::B => "B",
            Self::C => "C",
            Self::D => "D",
        }
    }
    #[doc = r" Creates an enum from field names used in the ProtoBuf definition."]
    pub fn from_str_name(value: &str) -> Option<Self> {
        match value {
            "A" => Some(Self::A),
            "B" => Some(Self::B),
            "C" => Some(Self::C),
            "D" => Some(Self::D),
            _ => None,
        }
    }
}
