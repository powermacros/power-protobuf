use power_porotbuf::protobuf;

protobuf! {
    syntax = "proto3";

    package scope1;

    message Message1 {
        string content
    }
}

protobuf! {
    message MessageinAnotherScope {
        scope1.Message1 inner;
        self.scope1.Message1 inner2;
    }
}

fn main() {}
