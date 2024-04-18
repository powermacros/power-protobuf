# protobuf!

power_protobuf is a proc-macro to let you write protobuf in rust source code directly like this way!:

```rust
protobuf! {
    syntax = "proto3";

    option java_multiple_files = true;
    option java_package = "io.grpc.examples.helloworld";
    option java_outer_classname = "HelloWorldProto";

    // The greeting service definition.
    service Greeter {
      // Sends a greeting
      rpc SayHello ({
        string name
      }) returns (HelloReply) {}
    }

    // The response message containing the greetings
    message HelloReply {
      string message
    }
}
```

## current state

| features                         | progress |
| -------------------------------- | -------- |
| proto2 and proto3 syntax         | Done     |
| tagless                          | Done     |
| free request/response param type | Done     |
| support prost                    | Partly   |
| support rust-protobuf            | Not yet  |
| stream support                   | Not yet  |

# release log

haven't any releases yet
