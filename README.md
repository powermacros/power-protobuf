# protobuf!

power_protobuf is a proc-macro to let you write protobuf in rust source code directly like this way!:

```rust
protobuf! {
  syntax = "proto3";

  package helloworld;

  option java_multiple_files = true;
  option java_package = "io.grpc.examples.helloworld";
  option java_outer_classname = "HelloWorldProto";

  // The greeting service definition.
  service Greeter {
    // Sends a greeting
    rpc SayHello ({
      // 'simplize_rpc_params' feature simplize to define Request/Response messages for rpc
      string name
    }) returns (HelloReply) {}
  }

  // The response message containing the greetings
  message HelloReply {
    string message // 'tagless' feature omit to assign tag number at design stage, it will do auto-increment as by refering previous number
  }
}
```

# features

## support proto2 and proto3 both

both protobuf version (2 to 3 yet) has been supported.

`protobuf!` also allows you not to declare the version explicitly, it will parse as **proto3 syntax by default**.

## translates package name as inner mod for rust

`package` will be generated into a rust `mod` in current source file if you define it. And by opposite, it expand to current place as not deinfed.

## support tagless

use `protobuf!` you can write protocol like this:

```rust
message A {
  int32 field1,
  string field2,

  repeated string field10 = 10,
  string field11
  sfix32 field12
}
```

the tags of field1, field2, field11, field12 will be assign as '1', '2', '11', '12' automatically.

## support to define rpc request/response message types directly

defines Request and Response message types for multiple rpc methods would be a redundant work for sometime.

`protobuf!` provides an option to simplize this:

```rust
  service Greeter {
    rpc SayHello ({
      string name
    }) returns (HelloReply) {}
  }
```

message `SayHelloRequest` will be generated as fields defined in `{}`. the type name for request/response combines rpc name and suffix(`Request`/`Response`) in upper camel case.

## support type reference cross multiple .rs files

`protobuf!` let's you easy to use external types in other protobuf definitions. it will check the type of fields by reading corresponding .rs files.

FIXME: more details will be introduced in this doc later.

## support tonic grpc stack

enable feature `impl_prost` to expand codes using prost/tonic stack.

the `impl_prost` feature is enabled defaultly.

## current state

| features                         | progress |
| -------------------------------- | -------- |
| proto2 and proto3 syntax         | Done     |
| tagless                          | Done     |
| free request/response param type | Done     |
| support prost                    | Partly   |
| support rust-protobuf            | Not yet  |
| stream support                   | Not yet  |
