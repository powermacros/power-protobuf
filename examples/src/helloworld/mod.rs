use power_porotbuf::protobuf;

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
      string message = 1;
    }
}
