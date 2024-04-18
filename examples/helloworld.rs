use std::time::Duration;
use example::{greeter_server::{Greeter,GreeterServer}, HelloReply, SayHelloRequest, greeter_client::GreeterClient};
use power_porotbuf::protobuf;
use tokio::{spawn, time::sleep};
use tonic::{transport::Server, Request, Response, Status};

protobuf! {
    syntax = "proto3";

    package example;

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

#[derive(Debug, Default)]
pub struct MyGreeter {}

#[tonic::async_trait]
impl Greeter for MyGreeter {
    async fn say_hello(
        &self,
        request: Request<SayHelloRequest>, // Accept request of type HelloRequest
    ) -> Result<Response<HelloReply>, Status> {
        // Return an instance of type HelloReply
        println!("Got a request: {:?}", request);

        let reply = HelloReply {
            message: format!("Hello {}!", request.into_inner().name), // We must use .into_inner() as the fields of gRPC requests and responses are private
        };

        Ok(Response::new(reply)) // Send back our formatted greeting
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    spawn(async {
        run_server().await.unwrap();
    });
    run_client().await
}

async fn run_server() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:50051".parse()?;
    let greeter = MyGreeter::default();

    Server::builder()
        .add_service(GreeterServer::new(greeter))
        .serve(addr)
        .await?;
    Ok(())
}

async fn run_client() -> Result<(), Box<dyn std::error::Error>> {
    sleep(Duration::from_secs(1)).await;

    let mut client = GreeterClient::connect("http://[::1]:50051").await?;

    let request = tonic::Request::new(SayHelloRequest {
        name: "PowerMacros".into(),
    });

    let response = client.say_hello(request).await?;

    println!("Got a response: {:?}", response);
    Ok(())
}
