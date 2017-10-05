myapp
=====

A demo of using Erlang to send and recieve messages to/from Azure Service Bus via AMQP 1.0 using the RabbitMQ client.

As this uses a beta version of the MQ library it includes the binaries due to some complexities in building it.

Build
=====
Create a Service Bus instance on Azure and queue with Session and Partitioning enabled.

$ git clone git@github.com:ParamountVentures/erlang-servicebus-receiver.git

$ cd erlang-servicebus-receiver

Add the config details for Azure to src/sender.app.src (remove the .template extension) - placeholders are highlighted.

# Send Messages

$ ./rebar3 shell

$ application:start(sender).

Send a message to Service Bus:

$ sender_app:send().

# Receive Messages

$ ./rebar3 shell

$ application:start(sender).

$ receiver_sup:start_link().

* Start 20 workers to receive messages *

$ receiver_sup:start_receivers(20).


