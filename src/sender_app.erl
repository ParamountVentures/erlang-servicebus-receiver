%%%-------------------------------------------------------------------
%% @doc sender public API
%% @end
%%%-------------------------------------------------------------------

-module(sender_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, send/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  io:format("Starting ..."),
  amqp10_client_sup:start_link(),
  sender_sup:start_link().

send() ->
  io:format("Connecting ...\n"),
	
  {ok, Address} = application:get_env(sender, "Address"),
  {ok, Hostname} = application:get_env(sender, "Hostname"),
  {ok, Port} = application:get_env(sender, "Port"),
  {ok, User} = application:get_env(sender, "User"),
  {ok, Password} = application:get_env(sender, "Password"),
  {ok, Container} = application:get_env(sender, "Queue"),
  {ok, Queue} = application:get_env(sender, "Queue"),
  OpnConf = #{
	      address => Address,
	      hostname => Hostname,
              port => Port,
	      notify => self(),
	      tls_opts => {secure_port, [{versions, ['tlsv1.1']}]},
              container_id => Container,
              sasl => {plain, User, Password}
	    },

  {ok, Connection} = amqp10_client:open_connection(OpnConf),

  receive
    {amqp10_event, {connection, Connection, opened}} ->
    io:format("Connected ...\n")
  after 2000 ->
    exit(connection_timeout)
  end,  

  {ok, Session} = amqp10_client:begin_session(Connection),
  receive
    {amqp10_event, {session, Session, begun}} ->
    	io:format("Session Id ...\n")
  after 2000 ->
    exit(session_timeout)
  end,
  {ok, Sender} = amqp10_client:attach_sender_link(Session,
                                                  <<"test-sender">>,
                                                  Queue), 
    % wait for credit to be received
    receive
      {amqp10_event, {link, Sender, credited}} -> 
        ok
    after 2000 ->
      exit(credited_timeout)
    end,
  
    % send message
    Msg0 = amqp10_msg:new(<<"test">>, <<"my-body">>, false),
    P =#{group_id => <<"test">>},
    Msg1 = amqp10_msg:set_properties(P, Msg0),
    Q =#{"x-opt-partition-key" => 12345},
    Msg = amqp10_msg:set_message_annotations(Q, Msg1),
    ok = amqp10_client:send_msg(Sender, Msg),
    io:format("Sent Message ...\n"),

    % close session link
    ok = amqp10_client:detach_link(Sender),

    % close off
    erlang:display("Closing Session ..."),
    ok = amqp10_client:end_session(Session),
    ok = amqp10_client:close_connection(Connection),
    ok.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
