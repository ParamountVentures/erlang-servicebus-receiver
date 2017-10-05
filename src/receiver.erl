-module(receiver).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

% Starts this module and will create an instance based on a spec if this is passed in
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(I) ->
    ServerName = lists:flatten(io_lib:format("~p~p", [?SERVER, I])),
    io:format("I am ~p~n", [list_to_atom(ServerName)]),
    gen_server:start_link({local, list_to_atom(ServerName)}, ?MODULE, [], []).

init([]) ->
     {registered_name, Worker}= erlang:process_info(self(), registered_name),
     io:format("M=~p~n", [Worker]),

     gen_server:cast(Worker, listen),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(listen, State) ->
     {registered_name, Worker}= erlang:process_info(self(), registered_name),
     retrieve(Worker),
     {noreply, State};
handle_cast(_Msg, State) ->
    io:format("Nada~n"),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% INTERNAL %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retrieve(Worker) ->
  io:format("Connecting as ~p ...\n", [Worker]),

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
   
  erlang:display("Creating receiver ..."),
    % create a receiver link
    {ok, Receiver} = amqp10_client:attach_receiver_link(Session,
                                                    <<"test-receiver">>,
                                                    Queue),

    erlang:display("Getting credit ..."),

    % grant some credit to the remote sender but don't auto-renew it
    ok = amqp10_client:flow_link_credit(Receiver, 5, 3),

    io:format("Waiting Delivery as ~p ...\n", [Worker]),

    % wait for a delivery
    listener(Worker),

    % close off
    erlang:display("Closing Session ..."),
    ok = amqp10_client:detach_link(Receiver),
    ok = amqp10_client:end_session(Session),
    ok = amqp10_client:close_connection(Connection),
ok.

listener(Worker) ->
  % wait for a delivery
  receive
    {amqp10_msg, Receiver, InMsg} -> 
      io:format("Got Message in ~p ...", [Worker]),
      erlang:display(InMsg),
      ok
    after infinity ->
      exit(delivery_timeout)
    end,
  io:format("Listening as ~p ...", [Worker]),
  listener(Worker).

