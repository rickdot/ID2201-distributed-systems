%% @author dell
%% @doc @todo Add description to node1.


-module(node2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% start a first node
start(Id) ->
	start(Id, nil).

%% connect to existing ring
start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	Store = storage:create(), %% emtyp store
	node(Id, Predecessor, Successor, Store).

connect(Id, nil) -> %% we re first node -> we re own successor
	{ok, {Id, self()}};
connect(Id, Peer) -> %% connect to a ring -> 
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey, Peer}}
		after ?Timeout ->
			io:format("Time out: no response~n",[])
	end.

node(Id, Predecessor, Successor, Store) ->
%% 	io:format("current Id: ~w predecessor: ~w successor: ~w~n", [Id, Predecessor, Successor]),
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store);
		{notify, New} ->  %%check if need to update the Predecessor
			Pred = notify(New, Id, Predecessor),  %% determine the correct predecessor
			node(Id, Pred, Successor, Store);
		{request, Peer} -> %% asked for info about pred
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor, Store);
		{status, Pred} ->  %% receive status of  the pred of its succ
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ, Store);
		stabilize ->  %% stabilize the ring
			stabilize(Successor),
			node(Id, Predecessor, Successor, Store);
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor, Store);
		{probe, Id, Nodes, T} ->
			io:format("Id ~w: ", [Id] ),
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor, Store);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor, Store);
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client,
			Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store);

		debug ->
			io:format("Id: ~w predecessor: ~w successor: ~w store: ~w~n", [Id, Predecessor, Successor, Store]),
			node(Id, Predecessor, Successor, Store)
	end.

create_probe(Id, {Skey, Spid})->
	io:format("created probe for node ~w~n",[Id]),
	Spid ! {probe, Id, [], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
	Time = (erlang:system_time(micro_seconds) - T),
	io:format("time: ~w microsec, Nodes: ~w~n", [Time, Nodes]).

forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
	New_Nodes = Nodes ++ [Id],
	io:format("Node ~w forward the prob to ~w~n",[Id, Skey]),
	Spid ! {probe, Ref, New_Nodes, T}.

%% being notified -> update its predecessor? return the correct Pred
notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, Npid};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true -> %% Id - Nkey - Pkey
					{Nkey, Npid};
				false ->
					Predecessor
			end
	end.


%% 
request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.

%% verify if the pred of node's succ is a node itself
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} ->
			Successor;
		{Skey, _} ->
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} ->			
			case key:between(Xkey, Id, Skey) of
				true ->
					Xpid ! {notify, {Id, self()}},
					{Xkey, Xpid};
				false ->
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.

%% regularly stabilize to ensure the ring is stable 
schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).    %% self ! stabilize

%% send a {request} msg to its successor
stabilize({_, Spid}) ->
	Spid ! {request, self()}.




%% new for node2
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of  %% verify
		true ->  
			Client ! {Qref, ok},
			storage:add(Key, Value, Store);
		false ->  %% not responsible, forward to succ
			Spid !  {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->  %% send the reply to requester
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {lookup, Key, Qref, Client}
	end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
	nil ->
		Keep = handover(Id, Store, Nkey, Npid),
		{{Nkey, Npid}, Keep};
	{Pkey, _} ->
		case key:between(Nkey, Pkey, Id) of
			true ->
				Keep = handover(Id, Store, Nkey, Npid),
				{{Nkey,Npid}, Keep};
			false ->
				{Predecessor, Store}
		end
	end.

handover(Id, Store, Nkey, Npid) ->
	{Keep, Rest} = storage:split(Id, Nkey, Store),
	Npid ! {handover, Rest},
	Keep.



%  Q  = test:start(node2).
%  test:start(node2, 5, Q).
%  Q ! stabilize.
%  K = test:keys(4000).
%  test:add(K, Q).
%  test:check(K, Q).


