%% @author dell
%% @doc @todo Add description to gms1.

-module(gms2).
%% risk of crashing
-define(arghh, 100).

-export([leader/4, slave/5, start/1, start/2]).

%% bcast(_, Msg, Slaves) ->
%% 	lists:foreach(fun(Slave) -> Slave ! Msg end, Slaves).
	
bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
	case random:uniform(?arghh) of
		100 ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.


leader(Id, Master, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, Msg}, Slaves),		% send to all slaves
			Master ! Msg,		% send to its master
			leader(Id, Master, Slaves, Group);
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]), % add new slave to the slave list
			Group2 = lists:append(Group, [Wrk]), % add new slave to group member list
			bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2), % send the new group view to all slaves
			Master ! {view, Group2}, % send the new view to its master
			leader(Id, Master, Slaves2, Group2);
		stop ->
			ok
	end.

slave(Id, Master, Leader, Slaves, Group) ->
	receive
		{mcast, Msg} ->		% receive a multicast request, then forward to leader
		Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Slaves, Group);
		{join, Wrk, Peer} ->		% receive a request from a new worker Wrk, then forawrd to leader
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, Slaves, Group);
		{msg, Msg} ->		% receive a message, send to its master
			Master ! Msg,
			slave(Id, Master, Leader, Slaves, Group);
		{view, [Leader|Slaves2], Group2} ->		% receive a new view, send to its master
			Master ! {view, Group2},
			slave(Id, Master, Leader, Slaves2, Group2);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, Slaves, Group);
		stop ->
			ok
	end.




election(Id, Master, Slaves, [_|Group]) ->
	Self = self(),
	case Slaves of
		[Self|Rest] ->
			bcast(Id, {view, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master, Rest, Group);
		[Leader|Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Rest, Group)
	end.




%% initialization

start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, [], [Master]).


%% starting a new node to join existing group
start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
	Rnd = random:uniform(1000),
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, [Leader|Slaves], Group} ->
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Slaves, Group)
	after 5000 ->
		Master ! {error, "no reply from leader"}
	end.

