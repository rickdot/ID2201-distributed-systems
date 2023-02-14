%% @author dell
%% @doc @todo Add description to time.


-module(time).

%% ====================================================================
%% API functions
%% ====================================================================
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).



zero() ->
	0.

inc(Name, T) ->
	T+1.

merge(Ti, Tj) ->
	max(Ti, Tj).

leq(Ti, Tj) ->
	if
		Ti =< Tj ->
			true;
		true ->
			false
	end.


clock(Nodes) ->
	lists:map(fun(Node) -> {Node, zero()} end, Nodes).


update(Node, Time, Clock) ->
%% 	{_, Old} = lists:keyfind(Node, 1, Clock),
%% 	if
%% 		Time > Old ->
%% 			Return = [{Node, Time}] ++ lists:keydelete(Node, 1, Clock);
%% 		true ->
%% 			Return = Clock
%% 	end.
	[{Node, Time}] ++ lists:keydelete(Node, 1, Clock).
	


safe(Time, Clock) ->
	Min = lists:min(lists:map(fun({_,T}) -> T end, Clock)),
	if
		Time =< Min -> true;
		true -> false
	end.

