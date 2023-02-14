%% @author dell
%% @doc @todo Add description to router.


-module(intf).

-export([add/4, remove/2, lookup/2, ref/2, name/2, list/1, new/0, broadcast/2]).



new() -> 
	[].

add(Name, Ref, Pid, Intf) -> 
%% 	io:fwrite("add, Intf: ~w~n", [Intf]),
	Intf ++ [{Name, Ref, Pid}].

remove(Name, Intf) ->
  	lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
	Find = lists:keyfind(Name, 1, Intf),
	case Find of
		false -> 
			notfound;
		{_, _, Pid} ->
			{ok, Pid}
	end.
	

ref(Name, Intf) -> 
	Find = lists:keyfind(Name, 1, Intf),
	case Find of
		false -> 
			notfound;
		{_, Ref, _} ->
			{ok, Ref}
	end.
	

name(Ref, Intf) ->
	Find = lists:keyfind(Ref, 2, Intf),
	case Find of
		false -> 
			notfound;
		{Name, _, _} ->
			{ok, Name}
	end.

%% list(Intf) -> 
%% 	Fun = fun(X, AccIn) -> 
%% 		{Node, _, _} = X,
%% 		AccIn ++ [Node]
%% 	end,
%% 
%% 	lists:foldl(Fun, [], Intf).




list(Intf) ->
	lists:map(fun({Name,_,_}) -> Name end, Intf).

broadcast(Message, Intf) ->
	io:format("~w~n", [Intf]),
	lists:map(fun({_,_,Pid}) -> Pid ! Message end, Intf).



%% router:add(name1, ref1, pid1, []).
%% router:remove(name1, [{name1,ref1,pid1}]).
%% router:lookup(name1, [{name1,ref1,pid1}]).
%% router:ref(name1, [{name1,ref1,pid1}]).
%% router:name(ref1, [{name1,ref1,pid1}]).
%% router:list([{name1,ref1,pid1}, {name2,ref2,pid2}]).
