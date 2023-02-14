%% @author rickz
%% @doc @todo Add description to map.

-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() -> 
	[].

update(Node, Links, Map) -> 
	Find = lists:keyfind(Node, 1, Map),
	if Find == false ->
		   NewMap = Map ++ [{Node, Links}];
	   true ->
		   NewMap = lists:keydelete(Node, 1, Map) ++ [{Node, Links}]
	end,
	NewMap.

reachable(Node, Map) -> 
	Find = lists:keyfind(Node, 1, Map),
	if Find == false ->
		   Return = [];
	   true ->
		   {_, Links} = Find,
		   Return = Links
	end,
	Return.

all_nodes(Map) -> 
	Fun = fun(X, AccIn) -> 
		{Node, Links} = X,
		TmpLst = [Node] ++ Links,
		AccIn ++ TmpLst
	end,

	Lst = lists:foldl(Fun, [], Map),
	Set = sets:from_list(Lst),
	sets:to_list(Set).


