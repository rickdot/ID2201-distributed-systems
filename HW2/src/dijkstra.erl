%% @author rickz
%% @doc @todo Add description to dijkstra.


-module(dijkstra).
-export([table/2, route/2]).


entry(Node, Sorted) -> 
	Find = lists:keyfind(Node, 1, Sorted),
	if Find == false ->
			Return = 0;
		true -> 
			{_, Return, _} = Find
	end,
	Return.

replace(Node, N, Gateway, Sorted) -> 
	NewList = lists:keydelete(Node, 1, Sorted) ++ [{Node, N, Gateway}],
	lists:keysort(2, NewList).

update(Node, N, Gateway, Sorted) -> 
	Entry = entry(Node, Sorted),
	if N < Entry -> 
			Return = replace(Node, N, Gateway, Sorted);
	   true -> 
			Return = Sorted
	end,
	Return.
		
	
%% dijkstra:update(london, 2, amsterdam, []).
%% dijkstra:update(london, 2, amsterdam, [{london, 2, paris}]).
%% dijkstra:update(london, 1, stockholm, [{berlin, 2, paris}, {london, 3, paris}]).




% empty sorted list
iterate([], _, Table) ->
	Table;

% infinity length
iterate([{_,inf,_}|_], _, Table) ->
	Table;

iterate([{Dst, Length, NextHop} | Tail], Map, Table) ->
	Reach = map:reachable(Dst, Map),
	NewSorted = lists:foldl(fun(Elem, Acc) ->
						update(Elem, Length+1, NextHop, Acc) end, Tail, Reach),
	iterate(NewSorted, Map, [{Dst,NextHop} | Table]).

%% dijkstra:iterate([{paris, 0, paris}, {berlin, inf, unknown}], [{paris, [berlin]}], []).

%% construct a routing table
table(Gateways, Map) ->
	AllNodes = map:all_nodes(Map),
    Rest = lists:filter(fun (X) -> not lists:member(X, Gateways) end, AllNodes),
    Direct = lists:map(fun (Node) -> {Node, 0, Node} end, Gateways),
    Indirect = lists:map(fun (Node) -> {Node, inf, unknown} end, Rest),
    Sorted = lists:append(Direct, Indirect),
    iterate(Sorted, Map, []).


route(Node, Table) ->
	case lists:keysearch(Node, 1, Table) of
		{value, {_, unknown}} ->
		    notfound;
		{value, {_, Gateway}} ->
		    {ok, Gateway};
		false ->
		    notfound
    end.

	



	