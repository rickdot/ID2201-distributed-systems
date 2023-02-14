%% @author dell
%% @doc @todo Add description to hist.


-module(hist).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, update/3]).


new(Name) ->
	[{Name, inf}].

update(Node, N, History) ->
	case lists:keyfind(Node, 1, History) of
		{_, Number} -> 	
			if
			Number > N -> Return = old;
			true ->
				Updated = lists:keydelete(Node, 1, History) + [{Node, N}],
				Return = {new, Updated}
			end;
		false ->
			Return = {new, History ++ [{Node, N}]}
	end,
	Return.
	
		
	
	


