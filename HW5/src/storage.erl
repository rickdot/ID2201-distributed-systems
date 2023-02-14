%% @author dell
%% @doc @todo Add description to storage.


-module(storage).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add/3, lookup/2, split/3, merge/2, create/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================


create() ->
	[].

add(Key, Value, Store) ->
	Store ++ [{Key, Value}].

lookup(Key, Store) ->
	case lists:keyfind(Key, 1, Store) of
		{Key, Value} -> {Key, Value};
		false -> false
	end.


split(From, To, Store) ->
	lists:partition(fun({Key,_})->key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
	lists:keymerge(1, Entries, Store).