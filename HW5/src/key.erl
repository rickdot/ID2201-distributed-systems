%% @author dell
%% @doc @todo Add description to key.


-module(key).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate/0, between/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================


generate() ->
	random:uniform(1000000000).


between(Key, From, To) when From < To ->
    From < Key andalso Key =< To;
between(Key, From, To) when From > To ->
	Key=<To orelse From<Key;
between(_, From, To) when From == To ->
	true.

