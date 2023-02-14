%% @author dell
%% @doc @todo Add description to logger.


-module(logger1).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	InitialClock = time:clock(Nodes),
	loop(InitialClock, []).

loop(Clock, Queue) ->
%% 	io:fwrite("clock is ~w~n", [Clock]),
%% 	io:fwrite("queue is ~w~n", [Queue]),
	receive
		{log, From, Time, Msg} ->
			NewClock = time:update(From, Time, Clock),
			NewQueue = lists:keysort(2, Queue ++ [{From, Time, Msg}]),
			ReturnQ = lists:dropwhile(fun({F,T,Msg}) ->
										case time:safe(T, NewClock) of
											true ->
												log(F, T, Msg),
												true;
											false ->
												false
										end
									end, NewQueue),
%% 			lists:foreach(fun({F,T,Msg}) -> 
%% 								  case time:safe(T, NewClock) of
%% 									  true -> 
%% 										  log(F, T, Msg);
%% 									  false ->
%% 										  false
%% 								  end	  
%% 						  			end
%% 								  , SortedQueue),
%% 			NewQueue2 = lists:filter(fun({F,T,Msg}) -> T<time:minTimestamp(NewClock) end, SortedQueue),
			loop(NewClock, ReturnQ);
		stop ->
			lists:foreach(fun({F,T,Msg}) -> log(F,T,Msg) end, Queue),
			ok
	end.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
