%% @author dell
%% @doc @todo Add description to worker.


-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
	random:seed(Seed, Seed, Seed),
	InitialClock = time:zero(),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, InitialClock);
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Clock)->
	Wait = random:uniform(Sleep),
	receive
		{msg, Time, Msg} ->
			Greater = time:merge(Clock, Time),  % merge
			NewClock = time:inc(Name, Greater),
			Log ! {log, Name, NewClock, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, NewClock);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
	after Wait ->
		Selected = select(Peers),
%% 		io:fwrite("clock of ~w: ~w ~n", [Name, Clock]),
		NewClock = time:inc(Name, Clock),  % internal clock increase by 1

		Message = {hello, random:uniform(100)},
		Selected ! {msg, NewClock, Message},  % send timestamp with message
		jitter(Jitter),
		Log ! {log, Name, NewClock, {sending, Message}},
		loop(Name, Log, Peers, Sleep, Jitter, NewClock)
	end.

select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> 
	ok;

jitter(Jitter) -> 
	timer:sleep(random:uniform(Jitter)).