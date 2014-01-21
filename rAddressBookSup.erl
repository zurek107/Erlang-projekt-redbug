-module(rAddressBookSup).
-export([start/0, init/0]).

start() ->
	spawn_link(rAddressBookSup, init, []).
	
init() ->
	process_flag(trap_exit, true),
	loop(startServer()).
	
loop(Pid) ->
	receive
		stop -> Pid ! stop, ok;
		{'EXIT', Pid, _} -> loop(startServer())
	end.
	
startServer() ->
	spawn_link(rAddressBook, start, []).
