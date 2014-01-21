-module(traceTest).
-export([start/0, loop/0, pi/1]).

pi(P) ->
	try 
		{element(2,case process_info(P,registered_name) of 
			[] -> process_info(P,initial_call); 
			R -> R 
		end), 
		element(2,process_info(P,current_function))} 
	catch 
		_:_ -> dead 
	end.

start() ->
	register(trip,self()), 
	erlang:trace(all,true,[send]), 
	loop().

loop() ->
	receive
		{trace,Sender,send,true,Rec} -> io:fwrite("~p -> ~p~n", [pi(Sender), pi(Rec)]); 
		_ -> ok
	end, 
	loop().


