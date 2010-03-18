-module(comet_tail).
-export([start/1, loop/1, handle_request/1]).


start(Port)->
	{ok, ListenSock}=gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
	?MODULE:loop(ListenSock).
	
loop(ListenSock) ->	
	{ok, Sock}=gen_tcp:accept(ListenSock),
	spawn(?MODULE, handle_request, [Sock]),
	?MODULE:loop(ListenSock).
	
handle_request(Sock) ->
	{ok, Device} = file:open("/tmp/x.txt", [read]),
	tail_to_socket(Device, Sock).

tail_to_socket(Device, Sock) ->
	case io:get_line(Device, "") of
        eof  -> tail_to_socket(Device, Sock);
        Line ->	gen_tcp:send(Sock, Line), tail_to_socket(Device, Sock)
	end.