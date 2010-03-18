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
	{ok, {http_request, Method, Path, Version}}=gen_tcp:recv(Sock, 0),
	{abs_path,"/?"++FilePath}=Path,
	{ok, Device} = file:open(FilePath, [read]),
	gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\nTransfer-Encoding: chunked\r\n\r\n"),
	tail_to_socket(Device, Sock).

tail_to_socket(Device, Sock) ->
	case io:get_line(Device, "") of
        eof  -> tail_to_socket(Device, Sock);
        Line ->	Length=io_lib:format("~.16B", [length(Line)]),gen_tcp:send(Sock, Length),gen_tcp:send(Sock, "\r\n"),gen_tcp:send(Sock, Line),gen_tcp:send(Sock, "\r\n"), tail_to_socket(Device, Sock)
	end.
	
	
%%generate_a_1K_comment_for_safari()	