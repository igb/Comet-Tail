-module(comet_tail).
-export([start/2, loop/2, handle_request/2, is_this_a_safety_path/1, generate_comment_for_length/2, generate_iframe_wrapper/1]).


start(Port, LogDir)->
	{ok, ListenSock}=gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
	?MODULE:loop(ListenSock, LogDir).
	
loop(ListenSock, LogDir) ->	
	{ok, Sock}=gen_tcp:accept(ListenSock),
	spawn(?MODULE, handle_request, [Sock, LogDir]),
	?MODULE:loop(ListenSock, LogDir).
	
handle_request(Sock, LogDir) ->
	{ok, {http_request, Method, Path, Version}}=gen_tcp:recv(Sock, 0),
	case (Path) of 
		{abs_path,"/?"++FilePath}->
			case (is_this_a_safety_path(FilePath)) of
				err -> gen_tcp:send(Sock, "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\nInvalid File Path\r\n\r\n"),gen_tcp:close(Sock);
				ok -> {ok, Device} = file:open(lists:append(LogDir,FilePath), [read]), 
				gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\nTransfer-Encoding: chunked\r\n\r\n"),
				tail_to_socket(Device, Sock, 1)
			end;
		{abs_path,"/comet-tail?"++FilePath} ->
				HtmlFile=generate_iframe_wrapper(FilePath),
				HtmlLength=lists:flatten(io_lib:format("~p", [length(HtmlFile)])),
				gen_tcp:send(Sock, lists:append(["HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\nContent-Length: ", HtmlLength, "\r\n\r\n", HtmlFile])),
				gen_tcp:close(Sock)
	end.

%% Backoff set so as to not throttle CPU when nothing left to read...1, 2, 4, 8, 16, 2, 4, 8, 16 second cycle...
tail_to_socket(Device, Sock, Backoff) ->
	case io:get_line(Device, "") of
        eof  -> timer:sleep(Backoff * 1000),NewBackoff=(Backoff * 2 rem 30),tail_to_socket(Device, Sock, NewBackoff);
        Line ->	HtmlLine=lists:append([generate_comment_for_length([], 1000), Line, "<br/>"]), 
			Length=io_lib:format("~.16B", [length(HtmlLine)]),
			gen_tcp:send(Sock, Length),gen_tcp:send(Sock, "\r\n"),
			gen_tcp:send(Sock, HtmlLine),gen_tcp:send(Sock, "\r\n"), 
			tail_to_socket(Device, Sock, 1)
	end.
	
%% simple saftey check for relative directory parent references within path... 	
is_this_a_safety_path([Head|Tail]) ->
	case (Head) of
		46 ->[NextHead|NextTail]=Tail, case (NextHead) of
			46 -> err;
			_ ->is_this_a_safety_path(NextTail)
		end;
		_->is_this_a_safety_path(Tail)
	end;
is_this_a_safety_path([])->
	ok.		
	
%% generate a ~n kb comment for Safari and other lame-o browsers in order to force flush...
generate_comment_for_length([], Length) ->
	generate_comment_for_length("<!--", Length - 4);
generate_comment_for_length(Comment, 0) ->
		lists:append(Comment, "-->");	
generate_comment_for_length(Comment, Length) ->
	generate_comment_for_length(lists:append(Comment, " "), Length -1).
		
%% generate a iframe wrapper
generate_iframe_wrapper(FilePath)->
	lists:append(["<html><body><iframe id='logframe' frameborder='0' src='/?", FilePath, "'    width='100%' height='95%' scrolling='no'> </iframe><script>function loop() { if(navigator.userAgent.indexOf('Safari') > 0) { 	window.frames['logframe'].scrollBy(0 , 10000000); } else  { document.getElementById('logframe').contentWindow.scrollBy(0, 100000); } setTimeout('loop()', 1000); } loop(); </script> </body></html>"]).	

			