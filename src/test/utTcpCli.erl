-module(utTcpCli).

-export([start/4, send/2, connect/4, loop/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}]).

start(0, _Num, _Host, _Port) ->
   ok;
start(Cnt, Num, Host, Port) ->
   spawn(?MODULE, connect, [Num, self(), Host, Port]),
   start(Cnt - 1, Num, Host, Num).

connect(Num, Parent, Host, Port) ->
   case gen_tcp:connect(Host, Port, ?TCP_OPTIONS, 6000) of
      {ok, Sock} ->
         Parent ! {connected, Sock},
         loop(Num, Sock);
      {error, Reason} ->
         io:format("Client ~p connect error: ~p~n", [Num, Reason])
   end.

loop(Num, Sock) ->
   % Timeout = 5000 + rand:uniform(5000),
   receive
      {tcp, Sock, Data} ->
         io:format("Client ~w received: ~s~n", [Num, Data]),
         loop(Num, Sock);
      {tcp_closed, Sock} ->
         io:format("Client ~w socket closed~n", [Num]);
      {tcp_error, Sock, Reason} ->
         io:format("Client ~w socket error: ~p~n", [Num, Reason]);
      Other ->
         io:format("Client ~w unexpected: ~p", [Num, Other])
   after 0 ->
      send(Num, Sock), loop(Num, Sock)
   end.

send(N, Sock) ->
   gen_tcp:send(Sock, [integer_to_list(N), ":", <<"Hello, eSockd!">>]).

