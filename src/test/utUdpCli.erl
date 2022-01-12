-module(utUdpCli).

-export([start/4, send/4, connect/4, loop/4]).

-define(UDP_OPTIONS, [binary, {active, true}]).

start(0, _Num, _Host, _Port) ->
   ok;
start(Cnt, Num, Host, Port) ->
   spawn(?MODULE, connect, [Num, self(), Host, Port]),
   start(Cnt - 1, Num, Host, Num).

connect(Num, Parent, Host, Port) ->
   case gen_udp:open(0, ?UDP_OPTIONS) of
      {ok, Sock} ->
         Parent ! {connected, Sock},
         loop(Num, Sock, Host, Port);
      {error, Reason} ->
         io:format("Client ~p connect error: ~p~n", [Num, Reason])
   end.

loop(Num, Sock, Host, Port) ->
   % Timeout = 5000 + rand:uniform(5000),
   receive
      {udp, Sock, IP, InPortNo, Data} ->
         io:format("Client ~w received: ~p ~p ~s~n", [Num, IP, InPortNo, Data]),
         loop(Num - 1, Sock, Host, Port);
      {udp, Sock, IP, InPortNo, AncData, Data} ->
         io:format("Client ~w received: ~p ~p ~p ~s~n", [Num, IP, InPortNo, AncData, Data]),
         loop(Num - 1, Sock, Host, Port);
      Other ->
         io:format("Client ~w unexpected: ~p", [Num, Other])
   after 5000 ->
      send(Num, Sock, Host, Port), loop(Num - 1, Sock, Host, Port)
   end.

send(N, Sock, Host, Port) ->
   io:format("fdsfsfs ~n"),
   gen_udp:send(Sock, Host, Port, [integer_to_list(N), ":", <<"Hello, eSockd!">>]).

