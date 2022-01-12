-module(utUdpCMod).             %% tcp active false server

-export([datagram/5]).

datagram(Sock, IP, Port, AncData, Data) ->
   io:format("udp receive the data ~p ~p ~p ~p ~p ~n ", [Sock, IP, Port, AncData, Data]),
   ok = gen_udp:send(Sock, IP, Port, Data).