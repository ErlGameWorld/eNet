-module(utUdpCMod).             %% upd active false server

-export([start/2, datagram/5]).

start(Name, Port) ->
   Opts = [{conMod, ?MODULE}],
   eNet:openUdp(Name, Port, Opts).


datagram(Sock, IP, Port, AncData, Data) ->
   io:format("udp receive the data ~p ~p ~p ~p ~p ~n ", [Sock, IP, Port, AncData, Data]),
   ok = gen_udp:send(Sock, IP, Port, Data),
   {ok, self()}.