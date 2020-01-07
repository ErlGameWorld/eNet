-module(erlNetLib).

-include("erlNetLib.hrl").

-define(TCP_DEFAULT_OPTIONS, [
   binary
   , {packet, 4}
   , {active, false}
   , {reuseaddr, true}
   , {nodelay, false}
   , {delay_send, true}
   , {send_timeout, 15000}
   , {keepalive, true}
   , {exit_on_close, true}]).

-export([
   start/0
   , addTcpLr/4
   , stopTcpLr/1
]).

-spec start() -> ok.
start() ->
   {ok, _} = application:ensure_all_started(erlNetLib),
   ok.

%% add a TCP listener
-spec addTcpLr(listenName(), listenOn(), conMod(), [listenOpt()]) -> {ok, pid()} | {error, term()}.
addTcpLr(ListenName, AddrPort, ConMod, ListenOpt) ->
   case erlang:whereis(?nlTcpMgrSup) of
      undefined ->
         TcpMgrSupSpec = #{
            id => ?nlTcpMgrSup,
            start => {?nlTcpMgrSup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [?nlTcpMgrSup]
         },
         {ok, _Pid} = erlNetLib_sup:startChild(TcpMgrSupSpec);
      _ ->
         ignore
   end,
   FixAddrPort = nlNetCom:fixAddr(AddrPort),
   TcpListenSpec = #{
      id => ListenName,
      start => {nlTcpListener, start_link, [ListenName, FixAddrPort, ConMod, ListenOpt]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [nlTcpListener]
   },
   nlTcpMgrSup:startChild(TcpListenSpec).

%% stop a TCP listener
-spec stopTcpLr(listenName()) -> ignore | {ok, pid()} | {error, term()}.
stopTcpLr(ListenName) ->
   case erlang:whereis(?nlTcpMgrSup) of
      undefined ->
         ignore;
      _ ->
         nlTcpMgrSup:terminateChild(ListenName),
         nlTcpMgrSup:deleteChild(ListenName)
   end.
