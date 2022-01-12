-module(eNet).

-include("eNet.hrl").

-export([
   start/0
   , stop/0
   , openTcp/3
   , openSsl/3
   , openUdp/3
   , openPpt/3
   , close/1
]).

-spec start() -> ok.
start() ->
   {ok, _} = application:ensure_all_started(eNet).

stop() ->
   application:stop(eNet).

%% add a TCP listener
-spec openTcp(ListenName :: atom(), Port :: inet:port_number(), ListenOpts :: [listenOpt()]) -> {ok, pid()} | {error, term()}.
openTcp(ListenName, Port, ListenOpts) ->
   TcpMgrSupSpec = #{
      id => ListenName,
      start => {ntTcpMgrSup, start_link, [ListenName, Port, ListenOpts]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [ntTcpMgrSup]
   },
   supervisor:start_child(eNet_sup, TcpMgrSupSpec).

%% add a Ssl listener
-spec openSsl(ListenName :: atom(), Port :: inet:port_number(), ListenOpts :: [listenOpt()]) -> {ok, pid()} | {error, term()}.
openSsl(ListenName, Port, ListenOpts) ->
   SslMgrSupSpec = #{
      id => ListenName,
      start => {ntSslMgrSup, start_link, [Port, ListenOpts]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [ntSslMgrSup]
   },
   supervisor:start_child(eNet_sup, SslMgrSupSpec).

%% add a Udp listener
-spec openUdp(UdpName :: atom(), Port :: inet:port_number(), ListenOpts :: [listenOpt()]) -> {ok, pid()} | {error, term()}.
openUdp(UdpName, Port, ListenOpts) ->
   UdpSrvSpec = #{
      id => UdpName,
      start => {ntUdpSrv, start_link, [UdpName, Port, ListenOpts]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [ntUdpSrv]
   },
   supervisor:start_child(eNet_sup, UdpSrvSpec).

%% add a Proxy protocol listener
-spec openPpt(ListenName :: atom(), Port :: inet:port_number(), ListenOpts :: [listenOpt()]) -> {ok, pid()} | {error, term()}.
openPpt(ListenName, Port, ListenOpts) ->
   SslMgrSupSpec = #{
      id => ListenName,
      start => {ntPptMgrSup, start_link, [Port, ListenOpts]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [ntPptMgrSup]
   },
   supervisor:start_child(eNet_sup, SslMgrSupSpec).

%% stop a listener
-spec close(atom()) -> ignore | {ok, pid()} | {error, term()}.
close(ListenName) ->
   supervisor:terminate_child(eNet_sup, ListenName),
   supervisor:delete_child(eNet_sup, ListenName).
