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

   , openTcp/4
   , openSsl/4
   , openUdp/4
   , openDtls/4
   , close/1
   , close/2
   , reopen/1
   , reopen/2

   , tcpChildSpec/4
   , udpChildSpec/4
   , dtlsChildSpec/4

   , listener/1
   , listeners/0

   , getStats/1
   , getOptions/1
   , getAcceptors/1

   , setMaxConnections/2
   , getMaxConnections/1
   , getCurConnections/1

   , getShutdownCount/1

   , getAccessRules/1
   , allow/2
   , deny/2

   , mergeOpts/2
   , parseOpt/1
   , getUlimit/0
   , fixAddr/1
   , addrToString/1
   , format/1
]).

-type(nameId() :: atom()).
-type(transport() :: module()).
-type(udp_transport() :: {udp | dtls, pid(), inet:socket()}).
-type(socket() :: esockd_transport:socket()).
-type(mfar() :: atom() | {atom(), atom()} | {module(), atom(), [term()]}).
-type(sock_fun() :: fun((esockd_transport:socket()) -> {ok, esockd_transport:socket()} | {error, term()})).
-type(host() :: inet:ip_address() | string()).
-type(addrPort() :: inet:port_number() | {host(), inet:port_number()}).
-type(protoOption() :: {acceptors, pos_integer()}
   | {max_connections, pos_integer()}
   | {max_conn_rate, pos_integer() | {pos_integer(), pos_integer()}}
   | {access_rules, [esockd_access:rule()]}
   | {shutdown, brutal_kill | infinity | pos_integer()}
   | tune_buffer | {tune_buffer, boolean()}
   | proxy_protocol | {proxy_protocol, boolean()}
   | {proxy_protocol_timeout, timeout()}
   | {ssl_options, [ssl:ssl_option()]}
   | {tcp_options, [gen_tcp:listen_option()]}
   | {udp_options, [gen_udp:option()]}
   | {dtls_options, [gen_udp:option() | ssl:ssl_option()]}).


-spec start() -> ok.
start() ->
   {ok, _} = application:ensure_all_started(netPools),
   ok.

%% @doc Open a TCP listener
-spec(openTcp(nameId(), addrPort(), mfa(), [protoOption()]) -> {ok, pid()} | {error, term()}).
openTcp(NameId, Port, MFA, Opts) when is_atom(NameId), is_integer(Port) ->
   esockd_sup:start_listener(NameId, Port, Opts, MFA);
openTcp(NameId, {Host, Port}, MFA, Opts) when is_atom(NameId), is_integer(Port) ->
   {IPAddr, _Port} = fixAddr({Host, Port}),
   case proplists:get_value(ip, tcp_options(Opts)) of
      undefined -> ok;
      IPAddr    -> ok;
      Other     -> error({badmatch, Other})
   end,
   esockd_sup:start_listener(Proto, {IPAddr, Port}, Opts, MFA).

tcp_options(Opts) ->
   proplists:get_value(tcp_options, Opts, []).

open_udp(Proto, Port, Opts, MFA) ->
   esockd_sup:start_child(udp_child_spec(Proto, Port, Opts, MFA)).

udp_child_spec(Proto, Port, Opts, MFA) ->
   esockd_sup:udp_child_spec(Proto, fixaddr(Port), udp_options(Opts), MFA).

udp_options(Opts) ->
   proplists:get_value(udp_options, Opts, []).

open_dtls(Proto, ListenOn, Opts, MFA) ->
   esockd_sup:start_child(dtls_child_spec(Proto, ListenOn, Opts, MFA)).

dtls_child_spec(Proto, ListenOn, Opts, MFA) ->
   esockd_sup:dtls_child_spec(Proto, fixaddr(ListenOn), Opts, MFA).


addListener(Proto, ok) ->
   ok.


-spec tcpChildSpec(atom(), listenOn(), [option()], mfargs()) -> supervisor:child_spec().
tcpChildSpec(Proto, IpPort, Opts, MFA) when is_atom(Proto) ->
   #{
      id => child_id(Proto, IpPort),
      start => {tcp_listener_sup, start_link, [Proto, IpPort, Opts, MFA]},
      restart => transient,
      shutdown => infinity,
      type => supervisor,
      modules => [esockd_listener_sup]
   }.

-spec(udpChildSpec(atom(), esockd:listenOn(), [esockd:option()], esockd:mfargs()) -> supervisor:child_spec()).
udpChildSpec(Proto, Port, Opts, MFA) when is_atom(Proto) ->
   #{id => child_id(Proto, Port),
      start => {esockd_udp, server, [Proto, Port, Opts, MFA]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [esockd_udp]}.

-spec(dtlsChildSpec(atom(), esockd:listenOn(), [esockd:option()], esockd:mfargs()) -> supervisor:child_spec()).
dtlsChildSpec(Proto, Port, Opts, MFA) when is_atom(Proto) ->
   #{id => child_id(Proto, Port),
      start => {dtls_listener_sup, start_link, [Proto, Port, Opts, MFA]},
      restart => transient,
      shutdown => infinity,
      type => supervisor,
      modules => [dtls_listener_sup]}.