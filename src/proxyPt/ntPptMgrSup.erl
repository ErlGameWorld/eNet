-module(ntPptMgrSup).

-behaviour(supervisor).

-include("eNet.hrl").
-include("ntCom.hrl").

-export([
   start_link/3
]).

-export([
   init/1
]).

-spec(start_link(SupName :: atom(), Port :: inet:port_number(), ListenOpts :: [listenOpt()]) -> {ok, pid()} | {error, term()}).
start_link(SupName, Port, ListenOpts) ->
   supervisor:start_link({local, ntCom:supName(ssl, SupName)}, ?MODULE, {SupName, Port, ListenOpts}).

init({SupName, Port, ListenOpts}) ->
   SupFlag = #{strategy => one_for_one, intensity => 100, period => 3600},

   AptSupName = ntCom:asName(ssl, SupName),
   ListenName = ntCom:lsName(ssl, SupName),

   SslOpts = ?ntGLV(sslOpts, ListenOpts, undefined),
   SslHSTet = ?ntGLV(sslHSTet, ListenOpts, ?DefSslHSTet),
   ProxyPt = ?ntGLV(proxyPt, ListenOpts, false),
   ProxyPtTet = ?ntGLV(proxyPtTet, ListenOpts, ?DefProxyPtTet),

   ChildSpecs = [
      #{
         id => AptSupName,
         start => {ntPptAcceptorSup, start_link, [AptSupName, SslOpts, SslHSTet, ProxyPt, ProxyPtTet]},
         restart => permanent,
         shutdown => infinity,
         type => supervisor,
         modules => [ntPptAcceptorSup]
      },
      #{
         id => ListenName,
         start => {ntPptListener, start_link, [ListenName, AptSupName, Port, ListenOpts]},
         restart => permanent,
         shutdown => 3000,
         type => worker,
         modules => [ntPptListener]
      }],
   {ok, {SupFlag, ChildSpecs}}.





