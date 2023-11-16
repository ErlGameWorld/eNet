-module(ntSslMgrSup).

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

   SslOpts = ?ntGLV(sslOpts, ListenOpts, []),
   SslHSTet = ?ntGLV(sslHSTet, ListenOpts, ?DefSslHSTet),

   ChildSpecs = [
      #{
         id => AptSupName,
         start => {ntSslAcceptorSup, start_link, [AptSupName, SslOpts, SslHSTet]},
         restart => permanent,
         shutdown => infinity,
         type => supervisor,
         modules => [ntSslAcceptorSup]
      },
      #{
         id => ListenName,
         start => {ntSslListener, start_link, [ListenName, AptSupName, Port, ListenOpts]},
         restart => permanent,
         shutdown => 3000,
         type => worker,
         modules => [ntSslListener]
      }],
   {ok, {SupFlag, ChildSpecs}}.





