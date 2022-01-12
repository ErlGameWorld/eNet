-module(ntTcpMgrSup).

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
   supervisor:start_link({local, SupName}, ?MODULE, {SupName, Port, ListenOpts}).

init({SupName, Port, ListenOpts}) ->
   SupFlag = #{strategy => one_for_one, intensity => 100, period => 3600},

   AptSupName = ntCom:asName(tcp, SupName),
   ListenName = ntCom:lsName(tcp, SupName),

   ChildSpecs = [
      #{
         id => AptSupName,
         start => {ntTcpAcceptorSup, start_link, [AptSupName]},
         restart => permanent,
         shutdown => infinity,
         type => supervisor,
         modules => [ntTcpAcceptorSup]
      },
      #{
         id => ListenName,
         start => {ntTcpListener, start_link, [ListenName, AptSupName, Port, ListenOpts]},
         restart => permanent,
         shutdown => 3000,
         type => worker,
         modules => [ntTcpListener]
      }],
   {ok, {SupFlag, ChildSpecs}}.





