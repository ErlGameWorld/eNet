-module(nlTcpMgrSup).
-include("eNet.hrl").

-behaviour(supervisor).

-export([
   start_link/0
   , init/1
   , startChild/1
   , terminateChild/1
   , deleteChild/1
]).


-spec(start_link() -> {ok, pid()} | {error, term()}).
start_link() ->
   supervisor:start_link({local, ?nlTcpMgrSup}, ?MODULE, undefined).

init(_Args) ->
   SupFlag = #{
      strategy => one_for_one,
      intensity => 100,
      period => 3600
   },

   TcpAcceptorSup = #{
      id => nlTcpAcceptorSup,
      start => {nlTcpAcceptorSup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [nlTcpAcceptorSup]
   },
   {ok, {SupFlag, [TcpAcceptorSup]}}.

-spec startChild(supervisor:child_spec()) -> {ok, pid()} | {error, term()}.
startChild(ChildSpec) ->
   supervisor:start_child(?nlTcpMgrSup, ChildSpec).

-spec terminateChild(supervisor:child_id()) -> 'ok' | {'error', term()}.
terminateChild(SpecId) ->
   supervisor:terminate_child(?nlTcpMgrSup, SpecId).

-spec deleteChild(supervisor:child_id()) -> 'ok' | {'error', term()}.
deleteChild(SpecId) ->
   supervisor:delete_child(?nlTcpMgrSup, SpecId).





