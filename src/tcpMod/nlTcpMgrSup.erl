-module(nlTcpMgrSup).
-include("erlNetLib.hrl").

-behaviour(supervisor).

-export([
   start_link/0
   , listener/1
   , acceptor_sup/1
   , connection_sup/1
   , init/1
]).


-spec(start_link() -> {ok, pid()} | {error, term()}).
start_link() ->
   supervisor:start_link({local, ?nlTcpMgrSup}, ?MODULE, undefined).

%% sup_flags() = #{strategy => strategy(),             % optional
%%                 intensity => non_neg_integer(),     % optional
%%                 period => pos_integer()}            % optional
%% child_spec() = #{id => child_id(),                  % mandatory
%%                  start => mfargs(),                 % mandatory
%%                  restart => restart(),              % optional
%%                  shutdown => shutdown(),            % optional
%%                  type => worker(),                  % optional
%%                  modules => modules()}              % optional

init(_Args) ->
   SupFlags = #{
      strategy => one_for_one,
      intensity => 100,
      period => 3600
   },

   NlTcpAcceptorSup = #{
      id => nlTcpAcceptorSup,
      start => {nlTcpAcceptorSup, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => supervior,
      modules => [nlTcpAcceptorSup]
   },
   {ok, {SupFlags, [NlTcpAcceptorSup]}}.

%% @doc Get listener.
-spec(listener(pid()) -> pid()).
listener(Sup) ->
   child_pid(Sup, listener).

%% @doc Get connection supervisor.
-spec(connection_sup(pid()) -> pid()).
connection_sup(Sup) -> child_pid(Sup, connection_sup).

%% @doc Get acceptor supervisor.
-spec(acceptor_sup(pid()) -> pid()).
acceptor_sup(Sup) -> child_pid(Sup, acceptor_sup).

%% @doc Get child pid with id.
child_pid(Sup, ChildId) ->
   hd([Pid || {Id, Pid, _, _}
      <- supervisor:which_children(Sup), Id =:= ChildId]).





