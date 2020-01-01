-module(nlTcpAcceptorSup).

-behaviour(supervisor).

-export([start_link/5]).

-export([start_acceptor/2
   , count_acceptors/1
]).

%% Supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Start Acceptor Supervisor.
-spec(start_link(pid(), esockd:sock_fun(), [esockd:sock_fun()], fun(), fun()) -> {ok, pid()}).
start_link(ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun) ->
   supervisor:start_link(?MODULE, [ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun]).

%% @doc Start a acceptor.
-spec(start_acceptor(pid(), inet:socket()) -> {ok, pid()} | ignore | {error, term()}).
start_acceptor(AcceptorSup, LSock) ->
   supervisor:start_child(AcceptorSup, [LSock]).

%% @doc Count acceptors.
-spec(count_acceptors(AcceptorSup :: pid()) -> pos_integer()).
count_acceptors(AcceptorSup) ->
   length(supervisor:which_children(AcceptorSup)).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun]) ->
   SupFlags = #{strategy => simple_one_for_one,
      intensity => 100,
      period => 3600
   },
   Acceptor = #{id => acceptor,
      start => {esockd_acceptor, start_link,
         [ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun]},
      restart => transient,
      shutdown => 1000,
      type => worker,
      modules => [esockd_acceptor]
   },
   {ok, {SupFlags, [Acceptor]}}.

