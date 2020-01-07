-module(nlTcpAcceptorSup).

-behaviour(supervisor).

-export([
   start_link/0
   , init/1
   , startChild/1
]).

%% Start Acceptor Supervisor.
-spec(start_link() -> {ok, pid()}).
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

init(_Args) ->
   SupFlags = #{strategy => simple_one_for_one,
      intensity => 100,
      period => 3600
   },
   Acceptor = #{id => acceptor,
      start => {nlTcpAcceptor, start_link, []},
      restart => transient,
      shutdown => 1000,
      type => worker,
      modules => [nlTcpAcceptorIns]
   },
   {ok, {SupFlags, [Acceptor]}}.

startChild(ArgsList) ->
   supervisor:start_child(?MODULE, ArgsList).

