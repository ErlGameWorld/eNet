-module(ntTcpAcceptorSup).

-behaviour(supervisor).

-export([
   start_link/1
]).

-export([
   init/1
]).

-spec(start_link(SupName :: atom()) -> {ok, pid()}).
start_link(SupName) ->
   supervisor:start_link({local, SupName}, ?MODULE, undefined).

init(_Args) ->
   SupFlags = #{strategy => simple_one_for_one, intensity => 100, period => 3600},

   Acceptor = #{
      id => ntTcpAcceptor,
      start => {ntTcpAcceptor, start_link, []},
      restart => transient,
      shutdown => 3000,
      type => worker,
      modules => [ntTcpAcceptor]
   },
   {ok, {SupFlags, [Acceptor]}}.

