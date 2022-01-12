-module(ntPptAcceptorSup).

-behaviour(supervisor).

-export([
   start_link/5
]).

-export([
   init/1
]).

-spec(start_link(SupName :: atom(), SslOpts :: list(), SslHSTet :: timeout(), ProxyPt :: boolean(), ProxyPtTet :: timeout()) -> {ok, pid()}).
start_link(SupName, SslOpts, SslHSTet, ProxyPt, ProxyPtTet) ->
   supervisor:start_link({local, SupName}, ?MODULE, {SslOpts, SslHSTet, ProxyPt, ProxyPtTet}).

init({SslOpts, SslHSTet, ProxyPt, ProxyPtTet}) ->
   SupFlags = #{strategy => simple_one_for_one, intensity => 100, period => 3600},

   Acceptor = #{
      id => ntPptAcceptor,
      start => {ntPptAcceptor, start_link, [SslOpts, SslHSTet, ProxyPt, ProxyPtTet]},
      restart => transient,
      shutdown => 3000,
      type => worker,
      modules => [ntPptAcceptor]
   },
   {ok, {SupFlags, [Acceptor]}}.

