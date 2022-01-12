-module(ntSslAcceptorSup).

-behaviour(supervisor).

-export([
   start_link/3
]).

-export([
   init/1
]).

-spec(start_link(SupName :: atom(), SslOpts :: list(), HandshakeTimeout :: timeout()) -> {ok, pid()}).
start_link(SupName, SslOpts, HandshakeTimeout) ->
   supervisor:start_link({local, SupName}, ?MODULE, {SslOpts, HandshakeTimeout}).

init({SslOpts, HandshakeTimeout}) ->
   SupFlags = #{strategy => simple_one_for_one, intensity => 100, period => 3600},

   Acceptor = #{
      id => ntSslAcceptor,
      start => {ntSslAcceptor, start_link, [SslOpts, HandshakeTimeout]},
      restart => transient,
      shutdown => 3000,
      type => worker,
      modules => [ntSslAcceptor]
   },
   {ok, {SupFlags, [Acceptor]}}.

