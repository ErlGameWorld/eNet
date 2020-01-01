-module(erlNetLib_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),             % optional
%%                 intensity => non_neg_integer(),     % optional
%%                 period => pos_integer()}            % optional
%% child_spec() = #{id => child_id(),                  % mandatory
%%                  start => mfargs(),                 % mandatory
%%                  restart => restart(),              % optional
%%                  shutdown => shutdown(),            % optional
%%                  type => worker(),                  % optional
%%                  modules => modules()}              % optional
init1([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.


init([]) ->
   SupFlags = #{strategy => one_for_one, intensity  => 1000, period => 3600},
   NetListen = #{id => netListen, start => {netListen, start_link, []}, restart => permanent, shutdown => 5000, type => supervisor, modules => [netListen]},
   NetAcceptor = #{id => netAcceptor, start => {netAcceptor, start_link, []}, restart => permanent, shutdown => 5000, type => supervisor, modules => [netAcceptor]},
   {ok, {SupFlags, [NetListen, NetAcceptor]}}.


%%====================================================================
%% Internal functions
%%====================================================================
-spec startChild(supervisor:child_spec()) -> {ok, Pid} | {error, term()}.
startChild(ChildSpec) ->
   supervisor:start_child(?MODULE, ChildSpec).
