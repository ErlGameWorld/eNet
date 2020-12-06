-module(eNet_sup).
-behaviour(supervisor).

-export([
   start_link/0
   , init/1
   , startChild/1
]).

-spec(start_link() -> {ok, pid()} | {error, term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),             % optional
%%                 intensity => non_neg_integer(),     % optional
%%                 period => pos_integer()}            % optional
%% child_spec() = #{id => child_id(),                  % mandatory
%%                  start => mfargs(),                 % mandatory
%%                  restart => restart(),              % optional
%%                  shutdown => shutdown(),            % optional
%%                  type => worker(),                  % optional
%%                  modules => modules()}              % optional

init([]) ->
   SupFlag = #{strategy => one_for_one, intensity  => 1000, period => 3600},
   {ok, {SupFlag, []}}.

-spec startChild(supervisor:child_spec()) -> {ok, pid()} | {error, term()}.
startChild(ChildSpec) ->
   supervisor:start_child(?MODULE, ChildSpec).
