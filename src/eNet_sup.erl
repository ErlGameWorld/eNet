-module(eNet_sup).

-behaviour(supervisor).

-export([
   start_link/0
]).

-export([
   init/1
]).

-spec(start_link() -> {ok, pid()} | {error, term()}).
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

init(_Args) ->
   SupFlag = #{strategy => one_for_one, intensity => 100, period => 3600},
   ChildSpecs = [],
   {ok, {SupFlag, ChildSpecs}}.
