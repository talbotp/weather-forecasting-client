%%%-------------------------------------------------------------------
%% @doc
%% weather_forecaster top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(weather_forecaster_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 0,
    period => 1},

  ChildSpecs = [
    city_converter_child_specs()
  ],

  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

city_converter_child_specs() ->
  #{id =>       weather_forecaster_city_cache,
    start =>    {weather_forecaster_city_cache, start_link, []},
    restart =>  permanent,
    shutdown => brutal_kill,
    type =>     worker,
    modules =>  [weather_forecaster_city_cache]}.
