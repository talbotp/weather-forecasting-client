%%%-------------------------------------------------------------------
%% @doc
%% Start the weather forecaster app.
%% @end
%%%-------------------------------------------------------------------
-module(weather_forecaster_app).
-behaviour(application).

-export([
  start/2,
  stop/1
]).

start(_StartType, _StartArgs) ->
  {ok, Pid} = weather_forecaster_sup:start_link(),

  %% Load the city conversion list into app env.
  ok = weather_forecaster_city_cache:set_city_mappings(),

  {ok, Pid}.


stop(_State) ->
    ok.
