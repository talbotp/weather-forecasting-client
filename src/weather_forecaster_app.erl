%%%-------------------------------------------------------------------
%% @doc weather_forecaster public API
%% @end
%%%-------------------------------------------------------------------

-module(weather_forecaster_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    weather_forecaster_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
