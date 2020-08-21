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

%%%===================================================================
%%% API
%%%===================================================================

start(_StartType, _StartArgs) ->
  {ok, Pid} = weather_forecaster_sup:start_link(),

  %% Load the city conversion list into app env.
  ok = weather_forecaster_city_cache:set_city_mappings(),

  %% Setup http endpoints.
  {ok, _CowboyPid} = cowboy_setup(),

  {ok, Pid}.

stop(_State) ->
    ok = cowboy:stop_listener(http).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Setup our cowboy dependency.
cowboy_setup() ->
  {ok, Port}          = weather_forecaster_env:get_env(cowboy_port),
  {ok, DispatchList}  = weather_forecaster_env:get_env(cowboy_dispatch_list),
  Dispatch            = cowboy_router:compile([{'_', DispatchList}]),
  {ok, _Pid} = cowboy:start_clear(
    http,
    [{port, Port}],
    #{env => #{dispatch => Dispatch}}
  ).

