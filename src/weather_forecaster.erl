%%%-------------------------------------------------------------------
%%% @author parker
%%% @doc
%%% Main module which is used to query the darksky api.
%%% @end
%%% Created : 13. Aug 2020 14:12
%%%-------------------------------------------------------------------
-module(weather_forecaster).
-author("parker").

%% API
-export([
  init/0,
  current_forecast/2
]).

-define(GET, get).

-define(is_number(X), is_integer(X) orelse is_float(X)).

%%%===================================================================
%%% API
%%%===================================================================

init() ->
  ibrowse:start().

%%--------------------------------------------------------------------
%% Send a get request to darksky, and return the json result, or error.
%%--------------------------------------------------------------------
current_forecast(Latitude, Longitude) when ?is_number(Latitude) andalso ?is_number(Longitude) ->
  %% Fetch the json values from the darksky api.
  {ok, Json}  = query_darksky(Latitude, Longitude),
  JsonMap     = jsx:decode(list_to_binary(Json)),

  %% Fetch current weather summary.
  CurrentlyMap    = maps:get(<<"currently">>, JsonMap),
  CurrentSummary  = maps:get(<<"summary">>, CurrentlyMap),

  %% Fetch the hourly weather summary, and precipitation probability.
  HourlyMap           = maps:get(<<"hourly">>, JsonMap),
  HourlySummary       = maps:get(<<"summary">>, HourlyMap),
  [HourlyDataMap | _] = maps:get(<<"data">>, HourlyMap),
  HourlyPrecipProb    = maps:get(<<"precipProbability">>, HourlyDataMap),

  %% Return the binary summary of current forecast.
  <<
    <<"Current weather - ">>/binary, CurrentSummary/binary,
    <<", Today we will see - ">>/binary, HourlySummary/binary,
    <<" with a ">>/binary, (number_to_binary(HourlyPrecipProb))/binary,
    <<"% chance of rain.">>/binary
  >>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Send a get request to darksky, and return the json result, or error.
%%--------------------------------------------------------------------
query_darksky(Latitude, Longitude) ->
  {ok, DarkskyHost} = get_env(darksky_host),
  {ok, DarkskyKey}  = get_env(darksky_key),

  LatitudeList  = number_to_list(Latitude),
  LongitudeList = number_to_list(Longitude),

  DarkSkyURL = lists:flatten([DarkskyHost, DarkskyKey, "/", LatitudeList, ",", LongitudeList]),

  case ibrowse:send_req(DarkSkyURL, [], ?GET) of
    {ok, "200", _HttpParams, Json} ->
      {ok, Json};
    _ ->
      error
  end.

get_env(Key) when is_atom(Key) ->
  application:get_env(weather_forecaster, Key).

number_to_list(Integer) when is_integer(Integer) ->
  integer_to_list(Integer);
number_to_list(Float) when is_float(Float) ->
  float_to_list(Float).

number_to_binary(Integer) when is_integer(Integer) ->
  integer_to_binary(Integer);
number_to_binary(Float) when is_integer(Float) ->
  float_to_binary(Float).
