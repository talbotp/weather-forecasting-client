%%%-------------------------------------------------------------------
%%% @author parker
%%% @doc
%%% Module which is used to query the darksky api.
%%% @end
%%% Created : 13. Aug 2020 14:12
%%%-------------------------------------------------------------------
-module(weather_forecaster).
-author("parker").

-include("../include/logger.hrl").

%% API
-export([
  current_forecast/1,
  current_forecast/2
]).

-define(GET, get).
-define(is_number(X), is_integer(X) orelse is_float(X)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% Fetch all latitude and longitudes from the given city name and then
%% print the current weather at that location.
%%--------------------------------------------------------------------
-spec current_forecast(City :: atom() | list() | binary()) -> ok.
current_forecast(City) when is_binary(City) ->
  case weather_forecaster_city_cache:convert_city(City) of
    [] ->
      ?INFO("No results found for ~p.", [City]);
    CityList when length(CityList) >= 1 ->
      lists:foreach(
        fun(CityDataMap) ->
          ?INFO("CityDataMap = ~p", [CityDataMap]),
          Latitude = maps:get(<<"lat">>, CityDataMap),
          Longitude = maps:get(<<"lng">>, CityDataMap),
          current_forecast(binary_to_list(Latitude), binary_to_list(Longitude))
        end, CityList)
  end;
current_forecast(City) when is_list(City) ->
  current_forecast(list_to_binary(City));
current_forecast(City) when is_atom(City) ->
  current_forecast(atom_to_list(City)).

%%--------------------------------------------------------------------
%% Fetch and print a binary message of the weather given a latitude and
%% longitude.
%%--------------------------------------------------------------------
-spec current_forecast(Latitude :: number() | list(), Longitude :: number() | list()) -> ok.
current_forecast(Latitude, Longitude) when is_list(Latitude) andalso is_list(Longitude) ->
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

  Result = <<
    <<"Current weather - ">>/binary, CurrentSummary/binary,
    <<", Today we will see - ">>/binary, HourlySummary/binary,
    <<" with a ">>/binary, (number_to_binary(HourlyPrecipProb))/binary,
    <<"% chance of rain.">>/binary
  >>,
  ?INFO(binary_to_list(Result));
current_forecast(Latitude, Longitude) when ?is_number(Latitude) andalso ?is_number(Longitude) ->
  current_forecast(number_to_list(Latitude), number_to_list(Longitude)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Send a get request to darksky, and return the json result, or error.
%%--------------------------------------------------------------------
query_darksky(Latitude, Longitude) ->
  {ok, DarkskyHost} = weather_forecaster_env:get_env(darksky_host),
  {ok, DarkskyKey}  = weather_forecaster_env:get_env(darksky_key),

  DarkSkyURL = lists:flatten([DarkskyHost, DarkskyKey, "/", Latitude, ",", Longitude]),

  case ibrowse:send_req(DarkSkyURL, [], ?GET) of
    {ok, "200", _HttpParams, Json} ->
      ?DEBUG("Fetched weather information from darksky with Lat=~p, Lng=~p", [Latitude, Longitude]),
      {ok, Json};
    Error ->
      ?ERROR("Error fetching data from darksky, Error=~p", [Error]),
      error
  end.

number_to_list(Integer) when is_integer(Integer) ->
  integer_to_list(Integer);
number_to_list(Float) when is_float(Float) ->
  float_to_list(Float).

number_to_binary(Integer) when is_integer(Integer) ->
  integer_to_binary(Integer);
number_to_binary(Float) when is_float(Float) ->
  float_to_binary(Float).
