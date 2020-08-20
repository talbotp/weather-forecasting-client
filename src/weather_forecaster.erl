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
current_forecast(City) when is_binary(City) ->
  {ok, ConversionURL} = get_env(city_conversion_url),
  {ok, "200", _HttpParams, Json} = ibrowse:send_req(ConversionURL, [], ?GET),
  CityMap = jsx:decode(list_to_binary(Json)),
  case filter_by_city_name(City, CityMap) of
    [] ->
      ?INFO("No results found for ~p.", [City]);
    CityList when length(CityList) >= 1 ->
      lists:foreach(
        fun(CityDataMap) ->
          ?INFO("CityDataMap = ~p", [CityDataMap]),
          Latitude = maps:get(<<"lat">>, CityDataMap),
          Longitude = maps:get(<<"lng">>, CityDataMap),
          current_forecast(Latitude, Longitude)
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
  Result = <<
    <<"Current weather - ">>/binary, CurrentSummary/binary,
    <<", Today we will see - ">>/binary, HourlySummary/binary,
    <<" with a ">>/binary, (number_to_binary(HourlyPrecipProb))/binary,
    <<"% chance of rain.">>/binary
  >>,
  ?INFO(binary_to_list(Result)).

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
      ?DEBUG("Fetched weather information from darksky with Lat=~p, Lng=~p", [Latitude, Longitude]),
      {ok, Json};
    Error ->
      ?ERROR("Error fetching data from darksky, Error=~p", [Error]),
      error
  end.

%%--------------------------------------------------------------------
%% Given a city and a List of maps containing city information,
%% return a list of cities with matching names.
%%--------------------------------------------------------------------
filter_by_city_name(City, CityDataList) ->
  lists:filter(
    fun(CityDataMap) ->
      case maps:find(<<"name">>, CityDataMap) of
        {ok, CurrentCityName} ->
          CurrentCityName =:= City;
        error ->
          false
      end
    end, CityDataList).

get_env(Key) when is_atom(Key) ->
  application:get_env(weather_forecaster, Key).

number_to_list(Integer) when is_integer(Integer) ->
  integer_to_list(Integer);
number_to_list(Float) when is_float(Float) ->
  float_to_list(Float).

number_to_binary(Integer) when is_integer(Integer) ->
  integer_to_binary(Integer);
number_to_binary(Float) when is_float(Float) ->
  float_to_binary(Float).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



-endif.