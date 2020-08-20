%%%-------------------------------------------------------------------
%%% @author parker
%%% @doc
%%% A gen server to handle requests to convert a city name to a list
%%% of corresponding Latitude and Longitudes, to make less large requests
%%% to the city conversion list.
%%% @end
%%% Created : 20. Aug 2020 17:15
%%%-------------------------------------------------------------------
-module(weather_forecaster_city_cache).
-author("parker").
-behaviour(gen_server).

-include("../include/logger.hrl").

%% API
-export([
  start_link/0,
  set_city_mappings/0,
  convert_city/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% gen_server handle call requests.
-define(SET_CITY_MAPPINGS,  set_city_mappings).
-define(CONVERT_CITY,       convert_city).

-define(GET, get).
-define(CITY_CONVERSION_LIST, wfc_city_conversion_list).

-record(weather_forecaster_city_converter_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Set the city mapping list as an environment variable, this halts all
%% convert requests, until the new env variable has been set.
%%--------------------------------------------------------------------
-spec set_city_mappings() -> ok | error.
set_city_mappings() ->
  gen_server:call(?MODULE, ?SET_CITY_MAPPINGS).

%%--------------------------------------------------------------------
%% Given a city name, fetch all the matching cities, with their latitude
%% and longitude, in the form of a map.
%%--------------------------------------------------------------------
-spec convert_city(City :: binary()) -> {ok, list(#{})}.
convert_city(City) when is_binary(City) ->
  gen_server:call(?MODULE, {?CONVERT_CITY, City}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #weather_forecaster_city_converter_state{}}.

handle_call(?SET_CITY_MAPPINGS, _From, State = #weather_forecaster_city_converter_state{}) ->
  {ok, ConversionURL} = weather_forecaster_env:get_env(city_conversion_url),
  Reply = case ibrowse:send_req(ConversionURL, [], ?GET) of
            {ok, "200", _HttpParams, CityJson} ->
              CityMap = jsx:decode(list_to_binary(CityJson)),
              weather_forecaster_env:set_env(?CITY_CONVERSION_LIST, CityMap),
              ?INFO("Successfully cached the city conversion list."),
              ok;
            _ ->
              ?ERROR("Failed to cache the city conversion list."),
              error
          end,
  {reply, Reply, State};
handle_call({?CONVERT_CITY, City}, _From, State = #weather_forecaster_city_converter_state{}) ->
  {ok, CityDataList} = weather_forecaster_env:get_env(?CITY_CONVERSION_LIST),
  {reply, filter_by_city_name(City, CityDataList), State}.

handle_cast(_Request, State = #weather_forecaster_city_converter_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #weather_forecaster_city_converter_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #weather_forecaster_city_converter_state{}) ->
  ok.

code_change(_OldVsn, State = #weather_forecaster_city_converter_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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