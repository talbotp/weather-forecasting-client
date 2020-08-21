%%%-------------------------------------------------------------------
%%% @author parker
%%% @doc
%%% Endpoint for the weather forecasting app.
%%% @end
%%% Created : 21. Aug 2020 03:22
%%%-------------------------------------------------------------------
-module(weather_forecaster_api).
-author("parker").

%% API
-export([init/2]).

-define(OK,                     200).
-define(METHOD_NOT_ALLOWED,     405).
-define(INTERNAL_SERVER_ERROR,  500).
-define(NOT_IMPLEMENTED,        501).

-define(TEXT_PLAIN, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

init(Req0, Opts) ->
  Method = cowboy_req:method(Req0),
  Req1   = handle_req(Method, Req0),
  {ok, Req1, Opts}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Case of resetting the city mappings.
handle_req(<<"POST">>, Req) ->
  case weather_forecaster_city_cache:set_city_mappings() of
    ok ->
      Msg = <<"Successfully reloaded the city mappings.">>,
      cowboy_req:reply(?OK, ?TEXT_PLAIN, Msg, Req);
    error ->
      cowboy_req:reply(?INTERNAL_SERVER_ERROR, Req)
  end;
handle_req(<<"GET">>, Req) ->
  cowboy_req:reply(?NOT_IMPLEMENTED, Req);
handle_req(_InvalidMethod, Req) ->
  cowboy_req:reply(?METHOD_NOT_ALLOWED, Req).

