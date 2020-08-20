%%%-------------------------------------------------------------------
%%% @author parker
%%% @doc
%%% Interact with the application env.
%%% @end
%%% Created : 20. Aug 2020 17:25
%%%-------------------------------------------------------------------
-module(weather_forecaster_env).
-author("parker").

-include("../include/logger.hrl").

%% API
-export([
  get_env/1,
  get_env/2,
  set_env/2
]).

-define(APP, weather_forecaster).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_env(Key ::atom()) -> {ok, term()} | error.
get_env(Key) when is_atom(Key) ->
  case application:get_env(?APP, Key) of
    {ok, Value} ->
      {ok, Value};
    undefined ->
      ?ERROR("No application environment variable for Key=~p", [Key]),
      error
  end .

-spec get_env(Key ::atom(), Default :: term()) -> {ok, term()}.
get_env(Key, Default) when is_atom(Key) ->
  case application:get_env(?APP, Key) of
    {ok, _Value} = Return ->
      Return;
    undefined ->
      {ok, Default}
  end.

-spec set_env(Key :: atom(), Value :: term()) -> ok.
set_env(Key, Value) when is_atom(Key) ->
  application:set_env(?APP, Key, Value).