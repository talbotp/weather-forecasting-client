%%%-------------------------------------------------------------------
%%% @author parker
%%% @doc
%%% Wrap the logging dependency.
%%% @end
%%% Created : 20. Aug 2020 16:22
%%%-------------------------------------------------------------------
-ifndef(LOGGER_HRL).
-define(LOGGER_HRL, "logger.hrl").

-define(INFO(Msg),          lager:info(Msg)).
-define(INFO(Msg, Args),    lager:info(Msg, Args)).

-define(ERROR(Msg),         lager:error(Msg)).
-define(ERROR(Msg, Args),   lager:error(Msg, Args)).

-define(DEBUG(Msg),         lager:debug(Msg)).
-define(DEBUG(Msg, Args),   lager:debug(Msg, Args)).

-define(WARNING(Msg),       lager:warning(Msg)).
-define(WARNING(Msg, Args), lager:warning(Msg, Args)).

-endif.