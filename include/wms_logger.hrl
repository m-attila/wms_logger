%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Headers for logging.
%%% @end
%%% Created : 11. May 2019 11:47
%%%-------------------------------------------------------------------
-author("Attila Makra").

-define(LAGER_MSG(Level, Format),
  _ = lager:Level(Format)).
-define(LAGER(Level, Format, Msg),
  _ = lager:Level(Format, Msg)).

%% =============================================================================
%% System event logging.
%% =============================================================================
-define(meta(Data), lager:md([{meta, Data}])).

-define(debug(Msg), ?LAGER_MSG(debug, "{sys} " ++ Msg)).
-define(debug(Format, Msg), ?LAGER(debug, "{sys} " ++ Format, Msg)).
-define(info(Msg), ?LAGER_MSG(info, "{sys} " ++ Msg)).
-define(info(Format, Msg), ?LAGER(info, "{sys} " ++ Format, Msg)).
-define(warning(Msg), ?LAGER_MSG(warning, "{sys} " ++ Msg)).
-define(warning(Format, Msg), ?LAGER(warning, "{sys} " ++ Format, Msg)).
-define(error(Msg), ?LAGER_MSG(error, "{sys} " ++ Msg)).
-define(error(Format, Msg), ?LAGER(error, "{sys} " ++ Format, Msg)).