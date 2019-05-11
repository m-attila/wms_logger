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

-define(debug(Msg), ?LAGER_MSG(debug, Msg)).
-define(debug(Format, Msg), ?LAGER(debug, Format, Msg)).
-define(info(Msg), ?LAGER_MSG(info, Msg)).
-define(info(Format, Msg), ?LAGER(info, Format, Msg)).
-define(warning(Msg), ?LAGER_MSG(warning, Msg)).
-define(warning(Format, Msg), ?LAGER(warning, Format, Msg)).
-define(error(Msg), ?LAGER_MSG(error, Msg)).
-define(error(Format, Msg), ?LAGER(error, Format, Msg)).