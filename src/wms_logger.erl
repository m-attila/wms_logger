%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Logger API module.
%%% @end
%%% Created : 11. May 2019 14:29
%%%-------------------------------------------------------------------
-module(wms_logger).
-author("Attila Makra").

%% API
-export([add_file_logger/2, set_console_level/1, set_file_level/2]).

%% =============================================================================
%% API functions
%% =============================================================================

-spec add_file_logger(string(), atom()) ->
  ok.
add_file_logger(Filename, Level) ->
  Module = lager_file_backend,
  Path = filename:join("log", Filename),
  Id = Path,
  Config = [{file, Path},
            {level, Level},
            {size, 10485760}, {date, "$D0"}, {count, 5},
            {formatter, lager_default_formatter}] ++ get_formatter_config(),

  supervisor:start_child(lager_handler_watcher_sup,
                         [lager_event, {Module, Id}, Config]),
  ok.

-spec set_console_level(atom()) ->
  ok.
set_console_level(Level) ->
  lager:set_loglevel(lager_console_backend, Level),
  ok.

-spec set_file_level(string(), atom()) ->
  ok.
set_file_level(Filename, Level) ->
  lager:set_loglevel(lager_file_backend, Filename, Level),
  ok.

%% =============================================================================
%% Private functions
%% =============================================================================
get_formatter_config() ->
  [{formatter_config, [
    "[",
    date,
    " ",
    time,
    "] ",
    "[", severity, "] ",
    "[", {application, "_"}, ":",
    {module, "_"}, ":", {function, "_"}, ":", {line, "_"}, "] ",
    "[", {pid, "_"}, "] ",
    "[",
    {meta, ["@@", meta, "@@"], ""},
    message, "] ",
    "\n"
  ]}].