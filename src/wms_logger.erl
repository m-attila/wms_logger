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

-spec add_file_logger(string(), atom()) ->
  ok.
add_file_logger(Filename, Level) ->
  Module = lager_file_backend,
  Path = filename:join("log", Filename),
  Id = Path,
  Config = [{file, Path}, {level, Level}],

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