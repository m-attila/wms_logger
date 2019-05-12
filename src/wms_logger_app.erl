%%%-------------------------------------------------------------------
%% @doc wms_logger public API
%% @end
%%%-------------------------------------------------------------------

-module(wms_logger_app).
-behaviour(application).
-include("wms_logger.hrl").

%% Application callbacks
-export([start/2,
         stop/1]).

%%====================================================================
%% API
%%====================================================================
-spec start(Type :: application:start_type(), Args :: term()) ->
  {ok, Pid :: pid()} |
  {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
  init(),
  wms_logger_sup:start_link().


%%--------------------------------------------------------------------
-spec stop(State :: term()) ->
  ok.
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
  wms_logger:add_file_logger("debug.log", debug),
  wms_logger:add_file_logger("error.log", error),
  wms_logger:add_file_logger("info.log", info),
  wms_logger:set_console_level(debug),
  ?info("wms_logger initialized.").
