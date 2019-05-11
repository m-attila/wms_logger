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
  wms_logger_sup:start_link().


%%--------------------------------------------------------------------
-spec stop(State :: term()) ->
  ok.
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
