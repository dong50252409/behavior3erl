-module(wait).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([open/2, tick/2, close/2]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec open(bt_node(), bt_state()) -> bt_state().
open(#{id := ID}, BTState) ->
    blackboard:set(start_time, erlang:system_time(millisecond), ID, BTState).

-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := ID, properties := #{milliseconds := EndTime}} = _BTNode, BTState) ->
    StartTime = blackboard:get(start_time, ID, BTState),
    case erlang:system_time(millisecond) - StartTime > EndTime of
        true ->
            {?BT_SUCCESS, BTState};
        false ->
            {?BT_RUNNING, BTState}
    end.

-spec close(bt_node(), bt_state()) -> bt_state().
close(#{id := ID} = _BTNode, BTState) ->
    blackboard:remove(start_time, ID, BTState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
