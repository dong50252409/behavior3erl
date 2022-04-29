-module('Wait').

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
-spec open(tree_node(), bt_state()) -> bt_state().
open(#{id := ID}, BTState) ->
    blackboard:set(start_time, erlang:system_time(millisecond), ID, BTState).

-spec tick(tree_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := ID, properties := #{milliseconds := EndTime}} = _TreeNode, BTState) ->
    StartTime = blackboard:get(start_time, ID, BTState),
    case erlang:system_time(millisecond) - StartTime > EndTime of
        true ->
            {?BT_SUCCESS, BTState};
        false ->
            {?BT_RUNNING, BTState}
    end.

-spec close(tree_node(), bt_state()) -> bt_state().
close(#{id := ID} = _TreeNode, BTState) ->
    blackboard:remove(start_time, ID, BTState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
