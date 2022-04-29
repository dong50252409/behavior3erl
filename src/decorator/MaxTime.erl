-module('MaxTime').

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
open(#{id := ID} = _TreeNode, BTState) ->
    blackboard:set(start_time, erlang:system_time(millisecond), ID, BTState).

-spec tick(tree_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := ID, children := [ChildID] , properties := #{max_time := MaxTime}} = _TreeNode, BTState) ->
    StartTime = blackboard:get(start_time, ID, BTState),
    {BTStatus, BTState1} = base_node:do_execute(ChildID, BTState),
    case MaxTime > erlang:system_time(millisecond) - StartTime of
        true ->
            {BTStatus, BTState1};
        false ->
            {?BT_FAILURE, BTState}
    end;
tick(_TreeNode, BTState) ->
    {?BT_ERROR, BTState}.

-spec close(tree_node(), bt_state()) -> bt_state().
close(#{id := ID} = _TreeNode, BTState) ->
    blackboard:remove(start_time, ID, BTState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
