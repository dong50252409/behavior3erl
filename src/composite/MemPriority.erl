-module('MemPriority').

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
open(#{id := ID, children := Children} = _TreeNode, BTState) ->
    blackboard:set(running_children, Children, ID, BTState).

-spec tick(tree_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := ID} = _TreeNode, BTState) ->
    RunningChildren = blackboard:get(running_children, ID, BTState),
    tick_1(RunningChildren, ID, BTState).

-spec close(tree_node(), bt_state()) -> bt_state().
close(#{id := ID} = _TreeNode, State) ->
    blackboard:remove(running_children, ID, State).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1([ChildID | T] = Children, ID, BTState) ->
    case base_node:do_execute(ChildID, BTState) of
        {?BT_FAILURE, BTState1} ->
            tick_1(T, ID, BTState1);
        {?BT_RUNNING, BTState1} ->
            BTState2 = blackboard:set(running_children, Children, ID, BTState1),
            {?BT_RUNNING, BTState2};
        {BTStatus, BTState1} ->
            {BTStatus, BTState1}
    end;
tick_1([], _ID, BTState) ->
    {?BT_FAILURE, BTState}.
