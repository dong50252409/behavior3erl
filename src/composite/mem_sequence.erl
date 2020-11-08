-module(mem_sequence).

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
open(#{id := ID, children := Children} = _BTNode, BTState) ->
    blackboard:set(running_children, Children, ID, BTState).

-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := ID} = _BTNode, BTState) ->
    RunningChildren = blackboard:get(running_children, ID, BTState),
    tick_1(RunningChildren, ID, BTState).

-spec close(bt_node(), bt_state()) -> bt_state().
close(#{id := ID} = _BTNode, BTState) ->
    blackboard:remove(running_children, ID, BTState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1([ChildID | T] = Children, ID, BTState) ->
    case base_node:execute(ChildID, BTState) of
        {?BT_SUCCESS, BTState1} ->
            tick_1(T, ID, BTState1);
        {?BT_RUNNING, BTState1} ->
            BTState2 = blackboard:set(running_children, Children, ID, BTState1),
            {?BT_RUNNING, BTState2};
        {BTStatus, BTState1} ->
            {BTStatus, BTState1}
    end;
tick_1([], _ID, BTState) ->
    {?BT_SUCCESS, BTState}.
