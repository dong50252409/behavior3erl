-module(repeater).

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
open(#{id := ID} = _BTNode, BTState) ->
    blackboard:set(i, 0, ID, BTState).

-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := ID, children := [ChildID], properties := #{max_loop := MaxLoop}} = _BTNode, BTState) ->
    I = blackboard:get(i, ID, BTState),
    {I1, BTStatus, BTState1} = tick_1(MaxLoop, I, ChildID, ?BT_SUCCESS, BTState),
    BTState2 = blackboard:set(i, I1, ID, BTState1),
    {BTStatus, BTState2};
tick(_BTNode, BTState) ->
    {?BT_ERROR, BTState}.

-spec close(bt_node(), bt_state()) -> bt_state().
close(#{id := ID} = _BTNode, BTState) ->
    blackboard:remove(i, ID, BTState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1(MaxLoop, I, ChildID, _BTStatus, BTState) when MaxLoop < 0 orelse I < MaxLoop ->
    case base_node:execute(ChildID, BTState) of
        {BTStatus, BTState1} when BTStatus =:= ?BT_SUCCESS; BTStatus =:= ?BT_FAILURE ->
            tick_1(MaxLoop, I + 1, ChildID, BTStatus, BTState1);
        {BTStatus, BTState1} ->
            {I, BTStatus, BTState1}
    end;
tick_1(_MaxLoop, I, _ChildID, BTStatus, BTState) ->
    {I, BTStatus, BTState}.