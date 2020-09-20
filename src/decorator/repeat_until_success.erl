-module(repeat_until_success).


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
open(#{id := Id} = _BTreeNode, State) ->
    blackboard:set(i, 0, Id, State).

-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := Id, children := Children, properties := #{max_loop := MaxLoop}} = _BTreeNode, State) ->
    case Children of
        [ChildId] ->
            I = blackboard:get(i, Id, State),
            {I1, Status, State1} = tick_1(MaxLoop, I, ChildId, ?BT_ERROR, State),
            State2 = blackboard:set(i, I1, Id, State1),
            {Status, State2};
        [] ->
            {?BT_ERROR, State}
    end.

-spec close(bt_node(), bt_state()) -> bt_state().
close(#{id := Id} = _BTreeNode, State) ->
    blackboard:remove(i, Id, State).
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1(MaxLoop, I, ChildId, _Status, State) when MaxLoop < 0 orelse I < MaxLoop ->
    case behavior_tree:do_execute(ChildId, State) of
        {?BT_FAILURE, State1} ->
            tick_1(MaxLoop, I + 1, ChildId, ?BT_FAILURE, State1);
        {Status, State1} ->
            {I, Status, State1}
    end;
tick_1(_MaxLoop, I, _ChildId, Status, State) ->
    {I, Status, State}.