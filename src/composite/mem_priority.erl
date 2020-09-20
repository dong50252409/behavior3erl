-module(mem_priority).


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
open(#{id := Id, children := Children} = _BTreeNode, State) ->
    blackboard:set(running_children, Children, Id, State).

-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := Id} = _BTreeNode, State) ->
    RunningChildren = blackboard:get(running_children, Id, State),
    tick_1(RunningChildren, Id, State).

-spec close(bt_node(), bt_state()) -> bt_state().
close(#{id := Id} = _BTreeNode, State) ->
    blackboard:remove(running_children, Id, State).
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1([ChildId | T] = Children, Id, State) ->
    case behavior_tree:do_execute(ChildId, State) of
        {?BT_FAILURE, State1} ->
            tick_1(T, Id, State1);
        {?BT_RUNNING, State1} ->
            State2 = blackboard:set(running_children, Children, Id, State1),
            {?BT_RUNNING, State2};
        {Status, State1} ->
            {Status, State1}
    end;
tick_1([], _Id, State) ->
    {?BT_FAILURE, State}.