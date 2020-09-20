-module(max_time).


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
    blackboard:set(start_time, erlang:system_time(millisecond), Id, State).

-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := Id, children := Children, properties := #{max_time := MaxTime}} = _BTreeNode, State) ->
    case Children of
        [ChildId] ->
            StartTime = blackboard:get(start_time, Id, State),
            {Status, State1} = behavior_tree:do_execute(ChildId, State),
            case MaxTime > erlang:system_time(millisecond) - StartTime of
                true ->
                    {Status, State1};
                false ->
                    {?BT_FAILURE, State}
            end;
        [] ->
            {?BT_ERROR, State}
    end.

-spec close(bt_node(), bt_state()) -> bt_state().
close(#{id := Id} = _BTreeNode, State) ->
    blackboard:remove(start_time, Id, State).
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

