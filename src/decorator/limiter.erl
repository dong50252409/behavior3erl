-module(limiter).


%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([tick/2]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := Id, children := Children, properties := #{max_loop := MaxLoop}} = _BTreeNode, State) ->
    case Children of
        [ChildId] ->
            I = blackboard:get(i, Id, 0, State),
            case I < MaxLoop of
                true ->
                    {Status, State1} = behavior_tree:do_execute(ChildId, State),
                    case Status =:= ?BT_SUCCESS orelse Status =:= ?BT_FAILURE of
                        true ->
                            State2 = blackboard:set(i, I + 1, Id, State1),
                            {Status, State2};
                        false ->
                            {Status, State1}
                    end;
                false ->
                    {?BT_FAILURE, State}
            end;
        [] ->
            {?BT_ERROR, State}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

