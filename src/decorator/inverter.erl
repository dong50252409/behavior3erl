-module(inverter).


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
tick(#{children := Children} = _BTreeNode, State) ->
    case Children of
        [ChildId] ->
            case behavior_tree:do_execute(ChildId, State) of
                {?BT_SUCCESS, State1} ->
                    {?BT_FAILURE, State1};
                {?BT_FAILURE, State1} ->
                    {?BT_SUCCESS, State1};
                {Status, State1} ->
                    {Status, State1}
            end;
        [] ->
            {?BT_ERROR, State}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
