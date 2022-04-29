-module('Inverter').

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
-spec tick(tree_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{children := [ChildID]} = _TreeNode, BTState) ->
    case base_node:do_execute(ChildID, BTState) of
        {?BT_SUCCESS, BTState1} ->
            {?BT_FAILURE, BTState1};
        {?BT_FAILURE, BTState1} ->
            {?BT_SUCCESS, BTState1};
        {BTStatus, BTState1} ->
            {BTStatus, BTState1}
    end;
tick(_TreeNode, BTState) ->
    {?BT_ERROR, BTState}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
