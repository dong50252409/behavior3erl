-module('Sequence').

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
tick(#{children := Children} = _TreeNode, BTState) ->
    tick_1(Children, BTState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1([ChildID | T], BTState) ->
    case base_node:do_execute(ChildID, BTState) of
        {?BT_SUCCESS, BTState1} ->
            tick_1(T, BTState1);
        {BTStatus, BTState1} ->
            {BTStatus, BTState1}
    end;
tick_1([], BTState) ->
    {?BT_SUCCESS, BTState}.
