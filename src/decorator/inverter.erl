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
tick(#{children := [ChildID]} = _BTNode, BTState) ->
    case base_node:execute(ChildID, BTState) of
        {?BT_SUCCESS, BTState1} ->
            {?BT_FAILURE, BTState1};
        {?BT_FAILURE, BTState1} ->
            {?BT_SUCCESS, BTState1};
        {BTStatus, BTState1} ->
            {BTStatus, BTState1}
    end;
tick(_BTNode, BTState) ->
    {?BT_ERROR, BTState}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
