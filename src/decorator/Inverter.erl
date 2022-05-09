-module('Inverter').

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").
%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([tick/3]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec tick(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) ->
    {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
tick(#tree_node{children = [ChildID]}, BB, State) ->
    case base_node:execute_child(ChildID, BB, State) of
        {?BT_SUCCESS, BB1, State1} ->
            {?BT_FAILURE, BB1, State1};
        {?BT_FAILURE, BB1, State1} ->
            {?BT_SUCCESS, BB1, State1};
        {BTStatus, BB1, State1} ->
            {BTStatus, BB1, State1}
    end;
tick(_TreeNode, BB, State) ->
    {?BT_ERROR, BB, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
