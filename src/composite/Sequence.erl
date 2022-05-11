-module('Sequence').

-behaviour(base_node).
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
tick(#tree_node{children = Children}, BB, State) ->
    tick_1(Children, BB, State).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1([ChildID | T], BB, State) ->
    case base_node:execute_child(ChildID, BB, State) of
        {?BT_SUCCESS, BB1, State1} ->
            tick_1(T, BB1, State1);
        {BTStatus, BB1, State1} ->
            {BTStatus, BB1, State1}
    end;
tick_1([], BB, State) ->
    {?BT_SUCCESS, BB, State}.