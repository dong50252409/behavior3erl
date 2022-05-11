-module('Limiter').

-behaviour(base_node).
%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").
%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([open/3, tick/3, close/3]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec open(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) -> {UpBB :: blackboard(), UpState :: term()}.
open(#tree_node{id = ID}, BB, State) ->
    {blackboard:set(i, 0, ID, BB), State}.

-spec tick(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) ->
    {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
tick(#tree_node{id = ID, children = [ChildID], properties = #{max_loop := MaxLoop}}, BB, State) ->
    case blackboard:get(i, ID, BB) of
        I when I < MaxLoop ->
            case base_node:execute_child(ChildID, BB, State) of
                {BTStatus, BB1, State1} when BTStatus == ?BT_SUCCESS orelse BTStatus == ?BT_FAILURE ->
                    BB2 = blackboard:set(i, I + 1, ID, BB1),
                    {BTStatus, BB2, State1};
                {BTStatus, BB1, State1} ->
                    {BTStatus, BB1, State1}
            end;
        _I ->
            {?BT_FAILURE, BB, State}
    end;
tick(_TreeNode, BB, State) ->
    {?BT_ERROR, BB, State}.

-spec close(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) -> {UpBB :: blackboard(), UpState :: term()}.
close(#tree_node{id = ID}, BB, State) ->
    {blackboard:remove(i, ID, BB), State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
