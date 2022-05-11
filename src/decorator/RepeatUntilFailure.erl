-module('RepeatUntilFailure').

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
    I = blackboard:get(i, ID, BB),
    {I1, BTStatus, BB1, State1} = tick_1(MaxLoop, I, ChildID, ?BT_ERROR, BB, State),
    BB2 = blackboard:set(i, I1, ID, BB1),
    {BTStatus, BB2, State1};
tick(_TreeNode, BB, State) ->
    {?BT_ERROR, BB, State}.

-spec close(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) -> {UpBB :: blackboard(), UpState :: term()}.
close(#tree_node{id = ID}, BB, State) ->
    {blackboard:remove(i, ID, BB), State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1(MaxLoop, I, ChildID, _BTStatus, BB, State) when MaxLoop < 0 orelse I < MaxLoop ->
    case base_node:execute_child(ChildID, BB, State) of
        {?BT_SUCCESS, BB1, State1} ->
            tick_1(MaxLoop, I + 1, ChildID, ?BT_SUCCESS, BB1, State1);
        {BTStatus, BB1, State1} ->
            {I, BTStatus, BB1, State1}
    end;
tick_1(_MaxLoop, I, _ChildID, BTStatus, BB, State) ->
    {I, BTStatus, BB, State}.
