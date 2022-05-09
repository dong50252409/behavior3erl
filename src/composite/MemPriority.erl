-module('MemPriority').

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
open(#tree_node{id = ID, children = Children}, BB, State) ->
    {blackboard:set(running_children, Children, ID, BB), State}.

-spec tick(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) ->
    {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
tick(#tree_node{id = ID}, BB, State) ->
    RunningChildren = blackboard:get(running_children, ID, BB),
    tick_1(RunningChildren, ID, BB, State).

-spec close(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) -> {UpBB :: blackboard(), UpState :: term()}.
close(#tree_node{id = ID}, BB, State) ->
    {blackboard:remove(running_children, ID, BB), State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1([ChildID | T] = Children, ID, BB, State) ->
    case base_node:execute_child(ChildID, BB, State) of
        {?BT_FAILURE, BB1, State1} ->
            tick_1(T, ID, BB1, State1);
        {?BT_RUNNING, BB1, State1} ->
            BB2 = blackboard:set(running_children, Children, ID, BB1),
            {?BT_RUNNING, BB2, State1};
        {BTStatus, BB1, State1} ->
            {BTStatus, BB1, State1}
    end;
tick_1([], _ID, BB, State) ->
    {?BT_FAILURE, BB, State}.
