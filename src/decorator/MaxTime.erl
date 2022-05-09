-module('MaxTime').

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
    {blackboard:set(start_time, erlang:system_time(millisecond), ID, BB), State}.

-spec tick(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) ->
    {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
tick(#tree_node{id = ID, children = [ChildID], properties = #{max_time := MaxTime}}, BB, State) ->
    StartTime = blackboard:get(start_time, ID, BB),
    {BTStatus, BB1, State1} = base_node:execute_child(ChildID, BB, State),
    case MaxTime > erlang:system_time(millisecond) - StartTime of
        true ->
            {BTStatus, BB1, State1};
        false ->
            {?BT_FAILURE, BB1, State}
    end;
tick(_TreeNode, BB, State) ->
    {?BT_ERROR, BB, State}.

-spec close(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) -> {UpBB :: blackboard(), UpState :: term()}.
close(#tree_node{id = ID}, BB, State) ->
    {blackboard:remove(start_time, ID, BB), State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
