-module('Wait').

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
tick(#tree_node{id = ID, properties = #{milliseconds := EndTime}}, BB, State) ->
    StartTime = blackboard:get(start_time, ID, BB),
    case erlang:system_time(millisecond) - StartTime > EndTime of
        true ->
            {?BT_SUCCESS, BB, State};
        false ->
            {?BT_RUNNING, BB, State}
    end.

-spec close(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) -> {UpBB :: blackboard(), UpState :: term()}.
close(#tree_node{id = ID}, BB, State) ->
    {blackboard:remove(start_time, ID, BB), State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
