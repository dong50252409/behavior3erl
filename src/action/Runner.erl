-module('Runner').

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
tick(_TreeNode, BB, State) ->
    {?BT_RUNNING, BB, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
