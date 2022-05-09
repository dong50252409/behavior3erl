-module(base_node).

-compile([inline]).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([execute/2, execute_child/3]).

%% @doc
%% 如果节点模块未标记开启，并且导出了open/3函数，则执行open/3函数，
%% 如果节点模块已标记开启，则跳过
-callback(open(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) ->
    {UpBB :: blackboard(), UpState :: term()}).

%% @doc
%% 执行节点模块tick/3函数
-callback(tick(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) ->
    {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}).

%% @doc
%% 如果节点模块导出了close/3函数，则执行close/3函数
-callback(close(TreeNode :: tree_node(), BB :: blackboard(), State :: term()) ->
    {UpBB :: blackboard(), UpState :: term()}).

-optional_callbacks([open/3, close/3]).

-define(IS_OPEN, is_open).

-define(SKIP_MOD, [
    'MemPriority', 'MemSequence', 'Priority', 'Sequence',
    'Repeater', 'RepeatUntilFailure', 'RepeatUntilSuccess'
]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

%% @doc 执行行为树
-spec execute(BB :: blackboard(), State :: term()) -> {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
execute(BB, State) ->
    RootNodeID = blackboard:get_root_node_id(BB),
    execute_child(RootNodeID, BB, State).

%% @doc 执行行为树子节点
-spec execute_child(NodeID :: node_id(), BB :: blackboard(), State :: term()) ->
    {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
execute_child(NodeID, BB, State) ->
    TreeMod = blackboard:get_tree_mod(BB),
    TreeNode = TreeMod:get_node(NodeID),
    {BB1, State1} = do_open(TreeNode, BB, State),
    case do_tick(TreeNode, BB1, State1) of
        {?BT_RUNNING, BB2, State2} ->
            {?BT_RUNNING, BB2, State2};
        {BTStatus, BB2, State2} ->
            {BB3, State3} = do_close(TreeNode, BB2, State2),
            {BTStatus, BB3, State3}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
do_open(#tree_node{id = ID, name = Mod} = TreeNode, BB, State) ->
    case blackboard:get(?IS_OPEN, ID, false, BB) of
        true ->
            {BB, State};
        false ->
            case erlang:function_exported(Mod, open, 3) of
                true ->
                    {BB1, State1} = Mod:open(TreeNode, BB, State),
                    debug_log("do_open TreeNode:~tp~nBlackboard:~p~nState:~tp~n", TreeNode, BB1, State1);
                false ->
                    BB1 = BB, State1 = State
            end,
            BB2 = blackboard:set(?IS_OPEN, true, ID, BB1),
            {BB2, State1}
    end.

do_tick(#tree_node{name = Mod} = TreeNode, BB, State) ->
    {BTStatus, BB1, State1} = Mod:tick(TreeNode, BB, State),
    debug_log("do_tick TreeNode:~tp~nBlackboard:~p~nStatus:~w~nState:~tp~n", TreeNode, BB1, State1),
    {BTStatus, BB1, State1}.

do_close(#tree_node{id = ID, name = Mod} = TreeNode, BB, State) ->
    case erlang:function_exported(Mod, close, 3) of
        true ->
            {BB1, State1} = Mod:close(TreeNode, State),
            debug_log("do_close TreeNode:~tp~nBlackboard:~p~nState:~tp~n", TreeNode, BB1, State1);
        false ->
            BB1 = BB, State1 = State
    end,
    BB2 = blackboard:erase_node(ID, BB1),
    {BB2, State1}.

-ifndef(BT_DEBUG).
debug_log(_Format, _TreeNode, _BB, _State) ->
    ok.
-else.
debug_log(Format, #tree_node{name = Mod, children = Children} = TreeNode, BB, State) ->
    case lists:member(Mod, ?SKIP_MOD) of
        true ->
            ok;
        false ->
            TreeMod = blackboard:get_tree_mod(BB),
            ?BT_DEBUG_LOG(blackboard:get_io(BB), Format, [
                TreeNode#tree_node{children = [(TreeMod:get_node(ChildId))#tree_node.name || ChildId <- Children]},
                blackboard:get_global_maps(BB), State
            ])
    end.
-endif.

