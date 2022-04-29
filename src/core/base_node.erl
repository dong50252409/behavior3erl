-module(base_node).

-compile([inline]).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([execute/3]).

%% Internal API
-export([do_execute/2, do_open/2, do_tick/2, do_close/2]).

%% 节点初始化
-callback(init(tree_node()) -> tree_node()).

%% 开启节点
-callback(open(tree_node(), bt_state()) -> bt_state()).

%% 执行节点
-callback(tick(tree_node(), bt_state()) -> {bt_status(), bt_state()}).

%% 关闭节点
-callback(close(tree_node(), bt_state()) -> bt_state()).

-optional_callbacks([init/1, open/2, close/2]).

-define(IS_OPEN, '$is_open').

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
%% @doc 执行节点
execute(TreeMod, Title, BTState) ->
    NodeID = TreeMod:get_tree_by_title(Title),
    UpBTState = blackboard:set_running_info(TreeMod, Title, BTState),
    do_execute(NodeID, UpBTState).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
do_execute(NodeID, BTState) ->
    TreeMode = blackboard:get_tree_mod(BTState),
    TreeNode = TreeMode:get_node(NodeID),
    BTState1 = do_open(TreeNode, BTState),
    case do_tick(TreeNode, BTState1) of
        {?BT_RUNNING, BTState2} ->
            {?BT_RUNNING, BTState2};
        {BTStatus, BTState2} ->
            BTState3 = do_close(TreeNode, BTState2),
            {BTStatus, BTState3}
    end.

-ifndef(BT_DEBUG).

%% @doc
%% 如果树节点没有开启，并且有open/2函数实现，则执行open函数，
%% 如果树节点已经开启，则跳过
do_open(#tree_node{id = ID, name = Mod} = TreeNode, BTState) ->
    case blackboard:get(?IS_OPEN, ID, false, BTState) of
        true ->
            BTState;
        false ->
            case erlang:function_exported(Mod, open, 2) of
                true ->
                    UpBTState = Mod:open(TreeNode, BTState);
                false ->
                    UpBTState = BTState
            end,
            blackboard:set(?IS_OPEN, true, ID, UpBTState)
    end.

%% @doc
%% 执行树节点tick函数
do_tick(#tree_node{name = Mod} = TreeNode, BTState) ->
    Mod:tick(TreeNode, BTState).

%% @doc
%% 如果树节点有close/2函数实现，则执行close函数
do_close(#tree_node{id = ID, name = Mod} = TreeNode, BTState) ->
    case erlang:function_exported(Mod, close, 2) of
        true ->
            UpBTState = Mod:close(TreeNode, BTState);
        false ->
            UpBTState = BTState
    end,
    blackboard:erase_node(ID, UpBTState).

-else.

do_open(#tree_node{id = ID, name = Mod} = TreeNode, BTState) ->
    case blackboard:get(?IS_OPEN, ID, false, BTState) of
        true ->
            BTState;
        false ->
            case erlang:function_exported(Mod, open, 2) of
                true ->
                    UpBTState = Mod:open(TreeNode, BTState),
                    case lists:member(Mod, ?SKIP_MOD) of
                        true ->
                            blackboard:set(?IS_OPEN, true, ID, BTState);
                        false ->
                            {IO, FinalBtState} = blackboard:get_log_file(UpBTState),
                            ?BT_DEBUG_LOG(IO, "do_open Name:~w Blackboard:~w~nBTState:~tp~n", [
                                Mod, blackboard:get_tree_maps(UpBTState), blackboard:erase_all_tree(UpBTState)
                            ]),
                            blackboard:set(?IS_OPEN, true, ID, FinalBtState)
                    end;
                false ->
                    blackboard:set(?IS_OPEN, true, ID, BTState)
            end
    end.

do_tick(#tree_node{name = Mod, children = Children} = TreeNode, BTState) ->
    {UpBTStatus, UpBTState} = Mod:tick(TreeNode, BTState),
    case lists:member(Mod, ?SKIP_MOD) of
        true ->
            {UpBTStatus, UpBTState};
        false ->
            {IO, FinalBtState} = blackboard:get_log_file(UpBTState),
            TreeMod = blackboard:get_tree_mod(BTState),
            ?BT_DEBUG_LOG(IO, "do_tick TreeNode:~w~nBlackboard:~w~nReply:~w~n", [
                TreeNode#tree_node{children = [(TreeMod:get_node(ChildId))#tree_node.name || ChildId <- Children]},
                blackboard:get_tree_maps(UpBTState),
                {UpBTStatus, blackboard:erase_all_tree(UpBTState)}
            ]),
            {UpBTStatus, FinalBtState}
    end.

do_close(#tree_node{id = ID, name = Mod} = TreeNode, BTState) ->
    case erlang:function_exported(Mod, close, 2) of
        true ->
            UpBTState = Mod:close(TreeNode, BTState),
            case lists:member(Mod, ?SKIP_MOD) of
                true ->
                    blackboard:erase_node(ID, UpBTState);
                false ->
                    {IO, FinalBtState} = blackboard:get_log_file(UpBTState),
                    ?BT_DEBUG_LOG(IO, "do_close Name:~w Blackboard:~w~nBTState:~w~n", [
                        Mod, blackboard:get_tree_maps(UpBTState), blackboard:erase_all_tree(UpBTState)
                    ]),
                    blackboard:erase_node(ID, FinalBtState)
            end;
        false ->
            blackboard:erase_node(ID, BTState)
    end.

-endif.

