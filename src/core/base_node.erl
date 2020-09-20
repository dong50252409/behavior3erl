-module(base_node).


%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

-compile([inline, {inline_size, 100}]).

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([is_open/1, do_execute/2]).

%% 内部调用
-export([do_init/3, do_open/2, do_tick/2, do_close/2]).

%% 节点初始化
-callback(init(bt_node()) -> bt_node()).

%% 开启节点
-callback(open(bt_node(), bt_state()) -> bt_state()).

%% 执行节点
-callback(tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}).

%% 关闭节点
-callback(close(bt_node(), bt_state()) -> bt_state()).

-optional_callbacks([init/1, open/2, close/2]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
%% @doc 检查节点开启状态
is_open(Id) ->
    BTreeNode = blackboard:get_btree_node(Id),
    maps:get(is_open, BTreeNode).

%% @doc 执行节点
-spec do_execute(bt_uid(), bt_state()) -> {bt_status(), bt_state()}.
do_execute(TreeNodeId, State) ->
    BTreeNode = blackboard:get_btree_node(TreeNodeId),
    State1 = do_open(BTreeNode, State),
    {Status, State2} = do_tick(BTreeNode, State1),
    case Status of
        ?BT_RUNNING ->
            {?BT_RUNNING, State2};
        Status ->
            State3 = do_close(BTreeNode, State2),
            {Status, State3}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @doc 初始化行为树
-spec do_init(bt_node_id(), #{bt_node_id()=> btree()}, tree_nodes()) -> bt_uid().
do_init(ChildId, TreeMaps, TreeNodeMaps) ->
    case behavior_tree:get_btree_node(ChildId, TreeNodeMaps) of
        #{name := Name, category := tree} ->
            #{root_id := RootId} = behavior_tree:get_btree(Name, TreeMaps),
            do_init(RootId, TreeMaps, TreeNodeMaps);
        #{name := Name, children := Children} = BTreeNode ->
            case code:ensure_loaded(Name) of
                {module, Mod} ->
                    case erlang:function_exported(Mod, init, 1) of
                        true ->
                            BTreeNode1 = Mod:init(BTreeNode);
                        false ->
                            BTreeNode1 = BTreeNode
                    end,
                    Id = make_ref(),
                    BTreeNode2 = BTreeNode1#{
                        id => Id,
                        bt_node_id => ChildId,
                        children => [do_init(CId, TreeMaps, TreeNodeMaps) || CId <- Children],
                        is_open => false
                    },
                    blackboard:set_btree_node(BTreeNode2),
                    Id;
                _ ->
                    erlang:error({btree_node_not_implement, Name})
            end
    end.

%% @doc
%% 如果树节点没有开启，并且有open/2函数实现，则执行open函数，
%% 如果树节点已经开启，则跳过
-spec do_open(bt_uid(), bt_state()) -> bt_state().
do_open(BTreeNode, State) ->
    case BTreeNode of
        #{is_open := true} ->
            State;
        #{name := Mod, is_open := false} = BTreeNode ->
            case erlang:function_exported(Mod, open, 2) of
                true ->
                    ?BT_PT(),
                    State1 = Mod:open(BTreeNode, State);
                false ->
                    State1 = State
            end,
            blackboard:set_btree_node(BTreeNode#{is_open := true}),
            State1
    end.


%% @doc 执行树节点tick函数
-spec do_tick(bt_uid(), bt_state()) -> {bt_status(), bt_state()}.
do_tick(#{name := Mod} = BTreeNode, State) ->
    ?BT_PT(),
    case Mod:tick(BTreeNode, State) of
        {?BT_ERROR, State1} ->
            ?BT_LOG("BTreeNode Error! Mod:~w State:~w", [Mod, State1]),
            {?BT_ERROR, State1};
        {Status, State1} ->
            {Status, State1}
    end.


%% @doc 如果树节点有close/2函数实现，则执行close函数
-spec do_close(bt_uid(), bt_state()) -> bt_state().
do_close(#{id := Id, name := Mod} = BTreeNode, State) ->
    case erlang:function_exported(Mod, close, 2) of
        true ->
            ?BT_PT(),
            State1 = Mod:close(BTreeNode, State);
        false ->
            State1 = State
    end,
    State2 = blackboard:remove(Id, State1),
    blackboard:set_btree_node(BTreeNode#{is_open := false}),
    State2.

