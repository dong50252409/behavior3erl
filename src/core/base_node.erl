-module(base_node).

-compile([inline, {inline_size, 100}]).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([execute/2]).

%% Internal API
-export([do_init/4, do_open/2, do_tick/2, do_close/2]).

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
%% @doc 执行节点
-spec execute(bt_uid(), bt_state()) -> {bt_status(), bt_state()}.
execute(NodeID, BTState) ->
    BTNode = blackboard:get_btree_node(NodeID),
    BTState1 = do_open(BTNode, BTState),
    {BTStatus, BTState2} = do_tick(BTNode, BTState1),
    case BTStatus of
        ?BT_RUNNING ->
            {?BT_RUNNING, BTState2};
        BTStatus ->
            BTState3 = do_close(BTNode, BTState2),
            {BTStatus, BTState3}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% 初始化行为树
-spec do_init(bt_uid()|undefined, bt_node_id(), #{bt_node_id() => btree()}, tree_nodes()) -> {ok, bt_uid()}|{error, term()}.
do_init(ParentNodeID, BTNodeID, TreeMaps, TreeNodeMaps) ->
    case behavior_tree:get_btree_node(BTNodeID, TreeNodeMaps) of
        #{name := Name, category := tree} ->
            #{root := Root} = behavior_tree:get_btree(Name, TreeMaps),
            do_init(ParentNodeID, Root, TreeMaps, TreeNodeMaps);
        #{name := Name, children := Children} = BTNode ->
            case code:ensure_loaded(Name) of
                {module, Mod} ->
                    case erlang:function_exported(Mod, init, 1) of
                        true ->
                            BTNode1 = Mod:init(BTNode);
                        false ->
                            BTNode1 = BTNode
                    end,
                    ID = make_ref(),
                    BTNode2 = BTNode1#{
                        id => ID,
                        bt_node_id => BTNodeID,
                        parent_id => ParentNodeID,
                        children => [do_init(ID, ChildBTNodeID, TreeMaps, TreeNodeMaps) || ChildBTNodeID <- Children]
                    },
                    blackboard:set_btree_node(BTNode2),
                    {ok, ID};
                _ ->
                    {error, {btree_node_not_implement, Name}}
            end
    end.

%% 如果树节点没有开启，并且有open/2函数实现，则执行open函数，
%% 如果树节点已经开启，则跳过
-spec do_open(bt_node(), bt_state()) -> bt_state().
do_open(#{id := ID, parent_id := ParentID, name := Mod} = BTNode, BTState) ->
    case blackboard:get('$is_open', ID, false, BTState) of
        true ->
            BTState;
        false ->
            case erlang:function_exported(Mod, open, 2) of
                true ->
                    BTState1 = Mod:open(BTNode, BTState);
                false ->
                    BTState1 = BTState
            end,
            BTState2 = blackboard:set('$is_open', true, ID, BTState1),
            case is_reference(ParentID) of
                true ->
                    Children = blackboard:get('$children', ParentID, [], BTState2),
                    blackboard:set('$children', [ID | Children], ParentID, BTState2);
                false ->
                    BTState2
            end
    end.


%% 执行树节点tick函数
-spec do_tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
do_tick(#{name := Mod} = BTNode, BTState) ->
    Mod:tick(BTNode, BTState).

%% @doc 如果树节点有close/2函数实现，则执行close函数
-spec do_close(bt_node(), bt_state()) -> bt_state().
do_close(#{id := ID, name := Mod} = BTNode, BTState) ->
    Children = blackboard:get('$children', ID, [], BTState),
    BTState1 = do_close_1(Children, BTState),
    case erlang:function_exported(Mod, close, 2) of
        true ->
            BTState2 = Mod:close(BTNode, BTState1);
        false ->
            BTState2 = BTState1
    end,
    BTState3 = blackboard:set('$is_open', false, ID, BTState2),
    BTState4 = blackboard:set('$children', [], ID, BTState3),
    blackboard:erase_node_local(ID, BTState4).

do_close_1([NodeID | T], BTState) ->
    BTState1 = do_close_1(T, BTState),
    case blackboard:get('$is_open', NodeID, false, BTState1) of
        true ->
            BTNode = blackboard:get_btree_node(NodeID),
            do_close(BTNode, BTState1);
        false ->
            BTState1
    end;
do_close_1([], BTState) ->
    BTState.