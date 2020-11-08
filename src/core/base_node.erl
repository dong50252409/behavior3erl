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
execute(NodeID, State) ->
    BTNode = blackboard:get_btree_node(NodeID),
    State1 = do_open(BTNode, State),
    {Status, State2} = do_tick(BTNode, State1),
    case Status of
        ?BT_RUNNING ->
            {?BT_RUNNING, State2};
        Status ->
            State3 = do_close(BTNode, State2),
            {Status, State3}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% 初始化行为树
-spec do_init(bt_uid(), bt_node_id(), #{bt_node_id()=> btree()}, tree_nodes()) -> bt_uid().
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
                    ID;
                _ ->
                    erlang:error({btree_node_not_implement, Name})
            end
    end.

%% 如果树节点没有开启，并且有open/2函数实现，则执行open函数，
%% 如果树节点已经开启，则跳过
-spec do_open(bt_node(), bt_state()) -> bt_state().
do_open(#{id := ID, parent_id := ParentID, name := Mod} = BTNode, State) ->
    case blackboard:get('$is_open', ID, false, State) of
        true ->
            State;
        false ->
            case erlang:function_exported(Mod, open, 2) of
                true ->
                    State1 = Mod:open(BTNode, State);
                false ->
                    State1 = State
            end,
            State2 = blackboard:set('$is_open', true, ID, State1),
            case is_reference(ParentID) of
                true ->
                    Children = blackboard:get('$children', ParentID, [], State2),
                    blackboard:set('$children', [ID | Children], ParentID, State2);
                false ->
                    State2
            end
    end.


%% 执行树节点tick函数
-spec do_tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
do_tick(#{name := Mod} = BTNode, State) ->
    Mod:tick(BTNode, State).

%% @doc 如果树节点有close/2函数实现，则执行close函数
-spec do_close(bt_node(), bt_state()) -> bt_state().
do_close(#{id := ID, name := Mod} = BTNode, State) ->
    Children = blackboard:get('$children', ID, [], State),
    State1 = do_close_1(Children, State),
    case erlang:function_exported(Mod, close, 2) of
        true ->
            State2 = Mod:close(BTNode, State1);
        false ->
            State2 = State1
    end,
    State3 = blackboard:set('$is_open', false, ID, State2),
    State4 = blackboard:set('$children', [], ID, State3),
    blackboard:erase_node_local(ID, State4).

do_close_1([NodeID | T], State) ->
    State1 = do_close_1(T, State),
    case blackboard:get('$is_open', NodeID, false, State1) of
        true ->
            BTNode = blackboard:get_btree_node(NodeID),
            do_close(BTNode, State1);
        false ->
            State1
    end;
do_close_1([], State) ->
    State.