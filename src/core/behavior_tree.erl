-module(behavior_tree).


-compile([inline, {inline_size, 100}]).
%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([load_tree_file/1, init_btree_by_title/4, init_btree/3, execute/2, execute_sub_tree/2, close_btree_node/2, unload_btree/1]).

%% 内部调用
-export([get_btree_id_by_title/2, get_btree/2, get_btree_node/2, do_execute/2]).
%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
%% @doc 载入行为树文件
-spec load_tree_file(file:name_all()) -> {#{string() => bt_node_id()}, #{bt_node_id()=> btree()}, tree_nodes()}|{error, term()}.
load_tree_file(JsonConfig) ->
    case file:read_file(JsonConfig) of
        {ok, Json} ->
            JsonTerm = jsx:decode(Json, [return_maps]),
            parse_btree(JsonTerm);
        {error, Reason} ->
            ?BT_LOG("Error:~w Reason:~w JsonConfig:~ts", [error, Reason, JsonConfig]),
            {error, Reason}
    end.

%% @doc 获取行为树根节点id
-spec get_btree_id_by_title(string(), #{string() => bt_node_id()}) -> bt_node_id()|undefined.
get_btree_id_by_title(Title, TitleMaps) ->
    case TitleMaps of
        #{Title := Id} ->
            Id;
        #{} ->
            undefined
    end.

%% @doc 获取行为树根节点
-spec get_btree(bt_node_id(), #{bt_node_id()=> btree()}) -> btree()|undefined.
get_btree(Id, TreeMaps) ->
    case TreeMaps of
        #{Id := BTree} ->
            BTree;
        #{} ->
            undefined
    end.

%% @doc 获取行为树子节点
-spec get_btree_node(bt_node_id(), tree_nodes()) -> uninit_bt_node()|undefined.
get_btree_node(Id, TreeNodeMaps) ->
    case TreeNodeMaps of
        #{Id := BTreeNode} ->
            BTreeNode;
        #{} ->
            undefined
    end.

%% @doc 根据给定节点名初始化整颗行为树
-spec init_btree_by_title(string(), #{string() => bt_node_id()}, #{bt_node_id()=> btree()}, tree_nodes()) -> {ok, bt_uid()}.
init_btree_by_title(Title, TitleMaps, TreeMaps, TreeNodeMaps) ->
    Id = get_btree_id_by_title(Title, TitleMaps),
    init_btree(Id, TreeMaps, TreeNodeMaps).

%% @doc 根据给定节点初始化整颗行为树
-spec init_btree(bt_node_id(), #{bt_node_id()=> btree()}, tree_nodes()) -> {ok, bt_uid()}.
init_btree(RootId, TreeMaps, TreeNodeMaps) ->
    do_init_btree(RootId, TreeMaps, TreeNodeMaps).

%% @doc 执行行为树节点
-spec execute(bt_uid(), bt_state()) -> {bt_status(), bt_state()}.
execute(Id, State) ->
    case do_execute(Id, State) of
        {?BT_RUNNING, State1} ->
            {?BT_RUNNING, State1};
        {Status, State1} ->
            State2 = do_close_btree_node(Id, State1),
            State3 = blackboard:remove('$global', State2),
            {Status, State3}
    end.

%% @doc 动态执行子树节点
execute_sub_tree(Id, State) ->
    case do_execute(Id, State) of
        {?BT_RUNNING, State1} ->
            {?BT_RUNNING, State1};
        {Status, State1} ->
            State2 = do_close_btree_node(Id, State1),
            {Status, State2}
    end.

%% @doc 关闭指定节点下的所有子节点
-spec close_btree_node(bt_uid(), bt_status()) -> bt_status().
close_btree_node(Id, State) ->
    State1 = do_close_btree_node(Id, State),
    blackboard:remove('$global', State1).

%% @doc 卸载指定节点下的所有子节点
-spec unload_btree(bt_uid()) -> ok.
unload_btree(Id) ->
    do_unload_btree_node(Id),
    ok.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @doc 解析行为树
-spec parse_btree(map()) -> {#{string() => bt_node_id()}, #{bt_node_id()=> btree()}, tree_nodes()}.
parse_btree(#{<<"trees">> := Trees}) ->
    parse_btree_1(Trees, #{}, #{}, #{}).

parse_btree_1([Node | T], TitleMaps, TreeMaps, TreeNodeMaps) ->
    #{<<"id">> := Id, <<"title">> := Title, <<"root">> := RootId, <<"nodes">> := Nodes} = Node,
    Iterator = maps:iterator(Nodes),
    {TreeNodes, TreeNodeMaps1} = parse_btree_node(maps:next(Iterator), #{}, TreeNodeMaps),
    BTree = #{
        id => Id,
        root_id => RootId,
        properties => parse_properties(Node),
        tree_nodes => TreeNodes
    },
    parse_btree_1(T, TitleMaps#{Title => Id}, TreeMaps#{Id => BTree}, TreeNodeMaps1);
parse_btree_1([], TitleMaps, TreeMaps, TreeNodeMaps) ->
    {TitleMaps, TreeMaps, TreeNodeMaps}.

%% @doc 解析树节点
-spec parse_btree_node(none | {bt_node_id(), map(), maps:iterator()}, tree_nodes(), tree_nodes()) -> {tree_nodes(), tree_nodes()}.
parse_btree_node({Id, Node, NextIterator}, TreeNodes, TreeNodeMaps) ->
    #{<<"name">> := Name, <<"category">> := Category} = Node,
    case Category of
        <<"tree">> ->
            Name1 = Name;
        _ ->
            Name1 = name_convert(Name)
    end,
    TreeNode = #{
        id => Id,
        name => Name1,
        category => binary_to_atom(Category, utf8),
        properties => parse_properties(Node),
        children => parse_children(Node)
    },
    parse_btree_node(maps:next(NextIterator), TreeNodes#{Id => TreeNode}, TreeNodeMaps#{Id => TreeNode});
parse_btree_node(none, TreeNodes, TreeNodeMaps) ->
    {TreeNodes, TreeNodeMaps}.

%% @doc
%% 解析子节点属性数据，
%% key会转为下划线分割小写atom，value保持不变
-spec parse_properties(map()) -> properties().
parse_properties(#{<<"properties">> := Properties}) ->
    Fun = fun(K, V, Map) -> Map#{name_convert(K) => V} end,
    maps:fold(Fun, #{}, Properties).

%% @doc 解析子节点
-spec parse_children(uninit_bt_node()) -> [bt_node_id()].
parse_children(Node) ->
    case Node of
        #{<<"child">> := Child} ->
            [Child];
        #{<<"children">> := Children} ->
            Children;
        #{} ->
            []
    end.

%% @doc
%% 模块名转换
%% 例：PlayerId -> player_id  Name -> name
-spec name_convert(unicode:chardata()) -> atom().
name_convert(Name) ->
    case name_convert_1(Name) of
        <<"_"/utf8, NewName/binary>> ->
            binary_to_atom(NewName, utf8);
        NewName ->
            binary_to_atom(NewName, utf8)
    end.

name_convert_1(<<C/utf8, Other/binary>>) when $A =< C, C =< $Z ->
    Other1 = name_convert_1(Other),
    C1 = C + 32,
    <<"_"/utf8, C1/utf8, Other1/binary>>;
name_convert_1(<<C/utf8, Other/binary>>) ->
    Other1 = name_convert_1(Other),
    <<C/utf8, Other1/binary>>;
name_convert_1(<<>>) ->
    <<>>.

%% @doc 执行行为树初始化
-spec do_init_btree(bt_node_id(), #{bt_node_id()=> btree()}, tree_nodes()) -> {ok, bt_uid()}.
do_init_btree(RootId, TreeMaps, TreeNodeMaps) ->
    #{root_id := Root} = behavior_tree:get_btree(RootId, TreeMaps),
    BTreeNode = behavior_tree:get_btree_node(Root, TreeNodeMaps),
    RootRef = base_node:do_init(maps:get(id, BTreeNode), TreeMaps, TreeNodeMaps),
    {ok, RootRef}.

%% @doc 执行指定节点的行为树
-spec do_execute(bt_uid(), bt_state()) -> {bt_status(), bt_state()}.
do_execute(TreeNodeId, State) ->
    base_node:do_execute(TreeNodeId, State).

%% @doc 关闭指定节点下的所有子节点,倒序依次关闭子节点，最后关闭根节点
-spec do_close_btree_node(bt_uid(), bt_state()) -> bt_state().
do_close_btree_node(TreeNodeId, State) ->
    #{children := Children, is_open := IsOpen} = BTreeNode = blackboard:get_btree_node(TreeNodeId),
    State1 = do_close_btree_node_1(Children, State),
    case IsOpen of
        true ->
            base_node:do_close(BTreeNode, State1);
        false ->
            State1
    end.

do_close_btree_node_1([Id | T], State) ->
    State1 = do_close_btree_node(Id, State),
    do_close_btree_node_1(T, State1);
do_close_btree_node_1([], State) ->
    State.

%% @doc 卸载指定节点下的所有子节点,倒序依次卸载子节点，最后卸载根节点
-spec do_unload_btree_node(bt_uid()) -> ok.
do_unload_btree_node(TreeNodeId) ->
    #{children := Children} = blackboard:get_btree_node(TreeNodeId),
    do_unload_btree_node_1(Children),
    blackboard:erase_btree_node(TreeNodeId).

do_unload_btree_node_1([Id | T]) ->
    do_unload_btree_node(Id),
    do_unload_btree_node_1(T);
do_unload_btree_node_1([]) ->
    ok.
