-module(behavior_tree).

-compile([inline, {inline_size, 100}]).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([
    load_tree_file/1,
    init_btree_by_title/4, init_btree/3,
    execute/2, execute_child/2, close_btree_node/2, unload_btree/1
]).

%% Internal API
-export([get_btree_id_by_title/2, get_btree/2, get_btree_node/2]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
%% @doc 
%% 载入行为树文件
-spec load_tree_file(file:name_all()) -> {#{string() => bt_node_id()}, #{bt_node_id() => btree()}, tree_nodes()}|{error, term()}.
load_tree_file(JsonConfig) ->
    case file:read_file(JsonConfig) of
        {ok, Json} ->
            JsonTerm = jsx:decode(Json, [return_maps]),
            parse_btree(JsonTerm);
        {error, Reason} ->
            ?BT_LOG("Error:~w Reason:~w JsonConfig:~w", [error, Reason, JsonConfig]),
            {error, Reason}
    end.

%% @doc 
%% 获取行为树根节点id
-spec get_btree_id_by_title(string(), #{string() => bt_node_id()}) -> bt_node_id()|undefined.
get_btree_id_by_title(Title, TitleMaps) ->
    case TitleMaps of
        #{Title := ID} ->
            ID;
        #{} ->
            undefined
    end.

%% @doc 
%% 获取行为树根节点
-spec get_btree(bt_node_id(), #{bt_node_id() => btree()}) -> btree()|undefined.
get_btree(BTNodeID, TreeMaps) ->
    case TreeMaps of
        #{BTNodeID := BTree} ->
            BTree;
        #{} ->
            undefined
    end.

%% @doc 
%% 获取行为树子节点
-spec get_btree_node(bt_node_id(), tree_nodes()) -> uninit_bt_node()|undefined.
get_btree_node(BTNodeID, TreeNodeMaps) ->
    case TreeNodeMaps of
        #{BTNodeID := BTNode} ->
            BTNode;
        #{} ->
            undefined
    end.

%% @doc 
%% 根据给定节点名初始化整颗行为树
-spec init_btree_by_title(string(), #{string() => bt_node_id()}, #{bt_node_id() => btree()}, tree_nodes()) -> {ok, bt_uid()}|{error, term()}.
init_btree_by_title(Title, TitleMaps, TreeMaps, TreeNodeMaps) ->
    BTNodeID = get_btree_id_by_title(Title, TitleMaps),
    init_btree(BTNodeID, TreeMaps, TreeNodeMaps).

%% @doc 
%% 根据给定节点初始化整颗行为树
-spec init_btree(bt_node_id(), #{bt_node_id() => btree()}, tree_nodes()) -> {ok, bt_uid()}|{error, term()}.
init_btree(BTNodeID, TreeMaps, TreeNodeMaps) ->
    #{root := Root} = behavior_tree:get_btree(BTNodeID, TreeMaps),
    #{id := NodeID} = behavior_tree:get_btree_node(Root, TreeNodeMaps),
    base_node:do_init(undefined, NodeID, TreeMaps, TreeNodeMaps).

%% @doc 
%% 执行行为树节点
-spec execute(bt_uid(), bt_state()) -> {bt_status(), bt_state()}.
execute(NodeID, BTState) ->
    BTState1 = blackboard:init_maps_keys(BTState),
    case base_node:execute(NodeID, BTState1) of
        {?BT_RUNNING, BTState2} ->
            {?BT_RUNNING, BTState2};
        {BTStatus, BTState2} ->
            BTState3 = blackboard:erase_btree(BTState2),
            {BTStatus, BTState3}
    end.

%% @doc 
%% 动态执行子节点
-spec execute_child(bt_uid(), bt_state()) -> {bt_status(), bt_state()}.
execute_child(NodeID, BTState) ->
    base_node:execute(NodeID, BTState).

%% @doc 
%% 强制关闭指定节点下的所有子节点
-spec close_btree_node(bt_uid(), bt_state()) -> bt_state().
close_btree_node(NodeID, BTState) ->
    BTState1 = do_close_btree_node(NodeID, BTState),
    blackboard:erase_btree(BTState1).

%% @doc 
%% 卸载指定节点下的所有子节点
-spec unload_btree(bt_uid()) -> ok.
unload_btree(NodeID) ->
    do_unload_btree_node(NodeID).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% 解析行为树
-spec parse_btree(map()) -> {#{string() => bt_node_id()}, #{bt_node_id() => btree()}, tree_nodes()}.
parse_btree(#{<<"trees">> := Trees}) ->
    parse_btree_1(Trees, #{}, #{}, #{}).

parse_btree_1([Node | T], TitleMaps, TreeMaps, TreeNodeMaps) ->
    #{<<"id">> := ID, <<"title">> := Title, <<"root">> := Root, <<"nodes">> := Nodes} = Node,
    Iterator = maps:iterator(Nodes),
    {TreeNodes, TreeNodeMaps1} = parse_btree_node(maps:next(Iterator), #{}, TreeNodeMaps),
    BTree = #{
        id => ID,
        root => Root,
        properties => parse_properties(Node),
        tree_nodes => TreeNodes
    },
    parse_btree_1(T, TitleMaps#{Title => ID}, TreeMaps#{ID => BTree}, TreeNodeMaps1);
parse_btree_1([], TitleMaps, TreeMaps, TreeNodeMaps) ->
    {TitleMaps, TreeMaps, TreeNodeMaps}.

%% 解析树节点
-spec parse_btree_node({bt_node_id(), map(), maps:iterator()}|none, tree_nodes(), tree_nodes()) -> {tree_nodes(), tree_nodes()}.
parse_btree_node({ID, Node, NextIterator}, TreeNodes, TreeNodeMaps) ->
    #{<<"name">> := Name, <<"category">> := Category} = Node,
    case Category of
        <<"tree">> ->
            Name1 = Name;
        _ ->
            Name1 = name_convert(Name)
    end,
    TreeNode = #{
        id => ID,
        name => Name1,
        category => binary_to_atom(Category, utf8),
        properties => parse_properties(Node),
        children => parse_children(Node)
    },
    parse_btree_node(maps:next(NextIterator), TreeNodes#{ID => TreeNode}, TreeNodeMaps#{ID => TreeNode});
parse_btree_node(none, TreeNodes, TreeNodeMaps) ->
    {TreeNodes, TreeNodeMaps}.

%% 解析子节点属性数据，
%% key会转为下划线分割小写atom，value保持不变
-spec parse_properties(map()) -> properties().
parse_properties(#{<<"properties">> := Properties} = _Node) ->
    Fun = fun(K, V, Map) -> Map#{name_convert(K) => V} end,
    maps:fold(Fun, #{}, Properties).

%% 解析子节点
-spec parse_children(map()) -> [bt_node_id()].
parse_children(#{<<"child">> := Child} = _Node) ->
    [Child];
parse_children(#{<<"children">> := Children} = _Node) ->
    Children;
parse_children(_Node) ->
    [].

%% 模块名转换
%% 例：PlayerID -> player_id  Name -> name
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
    <<"_"/utf8, (C + 32)/utf8, Other1/binary>>;
name_convert_1(<<C/utf8, Other/binary>>) ->
    Other1 = name_convert_1(Other),
    <<C/utf8, Other1/binary>>;
name_convert_1(<<>>) ->
    <<>>.

%% 执行关闭行为树节点
-spec do_close_btree_node(bt_uid(), bt_state()) -> bt_state().
do_close_btree_node(NodeID, BTState) ->
    BTNode = blackboard:get_btree_node(NodeID),
    base_node:do_close(BTNode, BTState).

%% 执行卸载行为树节点
-spec do_unload_btree_node(bt_uid()) -> ok.
do_unload_btree_node(NodeID) ->
    #{children := Children} = blackboard:get_btree_node(NodeID),
    do_unload_btree_node_1(Children),
    blackboard:erase_btree_node(NodeID),
    ok.

do_unload_btree_node_1([NodeID | T]) ->
    do_unload_btree_node(NodeID),
    do_unload_btree_node_1(T);
do_unload_btree_node_1([]) ->
    ok.
