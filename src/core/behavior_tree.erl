-module(behavior_tree).

-compile([inline]).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").
%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([load_tree_file/1, execute/2, execute_child/3, unload_tree_mod/1]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
%% @doc 
%% 载入行为树文件
-spec load_tree_file(JSONConfig :: file:name_all()) -> {ok, TreeMod :: module()}|{error, Reason :: term()}.
load_tree_file(JSONConfig) ->
    do_load_tree_file(JSONConfig).

%% @doc 
%% 执行行为树节点
-spec execute(BB :: blackboard(), State :: term()) -> {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
execute(BB, State) ->
    case base_node:execute(BB, State) of
        {?BT_RUNNING, BB1, State1} ->
            {?BT_RUNNING, BB1, State1};
        {BTStatus, BB1, State1} ->
            State2 = blackboard:erase_tree(State1),
            {BTStatus, BB1, State2}
    end.

%% @doc 
%% 动态执行子节点
-spec execute_child(NodeID :: node_id(), BB :: blackboard(), State :: term()) ->
    {BTStatus :: bt_status(), UpBB :: blackboard(), UpState :: term()}.
execute_child(NodeID, BB, State) ->
    base_node:execute_child(NodeID, BB, State).

%% @doc
%% 卸载已载入的行为树模块
-spec unload_tree_mod(TreeMod :: module()) -> boolean().
unload_tree_mod(TreeMod) ->
    code:delete(TreeMod).
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
do_load_tree_file(JSONConfig) ->
    case file:read_file(JSONConfig) of
        {ok, Content} ->
            JSONTerm = jsx:decode(Content, [return_maps]),
            {TreeNodes, Titles} = parse_trees(JSONTerm),
            {module, TreeMod} = load_beam_code(JSONConfig, TreeNodes, Titles),
            {ok, TreeMod};
        {error, Reason} ->
            ?BT_ERROR_LOG("Error:~w Reason:~w JsonConfig:~ts", [error, Reason, JSONConfig]),
            {error, Reason}
    end.

merge_trees([#{<<"id">> := ID, <<"root">> := Root, <<"nodes">> := Nodes} | T]) ->
    M = maps:merge(Nodes, merge_trees(T)),
    M#{ID => Root};
merge_trees([]) ->
    #{}.

%% 解析行为树
parse_trees(Trees) ->
    Nodes = merge_trees(Trees),
    parse_trees_1(Trees, 1, Nodes, #{}, []).

parse_trees_1([#{<<"title">> := Title, <<"root">> := Root} | T], UniqueID, Nodes, TreeNodes, Titles) ->
    case TreeNodes of
        #{Root := #tree_node{id = ID}} ->
            parse_trees_1(T, UniqueID, Nodes, TreeNodes, [{Title, ID} | Titles]);
        #{} ->
            {UpUniqueID, UpTreeNodes} = parse_node(Root, UniqueID, Nodes, TreeNodes),
            parse_trees_1(T, UpUniqueID, Nodes, UpTreeNodes, [{Title, (maps:get(Root, UpTreeNodes))#tree_node.id} | Titles])
    end;
parse_trees_1([], _UniqueID, _Nodes, TreeNodes, Titles) ->
    {lists:sort([TreeNode || #tree_node{} = TreeNode <- maps:values(TreeNodes)]), Titles}.

%% 解析树节点
parse_node(ID, UniqueID, Nodes, TreeNodes) ->
    case Nodes of
        #{ID := #{<<"name">> := ChildID, <<"category">> := <<"tree">>}} ->
            ChildRootID = maps:get(ChildID, Nodes),
            parse_node(ChildRootID, UniqueID, Nodes, TreeNodes#{ID => ChildRootID});
        #{ID := #{<<"name">> := Name, <<"properties">> := Properties} = Node} ->
            NewID = UniqueID + 1,
            Children = maps:get(<<"children">>, Node, []),
            {UpUniqueID, UpTreeNodes} = parse_children(Children, NewID, Nodes, TreeNodes),
            TreeNode = #tree_node{
                id = NewID,
                name = binary_to_atom(Name, utf8),
                properties = parse_properties(Properties),
                children = get_children_ids(Children, UpTreeNodes)
            },
            false = maps:is_key(ID, UpTreeNodes),
            {UpUniqueID, UpTreeNodes#{ID => TreeNode}}
    end.

%% 解析子节点
parse_children([ID | T], UniqueID, Nodes, TreeNodes) ->
    {UpUniqueID, UpTreeNodes} = parse_node(ID, UniqueID, Nodes, TreeNodes),
    parse_children(T, UpUniqueID, Nodes, UpTreeNodes);
parse_children([], UniqueID, _Nodes, TreeNodes) ->
    {UniqueID, TreeNodes}.

%% 获取子节点id列表
get_children_ids([ID | T], Nodes) ->
    case Nodes of
        #{ID := #tree_node{id = TreeNodeID}} ->
            [TreeNodeID | get_children_ids(T, Nodes)];
        #{ID := ChildRootID} ->
            get_children_ids([ChildRootID], Nodes)
    end;
get_children_ids([], _Nodes) ->
    [].

%% 解析子节点属性数据
parse_properties(Properties) ->
    Fun = fun(K, V, Map) -> Map#{binary_to_atom(K, utf8) => V} end,
    maps:fold(Fun, #{}, Properties).

%% 生成模块名
gen_tree_mod(JSONConfig) ->
    RootName = filename:rootname(filename:basename(JSONConfig)),
    I = integer_to_list(erlang:unique_integer([positive, monotonic])),
    list_to_atom(unicode:characters_to_list(["$b3_", RootName, "_", I])).

%% 动态编译
load_beam_code(JSONConfig, TreeNodes, Titles) ->
    TreeMod = gen_tree_mod(JSONConfig),

    Head = io_lib:format("-module(~w).\n\n-export([get_tree_by_title/1, get_node/1]).\n\n", [TreeMod]),

    Body1 = [
        [io_lib:format("get_tree_by_title(~w) -> ~w;\n", [Title, ID]) || {Title, ID} <- Titles],
        "get_tree_by_title(_) -> erlang:throw(tree_not_exist).\n\n"
    ],

    Body2 = [
        [io_lib:format("get_node(~w) -> ~w;\n", [ID, TreeNode]) || #tree_node{id = ID} = TreeNode <- TreeNodes],
        "get_node(_) -> erlang:throw(node_not_exist).\n\n"
    ],

    Text = unicode:characters_to_list([Head, Body1, Body2]),
%%    file:write_file([erlang:atom_to_list(TreeMod), ".erl"], Text),
    Forms = scan_and_parse(Text, 1),
    {ok, TreeMod, Binary} = compile:forms(Forms, [deterministic, no_line_info]),
    {module, TreeMod} = code:load_binary(TreeMod, TreeMod, Binary).

scan_and_parse(Text, Line) ->
    case erl_scan:tokens([], Text, Line) of
        {done, {ok, Tokens, NLine}, T} ->
            {ok, Forms} = erl_parse:parse_form(Tokens),
            [Forms | scan_and_parse(T, NLine)];
        {more, _UpContinuation} ->
            []
    end.
