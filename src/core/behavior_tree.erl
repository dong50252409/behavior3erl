-module(behavior_tree).

-behavior(gen_server).
%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([load_tree_file/1, execute/2, execute_child/2]).

-export([start_link/0]).

%% Internal API
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {unique = 0, tree_mods = #{}, loaded_tree_mods = #{}}).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 
%% 载入行为树文件
-spec load_tree_file(JSONConfig :: file:name_all()) -> {ok|reload, TreeMod :: module()}|{error, Reason :: term()}.
load_tree_file(JSONConfig) ->
    gen_server:call(?MODULE, {load_tree_file, JSONConfig}, infinity).

%% @doc 
%% 执行行为树节点
%% TODO 通过Title获取Mod并执行，热更新问题
-spec execute(Title :: string(), BTState :: bt_state()) -> {bt_status(), bt_state()}|{error, tree_not_found}.
execute(Title, BTState) when is_list(Title) ->
    execute(unicode:characters_to_binary(Title), BTState);
execute(Title, BTState) when is_binary(Title) ->
    case gen_server:call(?MODULE, {get_tree_mod, Title}) of
        {ok, TreeMod} ->
            case base_node:execute(TreeMod, Title, BTState) of
                {?BT_RUNNING, BTState2} ->
                    {?BT_RUNNING, BTState2};
                {BTStatus, BTState2} ->
                    BTState3 = blackboard:erase_tree(BTState2),
                    {BTStatus, BTState3}
            end;
        Err ->
            Err
    end.

%% @doc 
%% 动态执行子节点
-spec execute_child(NodeID :: node_id(), BTState :: bt_state()) -> {bt_status(), bt_state()}.
execute_child(NodeID, BTState) ->
    base_node:do_execute(NodeID, BTState).

init(_Args) ->
    {ok, #state{}}.

handle_call({load_tree_file, JSONConfig}, _From, State) ->
    {Reply, UpState} = do_load_tree_file(JSONConfig, State),
    {reply, Reply, UpState};

handle_call({get_tree_mod, Title}, _From, #state{tree_mods = TreeMods} = State) ->
    case TreeMods of
        #{Title := TreeMod} ->
            {reply, {ok, TreeMod}, State};
        #{} ->
            {reply, {error, tree_not_found}, State}
    end.


handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
to_list(Term) when erlang:is_list(Term) ->
    Term;
to_list(Term) when erlang:is_binary(Term) ->
    erlang:binary_to_list(Term);
to_list(Term) when erlang:is_integer(Term) ->
    erlang:integer_to_list(Term).

str_to_atom(Str) when erlang:is_list(Str) ->
    erlang:list_to_atom(Str);
str_to_atom(Str) when erlang:is_binary(Str) ->
    erlang:binary_to_atom(Str, utf8).

gen_tree_mod(JSONConfig, #state{unique = Unique, loaded_tree_mods = LoadedTreeMods} = State) ->
    RootName = to_list(filename:rootname(filename:basename(JSONConfig))),
    UpUnique = Unique + 1,
    StrTreeMod = RootName ++ to_list(UpUnique),
    TreeMod = str_to_atom(StrTreeMod),
    case LoadedTreeMods of
        #{RootName := L} ->
            {TreeMod, State#state{unique = UpUnique, loaded_tree_mods = LoadedTreeMods#{RootName := [TreeMod | L]}}};
        #{} ->
            {TreeMod, State#state{unique = UpUnique, loaded_tree_mods = LoadedTreeMods#{RootName => [TreeMod]}}}
    end.


do_load_tree_file(JSONConfig, State) ->
    {TreeMod, UpState} = gen_tree_mod(JSONConfig, State),
    case file:read_file(JSONConfig) of
        {ok, Content} ->
            JSONTerm = jsx:decode(Content, [return_maps]),
            {UpUnique, Titles} = parse_trees(TreeMod, JSONTerm, UpState#state.unique),
            erlang:garbage_collect(self(), [{async, load_tree_file_gc_finished}]),
            FinalState = UpState#state{
                unique = UpUnique,
                tree_mods = maps:merge(UpState#state.tree_mods, maps:from_list([{Title, TreeMod} || Title <- Titles]))
            },
            {{ok, TreeMod}, FinalState};
        {error, Reason} ->
            ?BT_ERROR_LOG("Error:~w Reason:~w JsonConfig:~ts", [error, Reason, JSONConfig]),
            {{error, Reason}, State}
    end.

merge_trees([#{<<"id">> := ID, <<"root">> := Root, <<"nodes">> := Nodes} | T]) ->
    M = maps:merge(Nodes, merge_trees(T)),
    M#{ID => Root};
merge_trees([]) ->
    #{}.

%% 解析行为树
parse_trees(TreeMod, #{<<"trees">> := Trees}, UniqueID) ->
    Nodes = merge_trees(Trees),
    {UpUniqueID, TreeNodes, Titles} = parse_trees_1(Trees, UniqueID, Nodes, #{}, []),
    load_beam_code(TreeMod, TreeNodes, Titles),
    {UpUniqueID, Titles}.

parse_trees_1([#{<<"title">> := Title, <<"root">> := Root} | T], UniqueID, Nodes, TreeNodes, Titles) ->
    case TreeNodes of
        #{Root := #tree_node{id = ID}} ->
            parse_trees_1(T, UniqueID, Nodes, TreeNodes, [{Title, ID} | Titles]);
        #{} ->
            {UpUniqueID, UpTreeNodes} = parse_node(Root, UniqueID, Nodes, TreeNodes),
            parse_trees_1(T, UpUniqueID, Nodes, UpTreeNodes, [{Title, (maps:get(Root, UpTreeNodes))#tree_node.id} | Titles])
    end;
parse_trees_1([], UniqueID, _Nodes, TreeNodes, Titles) ->
    {UniqueID, lists:sort([TreeNode || #tree_node{} = TreeNode <- maps:values(TreeNodes)]), Titles}.

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
                name = str_to_atom(Name),
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
    Fun = fun(K, V, Map) -> Map#{str_to_atom(K) => V} end,
    maps:fold(Fun, #{}, Properties).

%% 动态编译
load_beam_code(TreeMod, TreeNodes, Titles) ->
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
