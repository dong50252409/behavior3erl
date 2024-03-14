-module(blackboard).

-compile([inline]).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([init_blackboard/2]).
-export([set/4, set_global/3, get/3, get_global/2, get/4, get_global/3, remove/3, remove_global/2]).
-export([get_tree_mod/1, get_root_node_id/1, erase_node/2, erase_tree_nodes/1, get_global_maps/1, set_io/2, get_io/1]).
%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

%% @doc
%% 初始化黑板
-spec init_blackboard(TreeMod :: module(), Title :: string()) -> BB :: blackboard().
init_blackboard(TreeMod, Title) ->
    Title1 = unicode:characters_to_binary(Title),
    RootID = TreeMod:get_root_id(Title1),
    #blackboard{
        tree_mod = TreeMod, title = Title1, root_id = RootID,
        global_maps = #{}, io = erlang:group_leader()
    }.

%% @doc
%% 设置节点变量
-spec set(Key :: term(), Value :: term(), NodeID :: node_id(), BB :: blackboard()) -> UpBB :: blackboard().
set(Key, Value, NodeID, #blackboard{global_maps = GlobalMaps} = BB) ->
    case GlobalMaps of
        #{NodeID := #{Key := _} = NodeMaps} ->
            BB#blackboard{global_maps = GlobalMaps#{NodeID := NodeMaps#{Key := Value}}};
        #{NodeID := NodeMaps} ->
            BB#blackboard{global_maps = GlobalMaps#{NodeID := NodeMaps#{Key => Value}}};
        #{} ->
            BB#blackboard{global_maps = GlobalMaps#{NodeID => #{Key => Value}}}
    end.

%% @doc
%% 设置全局变量
-spec set_global(Key :: term(), Value :: term(), BB :: blackboard()) -> UpBB :: blackboard().
set_global(Key, Value, BB) ->
    set(Key, Value, 0, BB).

%% @doc
%% 获取节点变量
-spec get(Key :: term(), NodeID :: node_id(), BB :: blackboard()) -> Value :: term()|undefined.
get(Key, NodeID, #blackboard{global_maps = GlobalMaps}) ->
    case GlobalMaps of
        #{NodeID := #{Key := Value}} ->
            Value;
        #{} ->
            undefined
    end.

%% @doc
%% 获取全局变量
-spec get_global(Key :: term(), BB :: blackboard()) -> Value :: term()|undefined.
get_global(Key, BB) ->
    get(Key, 0, BB).

%% @doc
%% 获取节点变量，不存在则返回Default
-spec get(Key :: term(), NodeID :: node_id(), Default :: term(), BB :: blackboard()) -> Value :: term().
get(Key, NodeID, Default, BB) ->
    case get(Key, NodeID, BB) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% @doc
%% 获取全局变量，不存在则返回Default
-spec get_global(Key :: term(), Default :: term(), BB :: blackboard()) -> Value :: term()|undefined.
get_global(Key, Default, BB) ->
    get(Key, 0, Default, BB).

%% @doc
%% 删除节点变量
-spec remove(Key :: term(), NodeID :: node_id(), BB :: blackboard()) -> UpBB :: blackboard().
remove(Key, NodeID, #blackboard{global_maps = GlobalMaps} = BB) ->
    case GlobalMaps of
        #{NodeID := NodeMaps} ->
            BB#blackboard{global_maps = GlobalMaps#{NodeID := maps:remove(Key, NodeMaps)}};
        #{} ->
            BB
    end.

%% @doc
%% 删除节点变量
-spec remove_global(Key :: term(), BB :: blackboard()) -> UpBB :: blackboard().
remove_global(Key, BB) ->
    remove(Key, 0, BB).

%% @doc
%% 获取当前运行中行为树模块名
-spec get_tree_mod(BB :: blackboard()) -> TreeMod :: module().
get_tree_mod(#blackboard{tree_mod = TreeMod}) ->
    TreeMod.

%% @doc
%% 获取根节点id
-spec get_root_node_id(BB :: blackboard()) -> RootID :: node_id().
get_root_node_id(#blackboard{root_id = RootID}) ->
    RootID.

%% @doc
%% 擦除节点所有信息
-spec erase_node(NodeID :: node_id(), BB :: blackboard()) -> UpBB :: blackboard().
erase_node(NodeID, #blackboard{global_maps = GlobalMaps} = BB) ->
    BB#blackboard{global_maps = maps:remove(NodeID, GlobalMaps)}.

%% @doc
%% 擦除行为树所有节点信息
-spec erase_tree_nodes(BB :: blackboard()) -> UpBB :: blackboard().
erase_tree_nodes(BB) ->
    BB#blackboard{global_maps = #{}}.

%% @doc
%% 获取行为树所有节点信息
get_global_maps(#blackboard{global_maps = GlobalMaps}) ->
    GlobalMaps.

%% @doc
%% 设置IO
%% 可用于重定向调试日志输出位置，默认erlang:group_leader()
-spec set_io(IO :: io:device(), BB :: blackboard()) -> UpBB :: blackboard().
set_io(IO, BB) ->
    BB#blackboard{io = IO}.

%% @doc
%% 获取IO
-spec get_io(BB :: blackboard()) -> IO :: io:device().
get_io(#blackboard{io = IO}) ->
    IO.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------