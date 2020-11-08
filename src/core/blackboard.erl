-module(blackboard).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([
    get_btree_node/1, set_btree_node/1, erase_btree_node/1,
    set_global/3, set/4,
    get_global/2, get_global/3, get/3, get/4,
    remove/2, remove/3
]).

%% Internal API
-export([init_maps_keys/1, erase_node_local/2, erase_btree/1]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
%% @doc
%% 获取树节点
-spec get_btree_node(bt_uid()) -> bt_node()|undefined.
get_btree_node(NodeID) ->
    get({btree_node, NodeID}).

%% @doc
%% 设置树节点
-spec set_btree_node(bt_node()) -> ok.
set_btree_node(#{id := NodeID} = BTNode) ->
    put({btree_node, NodeID}, BTNode),
    ok.

%% @doc
%% 移除树节点
-spec erase_btree_node(bt_uid()) -> bt_node().
erase_btree_node(NodeID) ->
    erase({btree_node, NodeID}).

%% @doc
%% 设置全局变量
-spec set_global(term(), term(), bt_state()) -> bt_state().
set_global(Key, Value, #{'$global_maps' := GlobalMaps} = BTState) ->
    BTState#{'$global_maps' := GlobalMaps#{Key => Value}}.

%% @doc
%% 设置节点变量
-spec set(term(), term(), bt_uid(), bt_state()) -> bt_state().
set(Key, Value, NodeID, #{'$local_maps' := LocalMaps} = BTState) ->
    case LocalMaps of
        #{NodeID := ValueMaps} ->
            LocalMaps1 = LocalMaps#{NodeID := ValueMaps#{Key => Value}};
        #{} ->
            LocalMaps1 = LocalMaps#{NodeID => #{Key => Value}}
    end,
    BTState#{'$local_maps' := LocalMaps1}.

%% @doc
%% 获取全局变量
-spec get_global(term(), bt_state()) -> term()|undefined.
get_global(Key, BTState) ->
    case BTState of
        #{'$global_maps' := #{Key := Value}} ->
            Value;
        #{} ->
            undefined
    end.

%% @doc
%% 获取全局变量，不存在则返回Default
-spec get_global(term(), term(), bt_state()) -> term().
get_global(Key, Default, BTState) ->
    case BTState of
        #{'$global_maps' := #{Key := Value}} ->
            Value;
        #{} ->
            Default
    end.

%% @doc
%% 获取节点变量
-spec get(term(), bt_uid(), bt_state()) -> term()|undefined.
get(Key, NodeID, BTState) ->
    case BTState of
        #{'$local_maps' := #{NodeID := #{Key := Value}}} ->
            Value;
        #{} ->
            undefined
    end.

%% @doc
%% 获取节点变量，不存在则返回Default
-spec get(term(), bt_uid(), term(), bt_state()) -> term().
get(Key, NodeID, Default, BTState) ->
    case BTState of
        #{'$local_maps' := #{NodeID := #{Key := Value}}} ->
            Value;
        #{} ->
            Default
    end.

%% @doc
%% 删除全局变量
-spec remove(term(), bt_state()) -> bt_state().
remove(Key, #{'$global_maps' := GlobalMaps} = BTState) ->
    BTState#{'$global_maps' := maps:remove(Key, GlobalMaps)}.

%% @doc
%% 删除节点变量
-spec remove(term(), bt_uid(), bt_state()) -> bt_state().
remove(Key, NodeID, #{'$local_maps' := LocalMaps} = BTState) ->
    case LocalMaps of
        #{NodeID := ValueMap} ->
            LocalMaps1 = LocalMaps#{NodeID := maps:remove(Key, ValueMap)},
            BTState#{'$local_maps' := LocalMaps1};
        #{} ->
            BTState
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% 初始化行为树相关结构
-spec init_maps_keys(bt_state()) -> bt_state().
init_maps_keys(BTState) ->
    BTState1 = init_global(BTState),
    init_local(BTState1).

init_global(BTState) ->
    case maps:is_key('$global_maps', BTState) of
        true ->
            BTState;
        false ->
            BTState#{'$global_maps' => #{}}
    end.

init_local(BTState) ->
    case maps:is_key('$local_maps', BTState) of
        true ->
            BTState;
        false ->
            BTState#{'$local_maps' => #{}}
    end.

%% 移除节点局部变量结构
-spec erase_node_local(bt_uid(), bt_state()) -> bt_state().
erase_node_local(NodeID, #{'$local_maps' := LocalMaps} = BTState) ->
    BTState#{'$local_maps' := maps:remove(NodeID, LocalMaps)}.

%% 移除行为树相关结构
-spec erase_btree(bt_state()) -> bt_state().
erase_btree(BTState) ->
    BTState1 = maps:remove('$global_maps', BTState),
    maps:remove('$local_maps', BTState1).
