-module(blackboard).


%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([get_btree_node/1, set_btree_node/1, erase_btree_node/1, set/3, set/4, get/2, get/3, get/4, remove/2, remove/3]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

%% 获取树节点
-spec get_btree_node(bt_uid()) -> bt_node()|undefined.
get_btree_node(NodeRef) ->
    get({btree_node, NodeRef}).

%% 设置树节点
-spec set_btree_node(bt_node()) -> ok.
set_btree_node(#{id := NodeRef} = BTreeNode) ->
    put({btree_node, NodeRef}, BTreeNode),
    ok.

%% 移除树节点
-spec erase_btree_node(bt_uid()) -> bt_node().
erase_btree_node(NodeRef) ->
    erase({btree_node, NodeRef}).

%% 设置全局变量
-spec set(term(), term(), bt_state()) -> bt_state().
set(Key, Value, State) ->
    case State of
        #{'$global' := ValueMaps} ->
            State#{'$global' := ValueMaps#{Key => Value}};
        #{} ->
            State#{'$global' => #{Key => Value}}
    end.

%% 设置节点变量
-spec set(term(), term(), bt_uid(), bt_state()) -> bt_state().
set(Key, Value, NodeId, State) ->
    case State of
        #{NodeId := ValueMaps} ->
            State#{NodeId := ValueMaps#{Key => Value}};
        #{} ->
            State#{NodeId => #{Key => Value}}
    end.

%% 获取全局变量
-spec get(term(), bt_state()) -> undefined | term().
get(Key, State) ->
    case State of
        #{'$global' := #{Key := Value}} ->
            Value;
        #{} ->
            undefined
    end.

%% 获取节点变量
-spec get(term(), bt_uid(), bt_state()) -> undefined | term().
get(Key, NodeId, State) ->
    case State of
        #{NodeId := #{Key := Value}} ->
            Value;
        #{} ->
            undefined
    end.

%% 获取节点变量
-spec get(term(), bt_uid(), term(), bt_state()) -> undefined | term().
get(Key, NodeId, Default, State) ->
    case State of
        #{NodeId := #{Key := Value}} ->
            Value;
        #{} ->
            Default
    end.

%% 清除行为树节点变量
-spec remove(bt_uid(), bt_state()) -> bt_state().
remove(NodeId, State) ->
    maps:remove(NodeId, State).

%% 清除节点执行key变量
-spec remove(term(), bt_uid(), bt_state()) -> bt_state().
remove(Key, NodeId, State) ->
    case State of
        #{NodeId := ValueMap} ->
            State#{NodeId := maps:remove(Key, ValueMap)};
        #{} ->
            State
    end.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
