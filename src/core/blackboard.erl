-module(blackboard).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([set/4, get/3, get/4, remove/3]).

% Internal API
-export([ensure_blackboard/1, set_running_info/3, get_tree_mod/1, get_tree_title/1, erase_node/2, erase_tree/1, erase_all_tree/1, get_tree_maps/1, get_log_file/1]).

%%--------------------------------------------------------------------
%% defined
%%--------------------------------------------------------------------
-define(B3_MAPS, '$b3_maps').
-define(B3_RUNNING_TITLE, '$b3_running_title').
-define(B3_RUNNING_MOD, '$b3_running_mod').
-define(B3_LOG_FILE(Title), {'$b3_log_file', Title}).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
%% @doc
%% 设置节点变量
-spec set(Key :: term(), Value :: term(), NodeID :: node_id(), BTState :: bt_state()) -> bt_state().
set(Key, Value, NodeID, #{?B3_MAPS := B3Maps, ?B3_RUNNING_TITLE := Title} = BTState) ->
    case B3Maps of
        #{Title := #{NodeID := #{Key := _} = NodeMaps} = TitleMaps} ->
            BTState#{?B3_MAPS := B3Maps#{Title := TitleMaps#{NodeID := NodeMaps#{Key := Value}}}};
        #{Title := #{NodeID := NodeMaps} = TitleMaps} ->
            BTState#{?B3_MAPS := B3Maps#{Title := TitleMaps#{NodeID := NodeMaps#{Key => Value}}}};
        #{Title := TitleMaps} ->
            BTState#{?B3_MAPS := B3Maps#{Title := TitleMaps#{NodeID => #{Key => Value}}}};
        #{} ->
            BTState#{?B3_MAPS => B3Maps#{Title => #{NodeID => #{Key => Value}}}}
    end.

%% @doc
%% 获取节点变量
-spec get(Key :: term(), NodeID :: node_id(), _BTState :: bt_state()) -> Value :: term()|undefined.
get(Key, NodeID, #{?B3_MAPS := B3Maps, ?B3_RUNNING_TITLE := Title} = _BTState) ->
    case B3Maps of
        #{Title := #{NodeID := #{Key := Value}}} ->
            Value;
        #{} ->
            undefined
    end.

%% @doc
%% 获取节点变量，不存在则返回Default
-spec get(Key :: term(), NodeID :: node_id(), Default :: term(), BTState :: bt_state()) ->
    Value :: term().
get(Key, NodeID, Default, BTState) ->
    case get(Key, NodeID, BTState) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% @doc
%% 删除节点变量
-spec remove(Key :: term(), NodeID :: node_id(), BTState :: bt_state()) -> bt_state().
remove(Key, NodeID, #{?B3_MAPS := B3Maps, ?B3_RUNNING_TITLE := Title} = BTState) ->
    case B3Maps of
        #{Title := #{NodeID := NodeMaps} = TitleMaps} ->
            BTState#{?B3_MAPS := B3Maps#{Title := TitleMaps#{NodeID := maps:remove(Key, NodeMaps)}}};
        #{} ->
            BTState
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @doc
%% 确保blackboard初始化
-spec ensure_blackboard(BTState :: bt_state()) -> bt_state().
ensure_blackboard(BTState) ->
    case BTState of
        #{?B3_MAPS := _} ->
            BTState;
        #{} ->
            BTState#{?B3_MAPS => #{}}
    end.

%% @doc
%% 设置行为树运行信息
-spec set_running_info(TreeMod :: module(), Title :: binary(), BTState :: bt_state()) -> bt_state().
set_running_info(TreeMod, Title, BTState) ->
    BTState#{?B3_RUNNING_MOD => TreeMod, ?B3_RUNNING_TITLE => Title}.

%% @doc
%% 获取当前运行中行为树模块名
-spec get_tree_mod(BTState :: bt_status()) -> module().
get_tree_mod(#{?B3_RUNNING_MOD := TreeMod} = _BTState) ->
    TreeMod.

%% @doc
%% 获取当前运行中行为树的名称
-spec get_tree_title(BTState :: bt_state()) -> binary().
get_tree_title(#{?B3_RUNNING_TITLE := Title} = _BTState) ->
    Title.

%% @doc
%% 擦除节点信息
-spec erase_node(NodeID :: node_id(), BTState :: bt_state()) -> bt_state().
erase_node(NodeID, #{?B3_MAPS := B3Maps, ?B3_RUNNING_TITLE := Title} = BTState) ->
    case B3Maps of
        #{Title := TitleMaps} ->
            BTState#{?B3_MAPS := B3Maps#{Title := maps:remove(NodeID, TitleMaps)}};
        #{} ->
            BTState
    end.

%% @doc
%% 擦除行为树所有节点信息
-spec erase_tree(BTState :: bt_state()) -> bt_state().
erase_tree(#{?B3_MAPS := B3Maps, ?B3_RUNNING_TITLE := Title} = BTState) ->
    BTState#{?B3_MAPS := maps:remove(Title, B3Maps)}.

%% @doc
%% 擦除所有行为树所有节点信息
erase_all_tree(BTState) ->
    maps:remove(?B3_MAPS, BTState).

%% @doc
%% 获取行为树所有节点信息
get_tree_maps(#{?B3_MAPS := B3Maps, ?B3_RUNNING_TITLE := Title} = _BTState) ->
    case B3Maps of
        #{Title := TitleMaps} ->
            TitleMaps;
        #{} ->
            #{}
    end.

%% @doc
%% 获取调试文件pid
get_log_file(#{?B3_MAPS := B3Maps, ?B3_RUNNING_TITLE := Title} = BTState) ->
    case B3Maps of
        #{?B3_LOG_FILE(Title) := IO} ->
            {IO, BTState};
        #{} ->
            UpBtState = BTState#{?B3_MAPS := B3Maps#{?B3_LOG_FILE(Title) => create_log_file(Title)}},
            get_log_file(UpBtState)
    end.

create_log_file(Title) ->
    Filename = erlang:iolist_to_binary([Title, "_", lists:droplast(erlang:tl(erlang:pid_to_list(self()))), "_bt.log"]),
    {ok, IO} = file:open(Filename, [append, {encoding, utf8}]),
    IO.