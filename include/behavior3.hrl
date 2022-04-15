-ifndef(behavior3_h).
-define(behavior3_h, true).

-export_type([bt_uid/0, bt_status/0, bt_node_id/0, bt_state/0, properties/0, uninit_bt_node/0, tree_nodes/0, btree/0, bt_node/0]).

-type bt_uid() :: reference().

-define(BT_SUCCESS, 1).
-define(BT_FAILURE, 2).
-define(BT_RUNNING, 3).
-define(BT_ERROR, 4).

-type bt_status() :: ?BT_SUCCESS|?BT_FAILURE|?BT_RUNNING|?BT_ERROR.

-type bt_node_id() :: string().

-type bt_state() :: #{
    '$global_maps' => map(),
    '$local_maps' => map(),
    term() => term()
}.

-type properties() :: #{atom() => term()}.

%% 未初始化的树节点
-type uninit_bt_node() :: #{
    id => bt_node_id(),
    name => atom() | bt_node_id(),
    category => atom(),
    properties => properties(),
    children => [bt_node_id()]
}.

-type tree_nodes() :: #{bt_node_id() => uninit_bt_node()}.

%% 行为树
-type btree() :: #{
    id => bt_node_id(),
    root => bt_node_id(),
    properties => properties(),
    tree_nodes => tree_nodes()
}.

%% 行为树节点
-type bt_node() :: #{
    id := bt_uid(),
    bt_node_id := bt_node_id(),
    parent_id := bt_uid(),
    name := atom(),
    category := atom(),
    properties := properties(),
    children := [bt_uid()]
}.

-define(SKIP_MOD, [
    mem_priority, mem_sequence, priority, sequence,
    repeat_until_failure, repeat_until_success, repeater
]).

-ifdef(BT_DEBUG).
-define(BT_DEBUG_LOG(Format, Args), io:format(erlang:get(?BT_DEBUG), unicode:characters_to_list(["~w:~w:~w:~w ~w [debug] ~p: ", Format, "~n"]), tuple_to_list(time()) ++ [erlang:system_time(millisecond) rem 1000, self(), ?MODULE | Args])).
-define(BT_DEBUG_LOG(Format), ?BT_DEBUG_LOG(Format, [])).
-else.
-define(BT_DEBUG_LOG(Format, Args), ok).
-define(BT_DEBUG_LOG(Format), ok).
-endif.

-define(BT_ERROR_LOG(Format, Args), io:format(unicode:characters_to_list(["~w:~w:~w ~w [error] ~p: ", Format, "~n"]), tuple_to_list(time()) ++ [self(), ?MODULE | Args])).
-define(BT_ERROR_LOG(Format), ?BT_ERROR(Format, [])).

-endif.
