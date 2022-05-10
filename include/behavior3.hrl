-ifndef(behavior3_h).
-define(behavior3_h, true).

-define(BT_SUCCESS, 1).
-define(BT_FAILURE, 2).
-define(BT_RUNNING, 3).
-define(BT_ERROR, 4).

-type node_id() :: integer().

-record(tree_node, {
    id :: node_id(),
    name :: module(),
    properties :: properties(),
    children :: [node_id()]
}).

-type tree_node() :: #tree_node{}.

-type properties() :: #{atom() => term()}.

-record(blackboard, {
    tree_mod :: module(),
    title :: binary(),
    root_id :: node_id(),
    global_maps :: map(),
    io :: pid()|undefined
}).

-type blackboard() :: #blackboard{}.

-type bt_status() :: ?BT_SUCCESS|?BT_FAILURE|?BT_RUNNING|?BT_ERROR.

-define(BT_DEBUG_LOG(IO, Format, Args), io:format(IO, unicode:characters_to_list(["~w:~w:~w:~w ~w [debug] ~p: ", Format, "~n"]), tuple_to_list(time()) ++ [erlang:system_time(millisecond) rem 1000, self(), ?MODULE | Args])).
-define(BT_DEBUG_LOG(IO, Format), ?BT_DEBUG_LOG(IO, Format, [])).

-define(BT_ERROR_LOG(Format, Args), io:format(unicode:characters_to_list(["~w:~w:~w ~w [error] ~p: ", Format, "~n"]), tuple_to_list(time()) ++ [self(), ?MODULE | Args])).
-define(BT_ERROR_LOG(Format), ?BT_ERROR(Format, [])).

-endif.
