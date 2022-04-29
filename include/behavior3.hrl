-ifndef(behavior3_h).
-define(behavior3_h, true).

-define(BT_SUCCESS, 1).
-define(BT_FAILURE, 2).
-define(BT_RUNNING, 3).
-define(BT_ERROR, 4).

-record(tree_node,{
    id :: node_id(),
    name :: module(),
    properties :: properties(),
    children :: [node_id()]
}).

-type tree_node() :: tree_node().

-type node_id() :: integer().

-type bt_state() :: #{'$b3_running_title' => map(), '$b3_running_mod' => module(), '$b3_maps' => map(), term() => term()}.

-type properties() :: #{atom() => term()}.

-type bt_status() :: ?BT_SUCCESS|?BT_FAILURE|?BT_RUNNING|?BT_ERROR.

-define(SKIP_MOD, [
    mem_priority, mem_sequence, priority, sequence,
    repeat_until_failure, repeat_until_success, repeater
]).

-define(BT_DEBUG_LOG(IO, Format, Args), io:format(IO, unicode:characters_to_list(["~w:~w:~w:~w ~w [debug] ~p: ", Format, "~n"]), tuple_to_list(time()) ++ [erlang:system_time(millisecond) rem 1000, self(), ?MODULE | Args])).
-define(BT_DEBUG_LOG(IO, Format), ?BT_DEBUG_LOG(IO, Format, [])).

-define(BT_ERROR_LOG(Format, Args), io:format(unicode:characters_to_list(["~w:~w:~w ~w [error] ~p: ", Format, "~n"]), tuple_to_list(time()) ++ [self(), ?MODULE | Args])).
-define(BT_ERROR_LOG(Format), ?BT_ERROR(Format, [])).

-endif.
