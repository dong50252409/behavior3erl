-module(wait).


%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([open/2, tick/2, close/2]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec open(bt_node(), bt_state()) -> bt_state().
open(#{id := Id}, State) ->
    blackboard:set(start_time, erlang:system_time(millisecond), Id, State).

-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{id := Id, properties := #{milliseconds := EndTime}} = _BTreeNode, State) ->
    StartTime = blackboard:get(start_time, Id, State),
    case erlang:system_time(millisecond) - StartTime > EndTime of
        true ->
            {?BT_SUCCESS, State};
        false ->
            {?BT_RUNNING, State}
    end.

-spec close(bt_node(), bt_state()) -> bt_state().
close(#{id := Id} = _BTreeNode, State) ->
    blackboard:remove(start_time, Id, State).
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
