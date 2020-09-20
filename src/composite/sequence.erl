-module(sequence).


%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").

%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([tick/2]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec tick(bt_node(), bt_state()) -> {bt_status(), bt_state()}.
tick(#{children := Children} = _BTreeNode, State) ->
    tick_1(Children, State).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
tick_1([ChildId | T], State) ->
    case behavior_tree:do_execute(ChildId, State) of
        {?BT_SUCCESS, State1} ->
            tick_1(T, State1);
        {Status, State1} ->
            {Status, State1}
    end;
tick_1([], State) ->
    {?BT_SUCCESS, State}.
