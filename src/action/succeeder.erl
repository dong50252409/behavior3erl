-module(succeeder).

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
tick(_BTNode, BTState) ->
    {?BT_SUCCESS, BTState}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
