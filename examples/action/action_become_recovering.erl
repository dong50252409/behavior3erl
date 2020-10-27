%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 变为恢复状态
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_become_recovering).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, State) ->
    State1 = State#{cur_state := ?SET_STATE(?STATE_TYPE_RECOVERING)},
    ?INFO("~ts：变为恢复状态", [maps:get(uid, State1)]),
    {?BT_SUCCESS, State1}.