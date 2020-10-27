%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 自我死亡
%%% 销毁数据
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_died).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{uid := UID} = State) ->
    self() ! {died, UID},
    ?INFO("~ts 被消灭!", [UID]),
    {?BT_SUCCESS, State#{cur_state := ?SET_STATE(?STATE_TYPE_DEAD)}}.