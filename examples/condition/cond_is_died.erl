%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 判断自己是否死亡
%%% 死亡：SUCCESS
%%% 存活：FAILURE
%%% @end
%%% Created : 07. 10月 2020 15:26
%%%-------------------------------------------------------------------
-module(cond_is_died).
-include("behavior3.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{cur_power := CurPower} = State) ->
    case CurPower =< 0 of
        true ->
            {?BT_SUCCESS, State};
        false ->
            {?BT_FAILURE, State}
    end.