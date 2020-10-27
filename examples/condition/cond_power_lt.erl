%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 判断体力是否小于某值
%%% 是：SUCCESS
%%% 否：FAILURE
%%% @end
%%% Created : 07. 10月 2020 15:20
%%%-------------------------------------------------------------------
-module(cond_power_lt).
-include("behavior3.hrl").
-include("example.hrl").
%% API
-export([tick/2]).

tick(#{properties := #{power := LTPower}} = _BTree, #{cur_power := CurPower} = State) ->
    case CurPower < LTPower of
        true ->
            ?INFO("~ts：当前体力低于~w点，必须要尽快休息恢复体力！", [maps:get(uid, State), LTPower]),
            {?BT_SUCCESS, State};
        false ->
            {?BT_FAILURE, State}
    end.
