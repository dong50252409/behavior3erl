%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 恢复体力
%%% 根据Max - Min之间随机恢复几点体力
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_recover_power).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(#{properties := #{max_power := MaxPower, min_power := MinPower}} = _BTree, #{cur_power := CurPower} = State) ->
    RecoverPower = ?RAND(MinPower, MaxPower),
    State1 = State#{cur_power := min(CurPower + RecoverPower, ?MAX_POWER)},
    ?INFO("~ts：体力值增加了~w点 当前体力：~w点", [maps:get(uid, State1), RecoverPower, maps:get(cur_power, State1)]),
    {?BT_SUCCESS, State1}.