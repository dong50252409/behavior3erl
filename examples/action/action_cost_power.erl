%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 扣除体力
%%% 获取Misc结构中的atk_dmg字段的值
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_cost_power).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, State) ->
    #{cur_power := CurPower, misc := #{atk_dmg := {AtkType, Dmg}} = Misc} = State,
    State1 = State#{cur_power := max(CurPower - Dmg, 0), misc := maps:remove(atk_dmg, Misc)},
    ?INFO("~ts：受到~ts攻击，体力值减少~w点 剩余体力~w点", [maps:get(uid, State1), AtkType, Dmg,maps:get(cur_power, State1)]),
    {?BT_SUCCESS, State1}.