%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 扣除怒气
%%% 根据cost_rage扣除相应怒气，结果大于等于0
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_cost_rage).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(#{properties := #{cost_rage := CostRage}} = _BTree, #{cur_rage := CurRage} = State) ->
    State1 = State#{cur_rage := max(CurRage - CostRage, 0)},
    ?INFO("~ts：怒气值减少~w点", [maps:get(uid, State1), CurRage - maps:get(cur_rage, State1)]),
    {?BT_SUCCESS, State1}.