%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 增加怒气
%%% 根据add_rage增加相应怒气，结果小于等于100
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_add_rage).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(#{properties := #{add_rage := AddRage}} = _BTree, #{cur_rage := CurRage} = State) ->
    State1 = State#{cur_rage := min(CurRage + AddRage, ?MAX_RAGE)},
    ?INFO("~ts：怒气值增加~w点 总怒气~w点", [maps:get(uid, State1), AddRage, maps:get(cur_rage, State1)]),
    {?BT_SUCCESS, State1}.