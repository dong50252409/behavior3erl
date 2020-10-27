%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 判断怒气值是否已满
%%% 已满：SUCCESS
%%% 未满：FAILURE
%%% @end
%%% Created : 07. 10月 2020 15:18
%%%-------------------------------------------------------------------
-module(cond_is_rage_full).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{cur_rage := CurRage} = State) ->
    case CurRage >= ?MAX_RAGE of
        true ->
            {?BT_SUCCESS, State};
        false ->
            {?BT_FAILURE, State}
    end.