%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 判断是否处于巡逻状态
%%% 是：SUCCESS
%%% 否：FAILURE
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(cond_is_patrolling).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{cur_state := CurState} = State) ->
    case ?IS_STATE(?STATE_TYPE_PATROLLING, CurState) of
        true ->
            {?BT_SUCCESS, State};
        false ->
            {?BT_FAILURE, State}
    end.