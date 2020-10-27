%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 判断是否到达终点
%%% 是：SUCCESS
%%% 否：FAILURE
%%% @end
%%% Created : 07. 10月 2020 15:18
%%%-------------------------------------------------------------------
-module(cond_is_dest).
-include("behavior3.hrl").

%% API
-export([tick/2]).

tick(_BTree, State) ->
    case State of
        #{grid_list := []} ->
            {?BT_SUCCESS, State};
        #{} ->
            {?BT_FAILURE, State}
    end.