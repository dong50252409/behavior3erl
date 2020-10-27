%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 删除死亡目标
%%% 删除rival_list中第一个目标
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_del_dead_target).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{rival_list := [_ | T]} = State) ->
    State1 = State#{rival_list := T},
    {?BT_SUCCESS, State1}.