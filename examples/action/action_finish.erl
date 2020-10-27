%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 游戏结束，根据is_win属性打印结果
%%% 打印结果，结束行为树运行
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_finish).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(#{properties := #{is_win := IsWin}} = _BTree, #{uid := UID} = State) ->
    self() ! finish,
    ?INFO("游戏结束，~ts~ts!", [UID, IsWin]),
    {?BT_SUCCESS, State}.