%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 移动一格
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_move_grid).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{uid := UID, cur_grid := OldGrid, grid_list := [Grid | T]} = State) ->
    game_dict:update_map_date(OldGrid, Grid, UID),
    State1 = State#{grid_list := T, cur_grid := Grid},
    ?INFO("~ts：移动到了~w格子", [UID, Grid]),
    {?BT_SUCCESS, State1}.