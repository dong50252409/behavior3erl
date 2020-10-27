%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 收集巡逻路径
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_collect_path).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2, get_grid_path/1]).


tick(_BTree, #{cur_grid := CurGrid} = State) ->
    State1 = State#{grid_list := get_grid_path(CurGrid)},
    {?BT_SUCCESS, State1}.

get_grid_path(CurGrid) ->
    List = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
    Max = length(List),
    List1 = [E || {_, E} <- lists:sort([{?RAND(1, Max), E} || E <- List])],
    case get_grid_path_1(List1, CurGrid, []) of
        [] ->
            get_grid_path(CurGrid);
        Result ->
            Result
    end.

get_grid_path_1([{X, Y} | T], {CurX, CurY}, Result) ->
    X1 = CurX + X,
    Y1 = CurY + Y,
    Grid = {X1, Y1},
    case X1 >= 0 andalso X1 =< ?MAX_X andalso Y1 >= 0 andalso Y1 =< ?MAX_Y of
        true ->
            get_grid_path_1(T, Grid, [Grid | Result]);
        false ->
            []
    end;
get_grid_path_1([], _, Result) ->
    Result.

