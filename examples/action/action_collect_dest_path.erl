%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 收集终点路径
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_collect_dest_path).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{cur_grid := CurGrid, grid_list := GirdList} = State) ->
    case GirdList of
        [] ->
            EndGrid = get_end_grid(CurGrid),
            State1 = State#{grid_list := get_grid_path(CurGrid, EndGrid)},
            ?INFO("~ts：找到穿传说中的“大蒜”的位置，要尽快到达那里！", [maps:get(uid, State1)]),
            {?BT_SUCCESS, State1};
        _ ->
            ?INFO("~ts：天要黑了，要尽快到达那里！", [maps:get(uid, State)]),
            {?BT_SUCCESS, State}
    end.

get_end_grid(CurGrid) ->
    EndGridList = [{0, 0}, {0, ?MAX_X}, {?MAX_Y, ?MAX_X}, {?MAX_Y, 0}],
    {_, EndGrid} = hd(lists:sort([{calc_distance(CurGrid, Grid), Grid} || Grid <- EndGridList])),
    EndGrid.

calc_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

get_grid_path(CurGrid, EndGrid) ->
    case EndGrid of
        {0, 0} ->
            get_grid_path(CurGrid, [EndGrid], 1, 1);
        {0, ?MAX_X} ->
            get_grid_path(CurGrid, [EndGrid], 1, -1);
        {?MAX_Y, ?MAX_X} ->
            get_grid_path(CurGrid, [EndGrid], -1, -1);
        {?MAX_Y, 0} ->
            get_grid_path(CurGrid, [EndGrid], -1, 1)
    end.

get_grid_path(CurGrid, GridPath, XInc, YInc) ->
    case GridPath of
        [_,CurGrid|T] ->
            [CurGrid|T];
        [CurGrid | _] ->
            GridPath;
        [{X, Y} | _] ->
            case Y + YInc of
                Y1 when Y1 == 0 orelse Y1 == ?MAX_X ->
                    get_grid_path(CurGrid, [{X + XInc, Y1}, {X, Y1} | GridPath], XInc, bnot YInc + 1);
                Y1 ->
                    get_grid_path(CurGrid, [{X, Y1} | GridPath], XInc, YInc)
            end
    end.
