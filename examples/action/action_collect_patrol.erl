%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 寻找巡逻兵
%%% 找到：SUCCESS
%%% 未找到：FAILURE
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_collect_patrol).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(_BTree, #{cur_grid := CurGrid, rival_list := RivalList} = State) ->
    Fun = fun(UID) -> maps:get(type, game_dict:get_role_state(UID)) == ?HUMAN end,
    case lists:filter(Fun, game_dict:get_map_data(CurGrid)) of
        UIDList when UIDList /= [] ->
            ?INFO("~ts：嗅到了人类的味道，将要对[~ts]发起攻击", [maps:get(uid, State), lists:join(",", UIDList)]),
            {?BT_SUCCESS, State#{rival_list := UIDList ++ RivalList}};
        [] ->
            {?BT_FAILURE, State}
    end.