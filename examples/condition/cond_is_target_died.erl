%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 判断目标是否死亡
%%% 死亡：SUCCESS
%%% 未死亡：FAILURE
%%% @end
%%% Created : 07. 10月 2020 15:20
%%%-------------------------------------------------------------------
-module(cond_is_target_died).
-include("behavior3.hrl").
-include("example.hrl").
%% API
-export([tick/2]).

tick(_BTree, State) ->
    case State of
        #{rival_list := [RivalUID | _]} ->
            case game_dict:get_role_state(RivalUID) of
                #{cur_state := CurState} when ?IS_STATE(?STATE_TYPE_DEAD, CurState) ->
                    {?BT_SUCCESS, State};
                #{} ->
                    {?BT_FAILURE, State}
            end;
        #{} ->
            {?BT_FAILURE, State}
    end.
