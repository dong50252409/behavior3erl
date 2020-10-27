%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 技能攻击
%%% 随机伤害值 RAND(min_power ,max_power) * RAND(min_rate, max_rate)
%%% 返回：SUCCESS
%%% @end
%%% Created : 07. 10月 2020 15:13
%%%-------------------------------------------------------------------
-module(action_skill_attack).
-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([tick/2]).

tick(#{properties := Prop} = _BTree, #{uid := UID, rival_list := [RivalUID | _]} = State) ->
    BTreeID = game_dict:get_passivity_btree_id(RivalUID),
    RivalState = get_be_attacker_rival_state(Prop, UID, RivalUID),
    {_, RivalState1} = behavior_tree:execute_sub_tree(BTreeID, RivalState),
    game_dict:put_role_state(RivalUID, RivalState1),
    {?BT_SUCCESS, State}.

get_be_attacker_rival_state(Prop, UID, RivalUID) ->
    #{
        skill_type := SkillType,
        min_power := MinPower, max_power := MaxPower,
        min_rate := MinRate, max_rate := MaxRate
    } = Prop,
    #{rival_list := RivalList} = RivalState = game_dict:get_role_state(RivalUID),
    case lists:member(UID, RivalList) of
        true ->
            RivalList1 = RivalList;
        false ->
            RivalList1 = RivalList ++ [UID]
    end,
    RivalState1 = RivalState#{
        rival_list := RivalList1,
        misc := #{atk_dmg => {SkillType, ?RAND(MinPower, MaxPower) * ?RAND(MinRate, MaxRate)}}
    },
    game_dict:put_role_state(RivalUID, RivalState1),
    ?INFO("~ts：发动了~ts攻击", [UID, SkillType]),
    RivalState1.