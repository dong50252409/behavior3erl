%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 定义
%%% @end
%%% Created : 07. 10月 2020 15:16
%%%-------------------------------------------------------------------
-type uid() :: binary().
-type grid() :: {X :: non_neg_integer(), Y :: non_neg_integer()}.

-define(INFO(Format, Args), io:format(Format ++ "~n", Args)).

-define(MAX_X, 5).
-define(MAX_Y, 5).

-define(HUMAN, 1).
-define(ZOMBIE, 2).

-define(RAND(Min, Max), (rand:uniform(Max - (Min - 1)) + (Min - 1))).

-define(STATE_TYPE_IDLE, 1).
-define(STATE_TYPE_ATTACKING, 2).
-define(STATE_TYPE_PATROLLING, 3).
-define(STATE_TYPE_RECOVERING, 4).
-define(STATE_TYPE_DEAD, 5).

-define(IS_STATE(StateType, CurState), (CurState band (1 bsl (StateType - 1)) > 0)).
-define(SET_STATE(StateType), (1 bsl (StateType - 1))).

-define(MAX_POWER, 100).
-define(MAX_RAGE, 100).
