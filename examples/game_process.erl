%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 游戏进程
%%% @end
%%%-------------------------------------------------------------------
-module(game_process).

-behaviour(gen_server).

-include("example.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {TitleMaps, TreeMaps, TreeNodeMaps} = behavior_tree:load_tree_file("examples/example.json"),

    ZombieList = [
%%        <<"HUNTER"/utf8>>,
%%        <<"BOMMER"/utf8>>,
%%        <<"JOCKEY"/utf8>>,
%%        <<"SOMKER"/utf8>>,
%%        <<"SPITTER"/utf8>>,
%%        <<"CHARGER"/utf8>>,
%%        <<"TANK"/utf8>>,
        <<"WITCH"/utf8>>
    ],
    init_zombie(ZombieList, TitleMaps, TreeMaps, TreeNodeMaps),

    UID = <<"凯恩"/utf8>>,
    init_patrol(UID, TitleMaps, TreeMaps, TreeNodeMaps),
    erlang:put(uid_list, ZombieList ++ [UID]),
    self() ! run,
    {ok, #{frame => 1}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(run, #{frame := Frame} = State) ->
    try
        ?INFO("第~w分钟", [Frame]),
        Fun = fun(UID) -> run(UID) end,
        lists:foreach(Fun, get(uid_list)),
        erlang:send_after(1000, self(), run),
        {noreply, State#{frame := Frame + 1}}
    catch
        Error:Reason:StackTrace ->
            ?INFO("Error:~w Reason:~w ~n StackTrace:~p", [Error, Reason, StackTrace]),
            {stop, normal, State}
    end;

handle_info(finish, State) ->
    {stop, normal, State};

handle_info({died, UID}, State) ->
    erlang:put(uid_list, lists:delete(UID, erlang:get(uid_list))),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
new_state(UID, Type, CurGrid) ->
    case Type of
        ?HUMAN ->
            Power = ?MAX_POWER;
        ?ZOMBIE ->
            Power = ?RAND(?MAX_POWER div 2, ?MAX_POWER)
    end,
    State = #{
        uid => UID,
        type => Type,
        cur_state => ?STATE_TYPE_IDLE,
        cur_power => Power,
        cur_rage => 0,
        grid_list => [],
        cur_grid => CurGrid,
        rival_list => [],
        misc => #{}
    },
    ?INFO("生成单位:~tp", [State]),
    State.

init_zombie([UID | T], TitleMaps, TreeMaps, TreeNodeMaps) ->
    {ok, InitiativeBTreeID} = behavior_tree:init_btree_by_title(<<"丧尸主动AI"/utf8>>, TitleMaps, TreeMaps, TreeNodeMaps),
    {ok, PassivityBTreeID} = behavior_tree:init_btree_by_title(<<"丧尸被动AI"/utf8>>, TitleMaps, TreeMaps, TreeNodeMaps),
    game_dict:put_initiative_btree_id(UID, InitiativeBTreeID),
    game_dict:put_passivity_btree_id(UID, PassivityBTreeID),
    Grid = {?RAND(0, ?MAX_X), ?RAND(0, ?MAX_Y)},
    game_dict:put_role_state(UID, new_state(UID, ?ZOMBIE, Grid)),
    init_zombie(T, TitleMaps, TreeMaps, TreeNodeMaps);
init_zombie([], _TitleMaps, _TreeMaps, _TreeNodeMaps) ->
    ok.

init_patrol(UID, TitleMaps, TreeMaps, TreeNodeMaps) ->
    {ok, InitiativeBTreeID} = behavior_tree:init_btree_by_title(<<"巡逻兵主动AI"/utf8>>, TitleMaps, TreeMaps, TreeNodeMaps),
    {ok, PassivityBTreeID} = behavior_tree:init_btree_by_title(<<"巡逻兵被动AI"/utf8>>, TitleMaps, TreeMaps, TreeNodeMaps),
    game_dict:put_initiative_btree_id(UID, InitiativeBTreeID),
    game_dict:put_passivity_btree_id(UID, PassivityBTreeID),
    Grid = {?RAND(0, ?MAX_X), ?RAND(0, ?MAX_Y)},
    game_dict:put_role_state(UID, new_state(UID, ?HUMAN, Grid)).

run(UID) ->
    BTreeID = game_dict:get_initiative_btree_id(UID),
    #{cur_state := CurState} = State = game_dict:get_role_state(UID),
    case ?IS_STATE(?STATE_TYPE_DEAD, CurState) of
        true ->
            ok;
        false ->
            {_, State1} = behavior_tree:execute(BTreeID, State),
            game_dict:put_role_state(UID, State1)
    end.