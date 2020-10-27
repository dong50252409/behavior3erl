%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 数据字典
%%% @end
%%% Created : 18. 10月 2020 20:53
%%%-------------------------------------------------------------------
-module(game_dict).

-include("behavior3.hrl").
-include("example.hrl").

%% API
-export([
    get_map_data/1, update_map_date/3,
    get_initiative_btree_id/1, put_initiative_btree_id/2, erase_initiative_btree_id/1,
    get_passivity_btree_id/1, put_passivity_btree_id/2, erase_passivity_btree_id/1,
    get_role_state/1, put_role_state/2, erase_role_state/1
]).

-spec get_map_data(grid()) -> [uid()].
get_map_data(Grid) ->
    case erlang:get(Grid) of
        undefined ->
            [];
        L ->
            L
    end.

-spec update_map_date(grid(), grid(), uid()) -> ok.
update_map_date(OldGrid, NewGrid, UID) ->
    erlang:put(OldGrid, lists:delete(UID, get_map_data(OldGrid))),
    erlang:put(NewGrid, [UID | get_map_data(NewGrid)]),
    ok.

-spec get_initiative_btree_id(uid()) -> bt_uid().
get_initiative_btree_id(UID) ->
    erlang:get({initiative_btree_id, UID}).
put_initiative_btree_id(UID, BTreeID) ->
    erlang:put({initiative_btree_id, UID}, BTreeID).
erase_initiative_btree_id(UID) ->
    erlang:erase({initiative_btree_id, UID}).

-spec get_passivity_btree_id(uid()) -> bt_uid().
get_passivity_btree_id(UID) ->
    erlang:get({passivity_btree_id, UID}).
put_passivity_btree_id(UID, BTreeID) ->
    erlang:put({passivity_btree_id, UID}, BTreeID).
erase_passivity_btree_id(UID) ->
    erlang:erase({passivity_btree_id, UID}).

-spec get_role_state(uid()) -> map().
get_role_state(UID) ->
    erlang:get({role_state, UID}).
put_role_state(UID, State) ->
    erlang:put({role_state, UID}, State).
erase_role_state(UID) ->
    erlang:erase({role_state, UID}).
