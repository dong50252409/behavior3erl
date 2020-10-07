-module(load_behavior_tree).

-export([]).

load() ->
    State = #{},
    Ref = behavior_tree:load_tree_file("text.json"),
    {} = behavior_tree:execute(Ref, State),