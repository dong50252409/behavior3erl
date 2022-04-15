中文
=====
Behavior3的erlang支持库

快速开始
----
添加如下内容到**rebar.config**

    {deps, [
       ...
       {behavior3erl, "1.0.0"}
    ]}.

编译
----
    $ rebar3 compile
    
使用
----
    {TitleMaps, TreeMaps, TreeNodeMaps} = behavior_tree:load_tree_file("example.json"),
    {ok, RootID} = behavior_tree:init_btree_by_title(<<"example_ai"/utf8>>, TitleMaps, TreeMaps, TreeNodeMaps),
    {_BTStatus, _BTState1} = behavior_tree:execute(RootID, BTState = #{}).
    

调试信息
----
    {erl_opts, [
        {d, 'BT_DEBUG'}
    ]}.

    ./rebar3 clean -a
    ./rebar3 compile

更多
----
[behavior3editor](https://github.com/behavior3/behavior3editor)

[behavior3go](https://github.com/magicsea/behavior3go)

#### Behavior3使用系列文章：

[（一）行为树应用之行为树简介](http://note.youdao.com/s/77bGugj9)

[（二）行为树应用之组合节点](http://note.youdao.com/s/XiKlHPIr)

[（三）行为树应用之装饰节点](http://note.youdao.com/s/9Z6zI3YE)

[（四）行为树应用之自定义节点](http://note.youdao.com/s/AcRrY8ig)

[（五）行为树应用之加载行为树](http://note.youdao.com/s/DiqLf0ES)

[（六）行为树应用之节点执行](http://note.youdao.com/s/PI3Wic5D)

[（七）行为树应用之设计巡逻兵AI](http://note.youdao.com/s/HTCGTgAm)

[（八）行为树应用之设计丧尸AI](http://note.youdao.com/s/3wKFxcTw)

English
=====

Behavior3 by erlang library

Quickstart
----
add to **rebar.config**

    {deps, [
       ...
       {behavior3erl, "1.0.0"}
    ]}.

Build
----

    $ rebar3 compile
   
Usage
----
    {TitleMaps, TreeMaps, TreeNodeMaps} = behavior_tree:load_tree_file("example.json"),
    {ok, RootID} = behavior_tree:init_btree_by_title(<<"example_ai"/utf8>>, TitleMaps, TreeMaps, TreeNodeMaps),
    {_BTStatus, _BTState1} = behavior_tree:execute(RootID, BTState = #{}).
    

debug
----
    {erl_opts, [
        {d, 'BT_DEBUG'}
    ]}.

    ./rebar3 clean -a
    ./rebar3 compile

More
----

[behavior3editor](https://github.com/behavior3/behavior3editor)

[behavior3go](https://github.com/magicsea/behavior3go)
