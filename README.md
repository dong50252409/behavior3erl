中文
=====
Erlang的Behavior3运行库

快速开始
----
添加如下内容到`rebar.config`

    {deps, [
       ...
       {behavior3, "2.0.0"}
    ]}.

编译
----
    $ rebar3 compile
    
使用
----
    Filename = "example.json",
    BTTitle = <<"example_ai"/utf8>>, 
    State = #{},
    %% 执行这步后会产生大量垃圾，建议手动执行一次erlang:garbage_collect/0,1,2
    TreeMod = behavior_tree:load_tree_file(Filename),
    BB = blackboard:init_blackboard(TreeMod, BTTitle),
    {_BTStatus, _BB1, _State1} = behavior_tree:execute(BB, State).
    

调试
----
执行以下步骤后，在行为树运行时将节点模块信息写入对应的`{BTTitle}_{PID}_bt.log`文件中

    {erl_opts, [
        {d, 'BT_DEBUG'}
    ]}.

    ./rebar3 clean -a
    ./rebar3 compile
    
热更新行为树
----
行为树配置文件发生改变时，可调用`behavior_tree:load_tree_file/1`进行生成加载，并返回一个新的模块名供后续使用。

考虑到老的行为树模块会有已运行的行为树调用，自动清除可能会造成异常，老的行为树模块由使用者自行决定后续操作，可调用`behavior_tree:unload_tree_mod/1`清除。

更多
----
[behavior3editor](https://github.com/behavior3/behavior3editor)

[behavior3go](https://github.com/magicsea/behavior3go)

#### Behavior3使用系列文章：

[（一）行为树应用之行为树简介](http://note.youdao.com/s/77bGugj9)

[（二）行为树应用之组合节点](http://note.youdao.com/s/XiKlHPIr)

[（三）行为树应用之装饰节点](http://note.youdao.com/s/9Z6zI3YE)

[（四）行为树应用之自定义节点](http://note.youdao.com/s/AcRrY8ig)

English
=====

Behavior3 library for Erlang

Quickstart
----
add to `rebar.config`

    {deps, [
       ...
       {behavior3erl, "2.0.0"}
    ]}.

Build
----

    $ rebar3 compile
   
Usage
----
    Filename = "example.json",
    BTTitle = <<"example_ai"/utf8>>, 
    State = #{},
    %% This step generates a lot of garbage and a manual erlang:garbage_collect/0,1,2 is recommended
    TreeMod = behavior_tree:load_tree_file(Filename), 
    BB = blackboard:init_blackboard(TreeMod, BTTitle),
    {_BTStatus, _BB1, _State1} = behavior_tree:execute(BB, State).
    

DEBUG
----
After performing the following steps, write the node module information to the corresponding '{BTTitle}_{PID}_bt.log' file while the behavior tree is running

    {erl_opts, [
        {d, 'BT_DEBUG'}
    ]}.

    ./rebar3 clean -a
    ./rebar3 compile

HotFix
----
When the behavior tree configuration file changes, call `behavior_tree:load_tree_file/1` to load the configuration file and return a new module name for future use

Because the old behavior tree module has running behavior tree calls, automatic clearing may cause exceptions. Follow-up operations of the old behavior tree module are determined by users. You can call `behavior_tree:unload_tree_mod/1` to clear the behavior tree module

More
----

[behavior3editor](https://github.com/behavior3/behavior3editor)

[behavior3go](https://github.com/magicsea/behavior3go)
