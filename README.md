中文
=====
Erlang的行为树运行库，使用 [behavior3editor](https://github.com/behavior3/behavior3editor) 编辑生成行为树

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
添加`{d,'BT_DEBUG'}`到`rebar.config`文件中，默认调试信息输出到控制台，可调用`blackboard:set_io/2`重定向日志输出位置

    {erl_opts, [
        {d, 'BT_DEBUG'}
    ]}.
    ./rebar3 clean -a
    ./rebar3 compile
    
热更新行为树
----
行为树配置文件发生改变时，可调用`behavior_tree:load_tree_file/1`进行生成加载，并返回一个新的模块名供后续使用。

考虑到老的行为树模块会有已运行的行为树调用，自动清除可能会造成异常，老的行为树模块由使用者自行决定后续操作，可调用`behavior_tree:unload_tree_mod/1`清除。

一个DEMO
----
[rogue_adventure](https://github.com/dong50252409/rogue_adventure)


Behavior3使用系列文章：
----
里面的代码实例是1.0.0版本的，各位主要看看概念就好

[（一）行为树应用之行为树简介](http://note.youdao.com/s/77bGugj9)

[（二）行为树应用之组合节点](http://note.youdao.com/s/XiKlHPIr)

[（三）行为树应用之装饰节点](http://note.youdao.com/s/9Z6zI3YE)

[（四）行为树应用之自定义节点](http://note.youdao.com/s/AcRrY8ig)

English
=====

Erlang runtime library behavior tree, use [behavior3editor](https://github.com/behavior3/behavior3editor) to edit generated tree

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
Add `{d,'BT_DEBUG'}` to `rebar.config` and default debug information is output to the console by calling `blackboard:set_IO/2` to redirect the log output location

    {erl_opts, [
        {d, 'BT_DEBUG'}
    ]}.
    ./rebar3 clean -a
    ./rebar3 compile

HotFix
----
When the behavior tree configuration file changes, call `behavior_tree:load_tree_file/1` to load the configuration file and return a new module name for future use

Because the old behavior tree module has running behavior tree calls, automatic clearing may cause exceptions. Follow-up operations of the old behavior tree module are determined by users. You can call `behavior_tree:unload_tree_mod/1` to clear the behavior tree module
