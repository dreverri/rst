-module(rst).

-include("rst.hrl").

-export([start_node/4, 
         start_riak/1,
         start_application/2,
         clone_repo/2,
         run_make/2
         ]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% start node and add paths
start_node(Host, Name, Arg, RiakDir) ->
    %%error_logger:error_report(Arg),
    {ok, Node} = slave:start_link(Host, Name, Arg),
    AppEbins = filelib:wildcard(filename:join([RiakDir, "apps", "*", "ebin"])),
    DebEbins = filelib:wildcard(filename:join([RiakDir, "deps", "*", "ebin"])),
    ok = rpc:call(Node, code, add_paths, [AppEbins ++ DebEbins]), 
    Node.

%% start riak on the given node
start_riak(Node) ->
    start_application(Node, riak_kv).

start_application(Node, Application) when is_atom(Application) ->
    Deps = collect_deps(Node, [Application], []),
    start_application(Node, Deps ++ [Application]);
                      
start_application(Node, Apps) when is_list(Apps) ->
    StartFun = fun() -> [application:start(App) || App <- Apps] end,
    rpc:call(Node, erlang, apply, [StartFun, []]).

collect_deps(_Node, [], Collected) ->
    Deps = lists:foldl(fun(App, Acc) ->
                               case lists:member(App, Acc) of
                                   true -> Acc;
                                   false -> [App|Acc]
                               end end, [], lists:reverse(Collected)),
    lists:reverse(Deps);
collect_deps(Node, [App|Applications], Collected) ->
    rpc:call(Node, application, load, [App]),
    {ok, Deps} = rpc:call(Node, application, get_key, [App, applications]),
    collect_deps(Node, Applications ++ Deps, Collected ++ Deps).

%% AppDir is the location to clone to
%% Url is the URL of the hg repo
%% Rev is the version to checkout
clone_repo(AppDir, {hg, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("hg clone -U ~s ~s", [Url, filename:basename(AppDir)]), [], filename:dirname(AppDir)),
    rebar_utils:sh(?FMT("hg update ~s", [Rev]), [], AppDir).

run_make(Dir, Args) ->
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    os:cmd("make " ++ Args),
    ok = file:set_cwd(Cwd).
