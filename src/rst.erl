-module(rst).

-include("rst.hrl").

-export([start_node/4, 
         start_riak/1,
         clone_repo/2,
         run_make/2
         ]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% start node and add paths
start_node(Host, Name, Arg, RiakDir) ->
    {ok, Node} = slave:start_link(Host, Name, Arg),
    AppEbins = filelib:wildcard(filename:join([RiakDir, "apps", "*", "ebin"])),
    DebEbins = filelib:wildcard(filename:join([RiakDir, "deps", "*", "ebin"])),
    ok = rpc:call(Node, code, add_paths, [AppEbins ++ DebEbins]), 
    Node.

%% start riak on the given node
start_riak(Node) ->
    StartFun = fun() ->
            lists:foldl(fun(App, Acc) ->
                                case application:start(App) of
                                    ok ->
                                        [{ok, App}|Acc];
                                    Error ->
                                        error_logger:error_report([{application, App},
                                                                   Error]),
                                        [{error, App}|Acc]
                                end
                        end,
                        [],
                        [
                         %% list of applications to start on remote node
                         sasl,
                         os_mon,
                         crypto,
                         mochiweb,
                         webmachine,
                         erlang_js,
                         luke,
                         bitcask,
                         riak_core,
                         riak_kv
                        ])
    end,
    rpc:call(Node, erlang, apply, [StartFun, []]).


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
