-module(rst).

-export([start_riak/0, start_node/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

start_node(Web, Pb, Handoff) ->
    Env = [{riak_core, [
                  {cluster_name, "default"},
                  {ring_state_dir, "tmp/" ++ integer_to_list(Web)},
                  {web_ip, "127.0.0.1"},
                  {web_port, Web},
                  {handoff_port, Handoff}]},
     {riak_kv, [{storage_backend, riak_kv_ets_backend},
                {pb_ip, "127.0.0.1"},
                {pb_port, Pb}]}],
    EnvString = env_to_string(Env),
    Node = start_slave(list_to_atom("rst" ++ integer_to_list(Web)), EnvString),
    [{node, Node}, {web, Web}, {pb, Pb}, {handoff, Handoff}].
    
env_to_string(Env) ->
    lists:foldl(fun({App, Params}, Acc) ->
                  Acc1 = Acc ++ " -" ++ atom_to_list(App),
                  lists:foldl(fun({Par, Val}, Pacc) ->
                                      Pacc ++ " " ++ atom_to_list(Par) ++ " '" ++ to_string(Val) ++ "'"
                              end,
                              Acc1,
                              Params)
                end,
                "",
                Env).

to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_string(List) when is_list(List) ->
    "\"" ++ List ++ "\"".

start_riak() ->
    Apps = [
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
           ],
    start_apps(Apps).

start_apps(Apps) ->
    lists:foldl(fun(App, Acc) ->
                        case application:start(App) of
                            {ok, _} ->
                                [ok|Acc];
                            Error ->
                                [{Error, App}|Acc]
                        end
                end,
                [],
                Apps).

%% start slave node and add paths 
start_slave(Name, Env) ->
    {ok, Node} = slave:start_link('127.0.0.1', Name, Env),
    rpc:call(Node, code, add_paths, [code:get_path()]),
    Node.
