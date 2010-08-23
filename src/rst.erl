-module(rst).

-export([start_node/3, 
         start_riak/1,
         start_application/2,
         config_to_string/1
         ]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% start node and add paths
start_node(Host, Name, Arg) ->
    {ok, Node} = slave:start_link(Host, Name, Arg),
    Ebins = filelib:wildcard(filename:join([filename:dirname(code:which(?MODULE)), "..", "deps", "*", "ebin"])),
    ok = rpc:call(Node, code, add_paths, [Ebins]),
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

%% Convert an application config proplist to a string - useful 
%% for passing application variables to slave:start_link/3 
%% 
%% Currently only works for atom, integer, and list values
config_to_string(Config) ->
    lists:foldl(fun({App, Params}, Acc) ->
                  Acc1 = Acc ++ " -" ++ atom_to_list(App),
                  lists:foldl(fun({Par, Val}, Pacc) ->
                                      Pacc ++ " " ++ atom_to_list(Par) ++ 
                                          " '" ++ value_to_string(Val) ++ "'"
                              end,
                              Acc1,
                              Params)
                end,
                "",
                Config).

value_to_string(Tuple) when is_tuple(Tuple) ->
    Values = lists:foldl(fun(Val, Acc) ->
                                 [value_to_string(Val)|Acc]
                         end,
                         [],
                         tuple_to_list(Tuple)),
    "{" ++ string:join(lists:reverse(Values), ", ") ++ "}";
value_to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
value_to_string(Int) when is_integer(Int) ->
    integer_to_list(Int);
value_to_string(List) when is_list(List) ->
    "\"" ++ List ++ "\"".
