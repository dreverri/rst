-module(rst_tests).
-include_lib("eunit/include/eunit.hrl").

%% Define when riak repo should be relative to ".eunit"
-define(RIAK, "../riak").

setup() ->
    %% @todo find a better way of outputting info from setup
    %% @todo make riak dependency configurable (DVCS, URL, Dir)
    RiakDir = ?RIAK,
    case filelib:is_dir(RiakDir) of
        true ->
            error_logger:info_msg("Found riak in directory: ~p\n", [RiakDir]);
        false ->
            error_logger:info_msg("Did not find riak in directory: ~p\n", [RiakDir]),
            error_logger:info_msg("Cloning riak to directory: ~p\n", [RiakDir]),
            rst:clone_repo(RiakDir, {hg, "https://hg.basho.com/riak", "tip"})
    end,

    %% run make all
    RiakCoreEbin = filename:join([RiakDir, "apps", "riak_core", "ebin"]),
    case filelib:is_file(filename:join([RiakCoreEbin, "riak_core.beam"])) of
        true ->
            error_logger:info_msg("Found riak_core.beam in directory: ~p\n", [RiakCoreEbin]);
        false ->
            error_logger:info_msg("Did not find riak_core.beam in directory: ~p\n", [RiakCoreEbin]),
            error_logger:info_msg("Running make all in directory: ~p\n", [RiakCoreEbin]),
            rst:run_make(RiakDir, "all"),
            AppEbins = filelib:wildcard(filename:join([RiakDir, "apps", "*", "ebin"])),
            DebEbins = filelib:wildcard(filename:join([RiakDir, "deps", "*", "ebin"])),
            code:add_paths(AppEbins ++ DebEbins)
    end,

    %% clear out any node data (tmp/NodeName)
    os:cmd("rm -rf tmp"),

    %% ensure net kernel is started so we can start other nodes
    net_kernel:start(['rst@127.0.0.1']).

rst_test_() ->
    {setup, fun setup/0,
     [
      {"start_node/4 should start a remote node",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_start', "", ?RIAK),
                  ?assertEqual(pong, net_adm:ping(Node)),
                  cleanup_nodes([Node])
              end)},

      {"remote node should set application environment from Arg",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_env', "-kernel key value", ?RIAK),
                  ?assertEqual({ok, value}, rpc:call(Node, application, get_env, [kernel, key])),
                  cleanup_nodes([Node])
              end)},
      
      {"remote node should set bitcask environment value",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_bitcask_env', " -bitcask key 'value'", ?RIAK),
                  ok = rpc:call(Node, application, start, [bitcask]),
                  ?assertEqual({ok, value}, rpc:call(Node, application, get_env, [bitcask, key])),
                  cleanup_nodes([Node])
              end)},

      {"remote node should have access to riak",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_riak_access', "", ?RIAK),
                  ?assert(non_existing =/= rpc:call(Node, code, which, [riak_core])),
                  cleanup_nodes([Node])
              end)},

      {"riak should start successfully (check client_test and data directory)",
       ?_test(begin
                  Config = rst_config:new('riak_start', []),
                  Arg = rst_config:to_string(Config),
                  Node = rst:start_node('127.0.0.1', 'riak_start', Arg, ?RIAK),
                  rst:start_riak(Node),
                  ?assertEqual(ok, rpc:call(Node, riak, client_test, [Node])),
                  ?assert(filelib:is_dir("tmp/riak_start/data/bitcask")),
                  cleanup_nodes([Node])
              end)},

      {"cluster membership test", {timeout, 1000, 
       ?_test(begin
                  B = <<"bucket">>,
                  K = <<"key">>,
                  V = <<"value">>,
                  R1 = 1,
                  
                  error_logger:info_msg("Starting node 1~n"),
                  N1 = quick_start(1, ?RIAK),
                  ok = write(N1, B, K, V),
                  
                  error_logger:info_msg("Starting node 2~n"),
                  N2 = start_join_wait(2, [N1]),
                  assert_read(N2, B, K, R1),

                  error_logger:info_msg("Starting node 3~n"),
                  N3 = start_join_wait(3, [N1, N2]),
                  assert_read(N3, B, K, R1),
  
                  error_logger:info_msg("Starting node 4~n"),
                  N4 = start_join_wait(4, [N1, N2, N3]),
                  assert_read(N3, B, K, R1),

                  error_logger:info_msg("Stopping node 1~n"),
                  slave:stop(N1),
                  read(N2, B, K, R1),
                  read(N3, B, K, R1),
                  read(N4, B, K, R1),
                  assert_read(N2, B, K, R1),
                  assert_read(N3, B, K, R1),
                  assert_read(N4, B, K, R1),
                  
                  error_logger:info_msg("Stopping node 2~n"),
                  slave:stop(N2),
                  read(N3, B, K, R1),
                  read(N4, B, K, R1),
                  assert_read(N3, B, K, R1),
                  assert_read(N4, B, K, R1),
                  
                  error_logger:info_msg("Starting nodes 1 and 2~n"),
                  quick_start(1, ?RIAK),
                  quick_start(2, ?RIAK),
                  read(N1, B, K, R1),
                  read(N2, B, K, R1),
                  read(N3, B, K, R1),
                  read(N4, B, K, R1),
                  assert_read(N1, B, K, R1),
                  assert_read(N2, B, K, R1),
                  assert_read(N3, B, K, R1),
                  assert_read(N4, B, K, R1),
                  
                  error_logger:info_msg("Leaving nodes 1, 2, and 3~n"),
                  leave(N1),
                  leave(N2),
                  leave(N3),
                  read(N4, B, K, R1),
                  assert_read(N4, B, K, R1)
              end)}}
     ]}.

cleanup_nodes(Nodes) ->
    lists:foreach(fun(Node) ->
                          slave:stop(Node) end, Nodes).

%% Start a node named Int with unique web, protobuff, and handoff ports
%% ports are incremented from 7000
%% 1 -> 7000, 7001, 7002
%% 2 -> 7003, 7004, 7005
quick_start(Int, RiakDir) ->
    C = rst_config:new(Int),
    A = rst_config:to_string(C),
    Node = rst:start_node('127.0.0.1', list_to_atom(integer_to_list(Int)), A, RiakDir),
    rst:start_riak(Node),
    Node.

start_join_wait(Int, Cluster) ->
    N = quick_start(Int, ?RIAK),
    ok = rpc:call(N, riak, join, [hd(Cluster)]),
    ok = wait_for_cluster([N|Cluster]),
    N.

write(N, B, K, V) ->
    {ok, RC} = riak:client_connect(N),
    RC:put(riak_object:new(B, K, V)).

assert_read(N, B, K, R) ->
    ?assertEqual(ok, element(1, read(N, B, K, R))).
    
read(N, B, K, R) ->
    {ok, RC} = riak:client_connect(N),
    RC:get(B, K, R).

leave(N) ->
    rpc:call(N, riak_kv_console, leave, [[]]).
    
wait_for_cluster(Nodes) ->
    Me = self(),
    [rpc:call(N, riak_core_ring_events, add_callback, 
              [fun(Ring) -> Me ! {ring_update, N, Ring} end]) || N <- Nodes],
    wait_for_cluster(Nodes, []).

wait_for_cluster(Nodes, RingHashes) ->
    receive
        {ring_update, N, Ring} ->
            RingHash = erlang:phash2(element(4, Ring)),
            check_rings(Nodes, lists:keystore(N, 1, RingHashes, {N, RingHash}))
    end.

check_rings(Nodes, RingHashes) ->
    case length(Nodes) =:= length(RingHashes) of
        false ->
            send_rings(Nodes),
            wait_for_cluster(Nodes, RingHashes);
        true ->
            case length(lists:ukeysort(2, RingHashes)) =:= 1 of
                true ->
                    ok;
                false ->
                    send_rings(Nodes),
                    wait_for_cluster(Nodes, RingHashes)
            end
    end.

send_rings(Nodes) ->
    lists:foldl(fun(Current, Previous) ->
                        rpc:call(Current, riak_core_gossip, send_ring, [Previous]),
                        Current
                end, hd(lists:reverse(Nodes)), Nodes).
    
