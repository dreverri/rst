-module(rst_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    os:cmd("rm -rf tmp"),
    net_kernel:start(['rst@127.0.0.1']).

rst_test_() ->
    {setup, fun setup/0, {generator, fun() -> 
     [
      {"start_node/4 should start a remote node",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_start', ""),
                  ?assertEqual(pong, net_adm:ping(Node)),
                  cleanup()
              end)},

      {"remote node should set application environment from Arg",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_env', "-kernel key value"),
                  ?assertEqual({ok, value}, rpc:call(Node, application, get_env, [kernel, key])),
                  cleanup()
              end)},
      
      {"remote node should set bitcask environment value",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_bitcask_env', " -bitcask key 'value'"),
                  ok = rpc:call(Node, application, start, [bitcask]),
                  ?assertEqual({ok, value}, rpc:call(Node, application, get_env, [bitcask, key])),
                  cleanup()
              end)},

      {"remote node should have access to riak",
       ?_test(begin
                  Node = rst:start_node('127.0.0.1', 'test_riak_access', ""),
                  ?assert(non_existing =/= rpc:call(Node, code, which, [riak_core])),
                  cleanup()
              end)},

      {"riak should start successfully (check client_test and data directory)",
       ?_test(begin
                  Arg = rst:config_to_string(riak_config(1)),
                  Node = rst:start_node('127.0.0.1', 'riak_start', Arg),
                  rst:start_riak(Node),
                  ?assertEqual(ok, rpc:call(Node, riak, client_test, [Node])),
                  ?assert(filelib:is_dir("tmp/1/data/bitcask")),
                  cleanup()
              end)},

      {"cluster membership test", {timeout, 1000, 
       ?_test(begin
                  B = <<"bucket">>,
                  K = <<"key">>,
                  V = <<"value">>,
                  R1 = 1,
                  
                  error_logger:info_msg("Starting node 1~n"),
                  N1 = quick_start(1),
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
                  quick_start(1),
                  quick_start(2),
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
                  assert_read(N4, B, K, R1),
                  cleanup()
              end)}}
     ] end}}.

cleanup() ->
    lists:foreach(fun(Node) ->
                          slave:stop(Node) end, nodes()).

riak_config(Int) when is_integer(Int) ->
    H = (P = (W = 7000 + (Int - 1) * 3) + 1) + 1,
    Name = integer_to_list(Int),
    [
     {riak_core, [
                  {web_ip, "127.0.0.1"}, 
                  {web_port, W}, 
                  {handoff_port, H},
                  {ring_state_dir, filename:join(["tmp", Name, "data", "ring"])}
                 ]},
     {riak_kv, [
                {pb_ip, "127.0.0.1"}, 
                {pb_port, P},
                {storage_backend, riak_kv_bitcask_backend}
               ]},
     {bitcask, [
                {data_root, filename:join(["tmp", Name, "data", "bitcask"])}
               ]},
     {sasl, [
             {sasl_error_logger, {file, filename:join(["tmp", Name, "log", "sasl-error.log"])}},
             {error_logger_mf_dir, filename:join(["tmp", Name, "log", "sasl"])}
            ]}
    ].

%% Start a node named Int with unique web, protobuff, and handoff ports
%% ports are incremented from 7000
%% 1 -> 7000, 7001, 7002
%% 2 -> 7003, 7004, 7005
quick_start(Int) ->
    C = riak_config(Int),
    A = rst:config_to_string(C),
    Node = rst:start_node('127.0.0.1', list_to_atom(integer_to_list(Int)), A),
    rst:start_riak(Node),
    Node.

start_join_wait(Int, Cluster) ->
    N = quick_start(Int),
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
            wait_for_cluster(Nodes, RingHashes);
        true ->
            case length(lists:ukeysort(2, RingHashes)) =:= 1 of
                true ->
                    ok;
                false ->
                    wait_for_cluster(Nodes, RingHashes)
            end
    end.
