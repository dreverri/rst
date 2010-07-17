-module(rst_tests).
-include_lib("eunit/include/eunit.hrl").

slave_start_riak_test() ->
    net_kernel:start(['rst@127.0.0.1']),
    rst_master:start_link(),
    [{node, Node}, {web, _}, {pb, _}, {handoff, _}] = rst_master:start_node(),
    [{node, Node1}, {web, _}, {pb, _}, {handoff, _}] = rst_master:start_node(),
    [{node, Node2}, {web, _}, {pb, _}, {handoff, _}] = rst_master:start_node(),
    rpc:call(Node, rst, start_riak, []),
    rpc:call(Node1, rst, start_riak, []),
    rpc:call(Node1, riak, join, [Node]),
    rpc:call(Node2, rst, start_riak, []),
    rpc:call(Node2, riak, join, [Node]),
    ?assertEqual(ok, rpc:call(Node, riak, client_test, [Node])),
    ?assertEqual(ok, rpc:call(Node1, riak, client_test, [Node1])),
    ?assertEqual(ok, rpc:call(Node2, riak, client_test, [Node2])).
 
