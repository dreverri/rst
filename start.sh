erl -pa ebin/ riak/apps/*/ebin/ riak/deps/*/ebin/ -name rst@127.0.0.1 
# -riak_core cluster_name '"reverri"' ring_state_dir '"<nostore>"' web_ip '"127.0.0.1"' web_port '8001' handoff_port '8101' -riak_kv storage_backend 'riak_kv_ets_backend' pb_ip '"127.0.0.1"' pb_port '8201'
