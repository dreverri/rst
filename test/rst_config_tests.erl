-module(rst_config_tests).
-include_lib("eunit/include/eunit.hrl").

config_test_() ->
    [
     {"update_config should update an existing value",
      ?_test(begin
                 Config = rst_config:update(app, key, value2, [{app, [{key, value}]}]),
                 ?assertEqual([{app, [{key, value2}]}], Config)
             end)},

     {"update_config should add values if they don't exist",
      ?_test(begin
                 Config = rst_config:update(app, key, value, [{app, []}]),
                 ?assertEqual([{app, [{key, value}]}], Config)
             end)},
                 
     {"update_config should add applications if they don't exist",
      ?_test(begin
                 Config = rst_config:update(app, key, value, []),
                 ?assertEqual([{app, [{key, value}]}], Config)
             end)},

     {"update_config should not affect other values",
      ?_test(begin
                 Config = rst_config:update(app, key, value, [{app, [{key2, value}]}]),
                 {app, AppConfig} = lists:keyfind(app, 1, Config),
                 ?assertEqual({key2, value}, lists:keyfind(key2, 1, AppConfig)),
                 ?assertEqual({key, value}, lists:keyfind(key, 1, AppConfig))
             end)},
     
     {"update_config should not affect other applications",
      ?_test(begin
                 Config = rst_config:update(app, key, value, [{app2, [{key, value}]}]),
                 {app2, AppConfig} = lists:keyfind(app2, 1, Config),
                 ?assertEqual({key, value}, lists:keyfind(key, 1, AppConfig))
             end)},

     {"to_string should return an appropriate argument string",
      ?_test(begin
                 ?assertEqual(" -bitcask key 'value'", rst_config:to_string([{bitcask, [{key, value}]}]))
             end)}
    ].
