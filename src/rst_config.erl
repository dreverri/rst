-module(rst_config).

-export([
         new/1,
         new/2,
         merge/3,
         update/4,
         to_string/1
         ]).

-ifdef(TEST).
-compile(export_all).
-endif.

new(Number) ->
    H = (P = (W = 7000 + (Number - 1) * 3) + 1) + 1,
    new(integer_to_list(Number), 
        [
         {riak_core, [{web_port, W}, {handoff_port, H}]},
         {riak_kv, [{pb_port, P}]}
        ]).

%% Return config for node with Name
%% Config values updated from Default with Config
new(Name, Config) when is_atom(Name) ->
    new(atom_to_list(Name), Config);

new(Name, Config) when is_list(Name) andalso is_list(Config) ->
    Default = [
               {riak_core, [
                            {cluster_name, "default"},
                            {ring_state_dir, filename:join(["tmp", Name, "data", "ring"])},
                            {web_ip, "127.0.0.1"},
                            {web_port, 8098},
                            {handoff_port, 8099}
                           ]},
               {riak_kv, [
                          {storage_backend, riak_kv_bitcask_backend},
                          {pb_ip, "127.0.0.1"},
                          {pb_port, 8087}
                         ]},
               {bitcask, [
                          {data_root, filename:join(["tmp", Name, "data", "bitcask"])}
                         ]},
               {sasl, [
                       {sasl_error_logger, {file, filename:join(["tmp", Name, "log", "sasl-error.log"])}},
                       {errlog_type, error},
                       {error_logger_mf_dir, filename:join(["tmp", Name, "log", "sasl"])},
                       {error_logger_mf_maxbytes, 10485760},
                       {error_logger_mf_maxfiles, 5}
                      ]}
              ],
    merge(Config, Default, 2).
    
%% Recursively merge Configs to specified Depth
merge([], Config, _) ->
    Config;
merge([{Key, Value}|Updates], Config, 1) ->
    %% Insert Value in Config at Key
    Updated = replace_key(Key, Value, Config),
    
    %% Continue with remaining Updates
    merge(Updates, Updated, 1);
merge([{Key, Value}|Updates], Config, Depth) when is_integer(Depth) andalso Depth > 0 ->
    %% Grab the SubConfig at Key
    SubConfig = get_sub_config(Key, Config),
    
    %% Update the SubConfig with Value
    SubUpdated = merge(Value, SubConfig, Depth-1),
    
    %% Update Config with SubUpdated at Key
    Updated = replace_key(Key, SubUpdated, Config),
    
    %% Continue with remaining Updates
    merge(Updates, Updated, Depth).

%% return value or empty list
get_sub_config(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        false -> [];
        {Key, Value} -> Value
    end.

replace_key(Key, Value, Config) ->
    lists:keystore(Key, 1, Config, {Key, Value}).
  
%% update single App within Config with Value at Key  
update(App, Key, Val, Config) ->
    merge([{App, [{Key, Val}]}], Config, 2).

    
%% Convert an application config proplist to a string - useful 
%% for passing application variables to slave:start_link/3 
%% 
%% Currently only works for atom, integer, and list values
to_string(Config) ->
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
