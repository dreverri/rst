-define(FAIL, throw({error, failed})).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), io:format("DEBUG: " ++ Str, Args)).
-define(INFO(Str, Args), io:format("INFO: " ++ Str, Args)).
-define(WARN(Str, Args), io:format("WARN: " ++ Str, Args)).
-define(ERROR(Str, Args), io:format("ERROR: " ++ Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

