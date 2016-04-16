-define(VERSION, <<"3">>).

-define(DEFAULT_FILES, ["src", "c_src", "include", "rebar.config.script"
                       ,"priv", "rebar.config", "rebar.lock"
                       ,"README*", "readme*"
                       ,"LICENSE*", "license*"
                       ,"NOTICE"]).

-define(CHUNK, 10000).

-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).
