-module(bucket_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(bucket).


start(_StartType, _StartArgs) ->
    bucket_sup:start_link().

stop(_State) ->
    ok.

start_web() ->
    Dispatch = cowboy_router:compile([
        %% {URIHost, list({URIPath, Handler, Opts})}
        {'_', [{"/atom/:zip", atom_handler,  []},
               {"/stats",     stats_handler, []},
               {"/",          index_handler, []},
               {"/[...]", cowboy_static, [
                    {directory, "priv/www"},
                    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
               ]}
        ]}
    ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(my_http_listener, 100,
        [{port, 8888}],
        [{env, [{dispatch, Dispatch}]}]
    ).

