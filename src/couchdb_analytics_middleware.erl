-module(couchdb_analytics_middleware).
%% @doc A middlware for recording analytics data to CouchDB.
%%      Middleware must preceed beepbeep, but otherwise has no position requirements.
%% @author Will Larson <lethain@gmail.com> [lethain.com]

-export([run/2]).

%run(Ctx, App) ->
%    App(Ctx).
%%       request_key = referer | ip | user_agent | content_type | accept_language | domain | url | query_path
run(Ctx, App) ->
    Data = [{query_path, ewgi_api:query_string(Ctx)},
	    {domain, ewgi_api:server_name(Ctx)},
	    {ip, ewgi_api:remote_addr(Ctx)},
	    {uri, ewgi_api:path_info(Ctx)}
	    ],
    couchdb_analytics:record(Data),
    App(Ctx).
