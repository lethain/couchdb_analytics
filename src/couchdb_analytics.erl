-module(couchdb_analytics).
%% @author Will Larson <lethain@example.com> [http://lethain.com]
%% @version 0.0.1
%% @doc Project for storing analytics data in CouchDB.
%%
%% @reference Depends on <a href="http://github.com/ngerakines/erlang_couchdb">erlang_couchdb</a>.

-include("couchdb_analytics.hrl").
-export([retrieve/2, record/1, design/0, set_up/0, tear_down/0, info/0, reset/0]).

%% @doc query against CouchDB view.
%%      An example: <code>query(?TITLE_VIEW, [{key, <<"\"my-doc\"">>}, {limit, 1}])</code>
retrieve(View, Opts) ->
   erlang_couchdb:invoke_view(?COUCHDB_CONN, ?PROJECT_DB, ?DESIGN_DOC, View, Opts).

get_timestamp() ->    
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(erlang:now()) ) -	
	calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

%% @doc Create a new analytic entry.
%% @spec record(Opts:request_proplist()) -> term()
%%       request_proplist = [request_property()]
%%       request_property = {request_key(), request_value()}
%%       request_value = string() | binary() | number()
%%       request_key = referer | ip | user_agent | content_type | accept_language | domain | url | query_path
record(Opts) ->
    Opts1 = lists:map(fun({Key,X}) ->
			      case is_list(X) of
				  true ->
				      {Key, list_to_binary(X)};
				  false ->
				      {Key, X}
			      end end, Opts),
    Opts2 = [{"time", get_timestamp()} | Opts1],
    erlang_couchdb:create_document(?COUCHDB_CONN, ?PROJECT_DB, Opts2).

%% @doc return the views which make up the Pitance design document.
design() ->
   % format is {Name, MapFunction} | {Name, MapFunction, ReduceFunction}
   [{?TIME_VIEW, <<"function(doc) { var time = new Date(doc.time*1000); emit([doc.domain, time.getYear(), time.getMonth(), time.getHours(), time.getMinutes()], 1);}">>, <<"function(keys, values) { return sum(values); }">>},
    {?URI_VIEW, <<"function(doc) { emit(Array.concat([doc.domain],doc.uri.split(\"/\").filter(function(x) { return (x != \"\"); })), 1); }">>, <<"function(keys, values) { return sum(values); }">>},
    {?DOMAIN_VIEW, <<"function(doc) { emit(doc.domain, 1);}">>, <<"function(keys, values) { return sum(values); }">>}
   ].

%% @doc create databases and design views for CouchDB project.
set_up() ->
    erlang_couchdb:create_database(?COUCHDB_CONN, ?PROJECT_DB),
    erlang_couchdb:create_view(?COUCHDB_CONN, ?PROJECT_DB,
                               ?DESIGN_DOC, ?VIEW_LANG,
                               design()).

%% @doc delete the Pitance database                   
tear_down() ->
    erlang_couchdb:delete_database(?COUCHDB_CONN, ?PROJECT_DB).

%% @doc retrieve the current status of the database.
info() ->
    erlang_couchdb:database_info(?COUCHDB_CONN, ?PROJECT_DB).

%% @doc delete and then recreate the database.
reset() ->
    tear_down(),
    set_up().

