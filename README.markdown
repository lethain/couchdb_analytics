
# CouchDB-Analytics

[couchdb]: http://couchdb.apache.org/ "CouchDB Project"
[cdb-analytics]: http://github.com/lethain/couchdb_analytics "couchdb-analytics on GitHub"

[CouchDB-Analytics][cdb-analytics] is a tool for storing records of and querying against
stored records of web application traffic.

The primary goals are:

1.  Real-time reports.
2.  Define standard reports, and users can easily create more
    specialized reports by creating custom views.
3.  Don't impose any meaningful delay on page rendering.
4.  Implement reporting without reliance on JavaScript
    or serving invisible graphics.

## Dependencies

1. [CouchDB][couchdb] is most certainly a dependency. ;)
2. [erlang_couchdb](http://github.com/ngerakines/erlang_couchdb) is used for interfacing with CouchDB.
3. [mochiweb](http://code.google.com/p/mochiweb/)'s ``mochijson:encode/1`` and mochijson:decode/1``.

## Integration

1.  The first integration point will be as a [BeepBeep](http://github.com/lethain/beepbeep)
    middleware which will catch analytic data.
