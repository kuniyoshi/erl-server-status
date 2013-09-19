NAME
====

server_status

SYNOPSIS
========

    > application:start(server_status).
    > server_status_client:working([{path, <<"/index.html">>},
                                    {query_string, <<>>]).
    > server_status_client:done().
    > server_status_client:state_dump().

GOAL
====

Works like Apache's mod_status.

DESCRIPTION
===========

This module visualize HTTP server status.

i want to see web server's status that shows what processes are working,
and how many processes are waiting.
