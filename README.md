NAME
====

server_status

SYNOPSIS
========

    > application:start(server_status).
    ok
    > server_status_client:working([{path, <<"/index.html">>},
                                    {query_string, <<>>]).
    ok
    > server_status_client:done().
    ok
    > server_status_client:done_with(200).
    ok
    > server_status_client:state_dump().
    ...
    > io:put_chars(server_status_client:text_state_dump()).
    ...

GOAL
====

Works like Apache's mod_status.

DESCRIPTION
===========

This module visualize HTTP server status.

i want to see web server's status that shows what processes are working,
and how many processes are waiting.
