NAME
====

server_status

SYNOPSIS
========

    > ok = application:start(server_status).
    > ok = server_status_client:working([{method, <<"GET">>},
                                         {path, <<"/index.html">>},
                                         {query_string, <<>>}]).
    > ok = server_status_client:done().
    > server_status_client:state_dump().
    ...
    > io:put_chars(server_status_client:text_state_dump()).
    SERVER STATUS
    =============

    Dumped at 2013-10-19 1:1:41

    SUMMARY
    =======

    - count:         1
    - mean:      19.07

    LIST PROCESSES
    ==============

    |----------+------------+----------------+--------+------+--------------|
    | PID      | Started at | Wall clock [s] | Method | Path | Query String |
    |----------+------------+----------------+--------+------+--------------|
    | <0.55.0> |  1: 1:22   |          19.07 | GET    | /foo | ?foo=bar     |
    |----------+------------+----------------+--------+------+--------------|

GOAL
====

Works like Apache's mod_status.

DESCRIPTION
===========

This module visualize HTTP server status.

i want to see web server's status that shows what processes are working.
