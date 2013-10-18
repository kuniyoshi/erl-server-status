-record(server_status,
        {pid :: pid(),
         started_at :: erlang:timestamp(),
         wall_clock_us :: timer:now_diff(),
         method :: binary(),
         path :: binary(),
         query_string :: binary()}).
