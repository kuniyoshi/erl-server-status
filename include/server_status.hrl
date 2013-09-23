-record(server_status,
        {pid :: pid(),
         state :: working | done,
         started_at :: erlang:timestamp(),
         ended_at :: erlang:timestamp(),
         wall_clock_us :: timer:now_diff(),
         code :: integer(),
         path :: binary(),
         query_string :: binary()}).
