-define(INFO(S, A), scl_logger:info(?MODULE, ?LINE, S, A)).
-define(ERR(S, A), scl_logger:error(?MODULE, ?LINE, S, A)).
-define(WARN(S, A), scl_logger:warning(?MODULE, ?LINE, S, A)).
