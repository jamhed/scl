-module(scl_logger).
-include_lib("scl/include/config.hrl").
-export([info/4, error/4, warning/4, check_config/0]).

check_config() -> scl_cfg:validate([?CFG_EXISTS(log_modules), ?CFG_EXISTS(skip_modules)]).

get_app() ->
	case application:get_application() of
		{ok, App}-> atom_to_list(App);
		_ -> "_"
	end.

check_log(true, Module, Line, String, Args, Fun) -> log(Module, Line, String, Args, Fun);
check_log(false, _, _, _, _, _) -> skip.

member_of(_Module, []) -> true;
member_of(Module, List) -> lists:member(Module, List).

check(true, false) -> true;
check(_, _) -> false.

log_f(Module, Line, String, Args, Fun) ->
	Check = check(
		member_of(Module, ?CFG(log_modules, [])),
		lists:member(Module, ?CFG(skip_modules))
	),
	check_log(Check, Module, Line, String, Args, Fun).

log(Module, Line, InString, Args, Fun) ->
	String = 
		lists:concat([
			get_app(), "/", atom_to_list(Module), ".", integer_to_list(Line),
			" ", erlang:pid_to_list(self()), ": ", InString , "~n"
		]),
	error_logger:Fun(String, Args).

info(Module, Line, String, Args) -> log_f(Module, Line, String, Args, info_msg).

warning(Module, Line, String, Args) -> log_f(Module, Line, String, Args, warning_msg).

error(Module, Line, String, Args) -> log_f(Module, Line, String, Args, error_msg).
