-module(scl_cfg).
-export([validate/1, get/2, get/3, map_cfg/2]).
-define(PATH, "cfg").
-define(ETS_NAME, cfg).

map_cfg(undefined, Error) -> {fail, Error};
map_cfg(Value, _Error) -> {ok, Value}.

validate(Props) ->
	case proplists:get_value(fail, Props) of
		undefined -> ok;
		_ -> erlang:error({config_error, Props})
	end.

ensure_ets_table() ->
	case ets:info(?ETS_NAME) of
		undefined -> ets:new(?ETS_NAME, [set, named_table, public]);
		_  -> ?ETS_NAME
	end.

handle_read_file(Module, {error, enoent}) -> error_logger:warning_msg("missing config for ~p", [Module]), [];
handle_read_file(_Module, {ok, [Cfg]}) -> Cfg;
handle_read_file(Module, _Err) -> error_logger:error_msg("error reading config for ~p", [Module]), [].

handle_module_cfg(Module, []) ->
	Path = filename:join(?PATH, Module),
	Cfg = handle_read_file(Module, file:consult(Path)),
	ets:insert(?ETS_NAME, {Module, Cfg}),
	Cfg;
handle_module_cfg(Module, [{Module, PropList}]) -> PropList.
ensure_module_cfg(Module) ->
	handle_module_cfg( Module, ets:lookup(?ETS_NAME, Module) ).

get(Module, Key) ->
	ensure_ets_table(),
	Cfg = ensure_module_cfg(Module),
	Value = proplists:get_value(Key, Cfg),
	Value.

get(Module, Key, Default) ->
	ensure_ets_table(),
	Cfg = ensure_module_cfg(Module),
	Value = proplists:get_value(Key, Cfg, Default),
	Value.
