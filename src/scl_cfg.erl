-module(scl_cfg).
-include_lib("scl/include/logger.hrl").
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

handle_read_file(Module, {error, enoent}) -> erlang:error({no_config_file_for_module, Module, file:get_cwd()});
handle_read_file(_Module, {ok, [Cfg]}) -> Cfg;
handle_read_file(_Module, Err) -> erlang:error({config_file_read_error, Err}).

handle_module_cfg(Module, []) ->
	Path = filename:join("cfg", Module),
	Cfg = handle_read_file(Module, file:consult(Path)),
	ets:insert(?ETS_NAME, {Module, Cfg}),
	?INFO("Loaded config for module:~p, path:~p, data:~180p", [Module, Path, Cfg]),
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
