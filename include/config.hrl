-define(CFG(Key), scl_cfg:get(?MODULE, Key)).

-define(CFG(Key, Default), scl_cfg:get(?MODULE, Key, Default)).

-define(CFG_EXISTS(Name, Error), scl_cfg:map_cfg(cfg:get(?MODULE, Name), Error)).

-define(CFG_EXISTS(Name), scl_cfg:map_cfg(scl_cfg:get(?MODULE, Name),
	?MODULE_STRING ++ ": no parameter '" ++ erlang:atom_to_list(Name) ++ "' in config.")).
