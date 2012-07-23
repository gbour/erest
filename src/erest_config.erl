
-module(erest_config).

-export([init/0, insert/2, lookup/1, lookup/2]).

init() ->
	ets:new(erest_config, [set, named_table, public, {read_concurrency, true}]).

insert(Key, Value) ->
	ets:insert(erest_config, {Key, Value}).
%
lookup(Key) ->
	lookup(Key, undefined).
lookup(Key, Default) ->
	case ets:lookup(erest_config, Key) of
		[{Key, Value}] -> Value;
		[]             -> Default
	end.

