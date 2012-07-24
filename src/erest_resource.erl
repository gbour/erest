
-module(erest_resource).

-export([init/0, insert/1, lookup/1, all/0]).
-include("erest_resource.hrl").

init() ->
	ets:new(erest_resources, [set,named_table,public, {read_concurrency,true}]).

insert(Resource=#resource{name=Name}) ->
	ets:insert(erest_resources, {utils:bin(Name), Resource}).

lookup(Name) ->
	case ets:lookup(erest_resources, Name) of
		[{Name, Schema}] -> Schema;
		[]               -> undefined
	end.

all() ->
	fill(ets:first(erest_resources),[]).

fill('$end_of_table', Acc) ->
	Acc;
fill(Previous, Acc)        ->
	fill(ets:next(erest_resources,Previous), [erest_resource:lookup(Previous)|Acc]).

