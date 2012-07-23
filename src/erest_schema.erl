
-module(erest_schema).
-export([all/2]).

-include("erest_resource.hrl").

%
%
%
all(Prefix, Schema) ->
	all(utils:bin(Prefix), Schema, []).

all(_, [], Schema) ->
	Schema;
all(Prefix, [#resource{name=Name}|T], Schema) ->
	Name2 = utils:bin(Name),
	all(Prefix, T, [
		{Name, [
			{base  , <<Prefix/binary,"/",Name2/binary>>},
			{schema, <<Prefix/binary,"/",Name2/binary,"/schema">>}]
		}|Schema
	]).

