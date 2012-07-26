
-module(erest_schema).
-export([all/2, resource/2]).

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

%
%
%
resource(Prefix, Resource=#resource{fields=Fields}) ->
	[
		resource_key(Resource),
		{fields, lists:reverse(fields2schema(utils:bin(Prefix), Fields, []))}
	].

%
%
resource_key(#resource{key=undefined}) ->
	<<"">>;
resource_key(#resource{key=Key}) ->
	{key, utils:bin(Key)}.


fields2schema(_, [], Schema) ->
	Schema;
fields2schema(Prefix, [{Name, Field}|T], Schema) ->
	fields2schema(Prefix, T,
		[{Name, 
				% use record_info(fields, XX)
				lists:reverse(field2schema(Prefix, Field, record_info(fields, field), []))
		}|Schema
	]).

field2schema(_,_,[],Acc) ->
	Acc;
% NAME / skipped
field2schema(Prefix,Field,[name|T], Acc) ->
	field2schema(Prefix,Field,T,Acc);
% TYPE
field2schema(Prefix,Field=#field{type=Type},[type|T], Acc) when 
		Type =:= integer; Type =:= string; Type =:= boolean ->
	field2schema(Prefix,Field, T, [{type, utils:bin(Type)}|Acc]);
field2schema(Prefix,Field=#field{type=Type},[type|T], Acc) when is_atom(Type) ->
	Name = utils:bin(Type),
	field2schema(Prefix,Field, T, [
		{type, Name}|
		[{schema, <<Prefix/binary,"/",Name/binary,"/schema">>}|Acc]]
	);

field2schema(Prefix,Field=#field{type={list,Type}},[type|T], Acc) 
		when Type =:= integer; Type =:= string; Type =:= boolean ->
	field2schema(Prefix, Field, T, [
		{type, <<"list">>}|
		[{subtype, utils:bin(Type)}|Acc]
	]);

field2schema(Prefix,Field=#field{type={list,Type}},[type|T], Acc)  ->
	Name= utils:bin(Type),
	field2schema(Prefix,Field, T, [
		{type, <<"list">>}|
		[{subtype, Name}|
		[{schema, <<Prefix/binary,"/",Name/binary,"/schema">>}|Acc]]
	]);
% DEFAULT
field2schema(Prefix,Field=#field{default=undefined},[default|T], Acc) ->
	field2schema(Prefix,Field,T, [{default, null}|Acc]);
	%schema_field_(Field,T, [{default, erlang:atom_to_binary(Default,latin1)}|Acc]);
field2schema(Prefix,Field=#field{default=Default}, [default|T], Acc) when
		Default =:= now->
	field2schema(Prefix,Field,T,[{default, <<"now">>}|Acc]);
field2schema(Prefix,Field=#field{default=Default}, [default|T], Acc) ->
	field2schema(Prefix,Field,T,[{default, Default}|Acc]);
% REQUIRED	
field2schema(Prefix,Field=#field{required=Required},[required|T], Acc) ->
	field2schema(Prefix,Field, T, [{required, Required}|Acc]);
% UNIQUE
field2schema(Prefix,Field=#field{unique=Unique},[unique|T], Acc) ->
	field2schema(Prefix,Field, T, [{unique, Unique}|Acc]);
% DESCRIPTION
field2schema(Prefix,Field=#field{desc=undefined},[desc|T],Acc) ->
	field2schema(Prefix,Field, T, Acc);
field2schema(Prefix,Field=#field{desc=Desc},[desc|T],Acc) ->
	field2schema(Prefix,Field, T, [{desc, Desc}|Acc]).

