
-module(erest_yaml).
-export([load/1]).

-include("erest_resource.hrl").

load(File) ->
	{ok, [Yaml]} = yaml:load_file(File, [implicit_atoms]),
	Resources = parse_yaml(Yaml,[]),

	{ok, Resources}.

%%%
%%% Parse resource description file (yaml formatted)
%%%

%
% Resource parsing
%
% [{user, {fields...}]
%
parse_yaml([], Acc) ->
	Acc;
parse_yaml([{Name, Desc}|T], Acc) ->
	Resource = parse_resource(#resource{name=Name}, Desc),
	parse_yaml(T, [Resource|Acc]).

%
% Parse resource detail
%
parse_resource(Resource, []) ->
	Resource;
parse_resource(Resource, [{key, Key}|T]) ->
	parse_resource(Resource#resource{key=Key}, T);
parse_resource(Resource, [{fields, Fields}|T]) ->
	Fields2 = lists:map(
		fun({Name, Attrs}) ->
			{Name, parse_field(#field{name=Name}, Attrs)}
		end, Fields),
	parse_resource(Resource#resource{fields=Fields2}, T).

%
% Parse field
%
parse_field(Field, []) ->
	Field;
% TYPE
% simple field type (integer, string, boolean)
parse_field(Field, [{type, Type}|T]) when is_atom(Type) ->
	parse_field(Field#field{type=Type}, T);
% list-of-something
parse_field(Field, [{type, <<"list(",Rest/binary>>}|T]) ->
	Type = {list, utils:atom(utils:substr(Rest, -1))},
	parse_field(Field#field{type=Type}, T);
% dict-of-something (keys are binary)
parse_field(Field, [{type, <<"dict(",Rest/binary>>}|T]) ->
	Type = {dict, utils:atom(utils:substr(Rest, -1))},
	parse_field(Field#field{type=Type}, T);
% DEFAULT VALUE
parse_field(Field, [{default, Default}|T]) ->
	parse_field(Field#field{default=Default}, T);
% REQUIRED flag
parse_field(Field, [{required, Required}|T]) when is_boolean(Required) ->
	parse_field(Field#field{required=Required}, T);
% UNIQUE flag
parse_field(Field, [{unique, Unique}|T]) when is_boolean(Unique) ->
	parse_field(Field#field{unique=Unique}, T);
% DESCRIPTION
parse_field(Field, [{desc, Desc}|T]) when is_binary(Desc) ->
	parse_field(Field#field{desc=Desc}, T).
	
