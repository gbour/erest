
-module(erest_yaml_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erest_resource.hrl").

file_not_found_test() ->
	?assertEqual({error, enoent}, erest_yaml:load("/foo/bar")).

invalid_yaml_test() ->
	{error, {scanner, Reason}} = erest_yaml:load("./test/data/invalid-schema.yml").

invalid_parse_yaml_test() ->
	{error, {parsing, 'bad-format', Bad}} = erest_yaml:parse_yaml("foobar",[]).

bad_resource_name_test() ->
	{error, {parsing, 'bad-format', Bad}} = erest_yaml:parse_yaml([{"foo",bar}],[]).

bad_resource_spec_test() ->
	?assertEqual(
		{error, {parsing, 'unk-spec', foo}},
	   	erest_yaml:parse_resource(#resource{}, [{foo,bar}])
	).

bad_resource_field_type_test() ->
	?assertEqual(
		{error, {parsing, 'invalid-field-type', <<"foobar">>}},
		erest_yaml:parse_field(#field{}, [{type, <<"foobar">>}])
	).

bad_resource_field_required_test() ->
	?assertEqual(
		{error, {parsing, 'invalid-field-required', plop}},
		erest_yaml:parse_field(#field{}, [{required, plop}])
	).

bad_resource_field_unique_test() ->
	?assertEqual(
		{error, {parsing, 'invalid-field-unique', plop}},
		erest_yaml:parse_field(#field{}, [{unique, plop}])
	).

bad_resource_field_test() ->
	?assertEqual(
		{error, {parsing, 'unk-field', foo}},
		erest_yaml:parse_field(#field{}, [{foo,bar}])
	).

valid_test() ->
	{ok, Resources} = erest_yaml:load("./test/data/valid-schema.yml"),
	% NOTE: we need sort fields proplist as we cannot guarantee the result order 
	Resources2 = lists:map(fun(R) ->
		R#resource{fields=lists:keysort(1, R#resource.fields)}
	end, Resources),

	?assertEqual(
		[#resource{
			name=people,
			key=nickname,
			fields=[
				{dograce, #field{
					name=dograce,
					type=string,
					default= <<"chihuahua">>,
					desc= <<"what is people dog race">>
				}},
				{nickname, #field{
					name=nickname,
					type=string,
					required=true,
					unique=true,
					desc= <<"people's nickname">>
				}},
				{put_in_jail, #field{
					name=put_in_jail,
					type=integer,
					default=0,
					desc= <<"how many times the people goes to prison">>
				}},
				{sextape, #field{
					name=sextape,
					type=boolean,
					default=false,
					desc= <<"has the people's sextape been exposed on internet">>
				}}
			]
		}],
		Resources2
	).
