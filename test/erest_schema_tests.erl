
-module(erest_schema_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erest_resource.hrl").

field2schema_name_test() ->
	% name is ignored
	?assertEqual([], erest_schema:field2schema(u,u,[name],[])).

field2schema_type_test() ->
	% simple types
	?assertEqual([{type, <<"integer">>}], erest_schema:field2schema('',#field{type=integer},[type],[])),
	?assertEqual([{type, <<"string">>}], erest_schema:field2schema('',#field{type=string},[type],[])),
	?assertEqual([{type, <<"boolean">>}], erest_schema:field2schema('',#field{type=boolean},[type],[])),

	% Resource type
	?assertEqual(
		[{type, <<"groups">>},{schema, <<"/foo/bar/groups/schema">>}], 
		erest_schema:field2schema(<<"/foo/bar">>, #field{type=groups},[type],[])
	),

	% list type
	?assertEqual([{type,<<"list">>},{subtype, <<"integer">>}], erest_schema:field2schema('', #field{type={list,integer}},[type],[])),
	?assertEqual([{type,<<"list">>},{subtype, <<"string">>}] , erest_schema:field2schema('', #field{type={list,string}} ,[type],[])),
	?assertEqual([{type,<<"list">>},{subtype, <<"boolean">>}], erest_schema:field2schema('', #field{type={list,boolean}},[type],[])),

	?assertEqual(
		[{type,<<"list">>},{subtype, <<"groups">>},{schema, <<"/foo/bar/groups/schema">>}], 
		erest_schema:field2schema(<<"/foo/bar">>, #field{type={list,groups}},[type],[])
	),
	ok.

field2schema_default_test() ->
	?assertEqual([{default, null}], erest_schema:field2schema('', #field{default=undefined}, [default], [])),
	?assertEqual([{default, 42}], erest_schema:field2schema('', #field{default=42}, [default], [])),
	?assertEqual([{default, "foobar"}], erest_schema:field2schema('', #field{default="foobar"}, [default], [])),

	ok.

field2schema_required_test() ->
	?assertEqual([{required, true}], erest_schema:field2schema('', #field{required=true}, [required], [])),
	?assertEqual([{required, false}], erest_schema:field2schema('', #field{required=false}, [required], [])),
	ok.

field2schema_unique_test() ->
	?assertEqual([{unique, true}], erest_schema:field2schema('', #field{unique=true}, [unique], [])),
	?assertEqual([{unique, false}], erest_schema:field2schema('', #field{unique=false}, [unique], [])),
	ok.

field2schema_desc_test() ->
	?assertEqual([], erest_schema:field2schema('', #field{desc=undefined}, [desc], [])),
	?assertEqual([{desc, "blablabla"}], erest_schema:field2schema('', #field{desc="blablabla"}, [desc], [])),
	ok.

fields2schema_test() ->
	?assertEqual(
		[{count, [{type,<<"integer">>},{default,10},{required,true},{unique,false},{desc,"test"}]}],
		erest_schema:fields2schema(<<"/foo/bar">>, [{count,#field{name=count,type=integer,default=10,required=true,desc="test"}}], [])
	).
