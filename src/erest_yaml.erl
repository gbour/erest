
%%	eReST, Erlang REST app
%%	Copyright (C) 2012, Guillaume Bour <guillaume@bour.cc>
%%
%%	This program is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU Affero General Public License as
%%	published by the Free Software Foundation, either version 3 of the
%%	License, or (at your option) any later version.
%%
%%	This program is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU Affero General Public License for more details.
%%
%%	You should have received a copy of the GNU Affero General Public License
%%	along with this program.  If not, see <http://www.gnu.org/licenses/>.

% @doc eReST resources yaml schema loader
-module(erest_yaml).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([load/1]).
-ifdef(debug).
	-export([parse_yaml/2, parse_resource/2, parse_field/2]).
-endif.


-include("erest_resource.hrl").


%% @doc Load resources from yaml file
%%
%% @params
%%   - File (string) : yaml source file
%%
%% @return
%%   - {ok, Resources} where Resource is a list of #resource{} record
%%   - {error, Reason} when fail to load/decode yaml file or parse resources description
%%
-spec load(string()) -> {ok, [#resource{}]}|{error, atom()}.
load(File) ->
	case yaml:load_file(File, [implicit_atoms]) of
		{ok, [Yaml]}    -> parse_yaml(Yaml,[]);
		{error, Reason} -> {error, Reason}
	end.

%%%
%%% Parse resource description file (yaml formatted)
%%%

%% @doc YAML to Resources, list of resources
%% @private
%%
%% Browse loaded yaml description to instanciate resources
%%
-spec parse_yaml([{atom(), any()}], [#resource{}]) -> {error, any()}|{ok, [#resource{}]}.
parse_yaml([], Acc) ->
	{ok, Acc};
parse_yaml([{Name, Desc}|T], Acc) when is_atom(Name) ->
	Resource = parse_resource(#resource{name=Name}, Desc),
	parse_yaml(T, [Resource|Acc]);
parse_yaml(Bad, _) ->
	{error, {parsing, 'bad-format', Bad}}.


%% @doc parse resource detail
%% @private
%%
-spec parse_resource(#resource{}, [{atom(), any()}]) -> #resource{}.
parse_resource(Resource, []) ->
	Resource;
parse_resource(Resource, [{key, Key}|T]) ->
	parse_resource(Resource#resource{key=Key}, T);
parse_resource(Resource, [{backend, Module}|T]) ->
	parse_resource(Resource#resource{backend=Module}, T);
parse_resource(Resource, [{fields, Fields}|T]) ->
	Fields2 = lists:map(
		fun({Name, Attrs}) ->
			{Name, parse_field(#field{name=Name}, Attrs)}
		end, Fields),
	parse_resource(Resource#resource{fields=Fields2}, T);
parse_resource(Resource, [{Key,_}]) ->
	{error, {parsing, 'unk-spec', Key}}.

%% @doc parse resource field
%% @private
%%
-spec parse_field(#field{}, [{atom, any()}]) -> #field{}.
parse_field(Field, []) ->
	Field;
% TYPE
% simple field type (integer, string, boolean) or other resource
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
% invalid type
parse_field(Field, [{type, Type}|T]) ->
	{error, {parsing, 'invalid-field-type', Type}};
% DEFAULT VALUE
parse_field(Field, [{default, Default}|T]) ->
	parse_field(Field#field{default=Default}, T);
% REQUIRED flag
parse_field(Field, [{required, Required}|T]) when is_boolean(Required) ->
	parse_field(Field#field{required=Required}, T);
parse_field(Field, [{required, Required}|T]) ->
	{error, {parsing, 'invalid-field-required', Required}};
% UNIQUE flag
parse_field(Field, [{unique, Unique}|T]) when is_boolean(Unique) ->
	parse_field(Field#field{unique=Unique}, T);
parse_field(Field, [{unique, Unique}|T]) ->
	{error, {parsing, 'invalid-field-unique', Unique}};
% DESCRIPTION
parse_field(Field, [{desc, Desc}|T]) when is_binary(Desc) ->
	parse_field(Field#field{desc=Desc}, T);
parse_field(Field, [{Key, _}|T]) ->
	{error, {parsing, 'unk-field', Key}}.
