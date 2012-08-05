
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

% @doc construct REST resources schemas
-module(erest_schema).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-export([all/2, resource/2]).
% DEBUG
-ifdef(debug).
	-export([resource_key/1, fields2schema/3, field2schema/4]).
-endif.

-include("erest_resource.hrl").


%% @doc Build "printable" schema of resources index
%% @params
%%   prefix (binary)                   : eReST URI prefix
%%   schema (list of resource records) : schema of resources loaded in eReST
%%
%% @return
%%   ready-to-format index of resources
%%
%% @example
%%   if URI prefix is <<"/foo/bar">> and we have two resources 'users' and 'groups', it returns:
%%  
%%     [{users,
%%	      {base, <<"/foo/bar/users">>},
%%	      {schema, <</foo/bar/users/schema">>}
%%	    },
%%	    {groups,
%%	      {base, <<"/foo/bar/groups">>},
%%	      {schema, <</foo/bar/groups/schema">>}
%%	   }] 
%%
-spec all(binary(), [#resource{}]) -> any().
all(Prefix, Schema) ->
	all(utils:bin(Prefix), Schema, []).

% @private
% build index schema
-spec all(binary(), [#resource{}], [any()]) -> [any()].
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


%% @doc Build "printable" resource schema
%%
%% @params
%%   prefix (atom)          : eReST URI prefix
%%   resource (#resource{}) : Resource schema
%%
%% @return
%%   resource schema "printable" representation
%%
%-spec resource(atom(), #resource{}) -> [binary()|{key,binary()}, {fields, list(any())}].
resource(Prefix, Resource=#resource{fields=Fields}) ->
	[
		resource_key(Resource),
		{fields, lists:reverse(fields2schema(utils:bin(Prefix), Fields, []))}
	].

%% @doc format resource key
%% @private
%%
-spec resource_key(#resource{}) -> binary()|{key, binary()}.
resource_key(#resource{key=undefined}) ->
	<<"">>;
resource_key(#resource{key=Key}) ->
	{key, utils:bin(Key)}.


%% @doc format Resource fields schema
%% @private
%%
%% @params
%%   prefix (atom)          : eReST URI prefix
%%   fields (proplist of name,#resource{}) : Resource schema
%%
%% @return
%% 
-spec fields2schema(atom(), [{atom(), #field{}}], [{atom(), any()}]) -> [{atom(), any()}].
fields2schema(_, [], Schema) ->
	Schema;
fields2schema(Prefix, [{Name, Field}|T], Schema) ->
	fields2schema(Prefix, T,
		[{Name, 
				% use record_info(fields, XX)
				lists:reverse(field2schema(Prefix, Field, record_info(fields, field), []))
		}|Schema
	]).

-spec field2schema(atom(), #field{}, [atom()], [{atom(), any()}]) -> [{atom(), any()}].
field2schema(_,_,[],Acc) ->
	Acc;
% *name* field: skipped as already set as proplist key above
field2schema(Prefix,Field,[name|T], Acc) ->
	field2schema(Prefix,Field,T,Acc);
% *type* field: integer, string, boolean
field2schema(Prefix,Field=#field{type=Type},[type|T], Acc) when 
		Type =:= integer; Type =:= string; Type =:= boolean ->
	field2schema(Prefix,Field, T, [{type, utils:bin(Type)}|Acc]);
% *type* field: another resource
% return:
%   [{type, <<"groups">>}, {schema, <<"/foo/bar/groups/schema">>}]
field2schema(Prefix,Field=#field{type=Type},[type|T], Acc) when is_atom(Type) ->
	Name = utils:bin(Type),
	field2schema(Prefix,Field, T, [
		{type, Name}|
		[{schema, <<Prefix/binary,"/",Name/binary,"/schema">>}|Acc]]
	);
% *type* field : list of integer, string or boolean
% return:
%   [{type, <<"list">>}, {subtype, <<"string">>}]
field2schema(Prefix,Field=#field{type={list,Type}},[type|T], Acc) 
		when Type =:= integer; Type =:= string; Type =:= boolean ->
	field2schema(Prefix, Field, T, [
		{type, <<"list">>}|
		[{subtype, utils:bin(Type)}|Acc]
	]);
% *type* field: list of another resource
% return:
%	[{type, <<"list"},
%	 {subtype, <<"groups">>},
%	 {schema, <<"/foo/bar/groups/schema">>}
%	]
field2schema(Prefix,Field=#field{type={list,Type}},[type|T], Acc)  ->
	Name= utils:bin(Type),
	field2schema(Prefix,Field, T, [
		{type, <<"list">>}|
		[{subtype, Name}|
		[{schema, <<Prefix/binary,"/",Name/binary,"/schema">>}|Acc]]
	]);
% *default* field: ignored if undefined
%
field2schema(Prefix,Field=#field{default=undefined},[default|T], Acc) ->
	field2schema(Prefix,Field,T, [{default, null}|Acc]);
	%schema_field_(Field,T, [{default, erlang:atom_to_binary(Default,latin1)}|Acc]);
%field2schema(Prefix,Field=#field{default=Default}, [default|T], Acc) when
%		Default =:= now->
%	field2schema(Prefix,Field,T,[{default, <<"now">>}|Acc]);
% *default* field
field2schema(Prefix,Field=#field{default=Default}, [default|T], Acc) ->
	field2schema(Prefix,Field,T,[{default, Default}|Acc]);
% *required* field
field2schema(Prefix,Field=#field{required=Required},[required|T], Acc) ->
	field2schema(Prefix,Field, T, [{required, Required}|Acc]);
% *unique* field
field2schema(Prefix,Field=#field{unique=Unique},[unique|T], Acc) ->
	field2schema(Prefix,Field, T, [{unique, Unique}|Acc]);
% *desc* field : ignored when undefined
field2schema(Prefix,Field=#field{desc=undefined},[desc|T],Acc) ->
	field2schema(Prefix,Field, T, Acc);
field2schema(Prefix,Field=#field{desc=Desc},[desc|T],Acc) ->
	field2schema(Prefix,Field, T, [{desc, Desc}|Acc]).

