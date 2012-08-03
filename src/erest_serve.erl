
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

% @doc serve eReST resources
-module(erest_serve).
-author("Guillaume Bour <guillaume@bour.cc>").

%% 
%% NOTE: this module is currently COWBOY AND JSON SPECIFIC
%%       It has to be changed to be generic
%%

-export([init/3,handle/2]).

-include("http.hrl").

%% @doc Initialise module
%%
%%
-spec init(any(), any(), any()) -> {ok, any(), atom()}.
init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

%% @doc Handle REST queries
%%
%% NOTE: path_info = path MINUS prefix
%%
-spec handle(#http_req{}, any()) -> any().
%
% Query Resources global schema
%
% List all declared resources
%
handle(Req=#http_req{method='GET', path_info=[]}, State) ->
	Prefix = erest_config:lookup(prefix, ""),
	Schema = erest_schema:all(Prefix, erest_resource:all()),

	reply(Req, 200, json, Schema);

%
% Query Resource schema 
% return schema of requested resource
%
% ie: GET http://server/foo/bar/schema returns bar resource schema
%
% NOTE: Resource is a binary. we MUST NOT convert it to an atom (for security reason)
%
handle(Req=#http_req{method='GET', path_info=[Resource,<<"schema">>]}, State) ->
	case erest_resource:lookup(Resource) of
		undefined ->
			reply(Req, 404, json, <<"resource ",Resource/binary," not found">>);

		Schema  ->
			Prefix = erest_config:lookup(prefix, ""),
			reply(Req, 200, json, erest_schema:resource(Prefix, Schema))
	end;

% Query Resource Items
%
% NOTE: no pagination, returns all items
%
handle(Req=#http_req{method='GET', path_info=[Resource]}, State) ->
	case erest_resource:get(Resource, backend) of
		resource_not_found ->
			reply(Req, 404, json, <<"resource ",Resource/binary," not found">>);
		spec_not_found     ->
			% MUST not append
			% TODO: dump a stacktrace
			reply(Req, 500, json, <<"">>);
		Module  ->
			% NOTE: here we know resource exists, we can convert resource name to an
			% atom safely
			%
			% TODO: catch exception (Module a/o Fun not found
			reply(Req, 200, json, 
				Module:list(utils:atom(Resource), [{prefix,	erest_config:lookup(prefix,"")}])
			)

	end;

% Query Resource Items
%
handle(Req=#http_req{method='GET', path_info=[Resource, Id]}, State) ->
	{Code, Payload} = case erest_resource:get(Resource, backend) of
		resource_not_found ->
			{404, <<"resource ",Resource/binary," not found">>};
		spec_not_found     ->
			% MUST not append
			% TODO: dump a stacktrace
			{500, <<"">>};
		Module             ->
			case Module:lookup(utils:atom(Resource), Id, []) of
				undefined  ->
					{404, <<"resource ",Resource/binary,"/",Id/binary," not found">>};
				Data       ->
					{200, Data}
			end
	end,

	reply(Req, Code, json, Payload);

% Put new resource Item
%
%
handle(Req=#http_req{method='PUT', path_info=[Resource]}, State) ->
	handle(Req#http_request{method='POST'}, State);
handle(Req=#http_req{method='POST', path_info=[Resource], buffer=Body}, State) ->
	{Code, Payload} = case erest_resource:get(Resource, backend) of
		resource_not_found ->
			{404, <<"resource ",Resource/binary," not found">>};
		spec_not_found     ->
			{500, <<"">>};
		Module             ->
			case Module:insert(utils:atom(Resource), unformating(json, Body), []) of
				{error, found} ->
					{409, <<"resource already set">>};
				{ok, Id}       ->
					{200, utils:bin(Id)}
			end
	end,

	reply(Req, Code, json, Payload);

% Update existing Resource Item
%
%
handle(Req=#http_req{method='PUT', path_info=[Resource, Id]}, State) ->
	handle(Req#http_request{method='POST'}, State);
handle(Req=#http_req{method='POST', path_info=[Resource, Id], buffer=Body}, State) ->
	{Code, Payload} = case erest_resource:get(Resource, backend) of
		resource_not_found ->
			{404, <<"resource ",Resource/binary," not found">>};
		spec_not_found     ->
			{500, <<"">>};
		Module             ->
			case Module:update(utils:atom(Resource), utils:int(Id), unformating(json, Body), []) of
				{error, 'not-found'} ->
					{404, <<"resource ",Resource/binary,"/",Id/binary," not found">>};
				{error, fail} ->
					{409, <<"fail to update record">>};
				{ok, Id2}       ->
					{200, Id2}
			end
	end,

	reply(Req, Code, json, Payload);

% Delete Resource Item
%
%
handle(Req=#http_req{method='DELETE', path_info=[Resource, Id]}, State) ->
	{Code, Payload} = case erest_resource:get(Resource, backend) of
		resource_not_found ->
			{404, <<"resource ",Resource/binary," not found">>};
		spec_not_found     ->
			% MUST not append
			% TODO: dump a stacktrace
			{500, <<"">>};
		Module             ->
			case Module:delete(utils:atom(Resource), utils:int(Id), []) of
				{error, fail} ->
					{404, <<"resource ",Resource/binary,"/",Id/binary," not found">>};
				ok            ->
					{200, Id}
			end
	end,

	reply(Req, Code, json, Payload).


%% @doc Format response
%%
%%
-spec reply(#http_req{}, integer(), atom(), any()) -> any().
reply(Req, Code, Format, Content) ->
	Resp  = simple_bridge:make_response(cowboy_response_bridge, {Req, undefined}),
	Resp1 = Resp:status_code(Code),
	Resp2 = Resp1:header("Content-Type", content_type(Format)),
	Resp3 = Resp2:data(formating(Format, Content)),

	Resp3:build_response().


%% @doc formatters content-type
%% @private
%%
-spec content_type(atom()) -> string().
content_type(json) -> 
	"application/json".


%% @doc Unformat data
%% @private
%% 
%% Convert received binary string (encoded with formatter) to "instanciated object"
%%
-spec unformating(atom(), binary()) -> any().
unformating(json, Raw)   ->
	jsx:decode(Raw).


%% @doc format data
%%
%% Convert "instanciated object" to binary string using formatter
%%
-spec formating(atom(), any()) -> binary()
formating(json, Content) ->
	jsx:encode(Content).

