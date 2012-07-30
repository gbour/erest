
-module(erest_serve).
-export([init/3,handle/2]).

-include("http.hrl").

init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

%
% global overview schema
% list all declared resources
%
% path_info = path MINUS prefix
handle(Req=#http_req{method='GET', path_info=[]}, State) ->
	Prefix = erest_config:lookup(prefix, ""),
	Schema = erest_schema:all(Prefix, erest_resource:all()),

	reply(Req, 200, json, Schema);

%
% Resource schema
% return schema of requested resource
%
% ie: GET http://server/foo/bar/schema returns bar resource schema
%
% NOTE: Object is a binary. we MUST NOT convert it to an atom (for security reason)
%
handle(Req=#http_req{method='GET', path_info=[Resource,<<"schema">>]}, State) ->
	case erest_resource:lookup(Resource) of
		undefined ->
			reply(Req, 404, json, <<"resource ",Resource/binary," not found">>);

		Schema  ->
			Prefix = erest_config:lookup(prefix, ""),
			reply(Req, 200, json, erest_schema:resource(Prefix, Schema))
	end;

%
%
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


reply(Req, Code, Format, Content) ->
	io:format(user,"content= ~p~n", [Content]),

	Resp  = simple_bridge:make_response(cowboy_response_bridge, {Req, undefined}),
	Resp1 = Resp:status_code(Code),
	Resp2 = Resp1:header("Content-Type", content_type(Format)),
	Resp3 = Resp2:data(formating(Format, Content)),

	Resp3:build_response().


content_type(json) -> 
	"application/json".

unformating(json, Raw)   ->
	jsx:decode(Raw).
formating(json, Content) ->
	jsx:encode(Content).

