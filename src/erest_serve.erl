
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

	end.


reply(Req, Code, Format, Content) ->
	io:format(user,"content= ~p~n", [Content]),

	Resp  = simple_bridge:make_response(cowboy_response_bridge, {Req, undefined}),
	Resp1 = Resp:status_code(Code),
	Resp2 = Resp1:header("Content-Type", content_type(Format)),
	Resp3 = Resp2:data(formating(Format, Content)),

	Resp3:build_response().


content_type(json) -> 
	"application/json".

formating(json, Content) ->
	jsx:encode(Content).

