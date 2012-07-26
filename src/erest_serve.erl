
-module(erest_serve).
-export([init/3,handle/2]).

-include("http.hrl").

init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

% path_info = path MINUS prefix
handle(Req=#http_req{method='GET', path_info=[]}, State) ->
	Prefix = erest_config:lookup(prefix, ""),
	Schema = erest_schema:all(Prefix, erest_resource:all()),

	reply(Req, 200, json, Schema);

%
% NOTE: Object is a binary. we MUST NOT convert it to an atom (for security reason)
%
handle(Req=#http_req{method='GET', path_info=[Object,<<"schema">>]}, State) ->
	case erest_resource:lookup(Object) of
		undefined ->
			reply(Req, 404, json, <<"resource ",Object/binary," not found">>);

		Resource  ->
			Prefix = erest_config:lookup(prefix, ""),
			reply(Req, 200, json, erest_schema:resource(Prefix, Resource))
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

