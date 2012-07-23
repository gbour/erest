
-module(erest_serve).
-export([init/3,handle/2]).

-include("http.hrl").

init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

% path_info = path MINUS prefix
handle(Req=#http_req{method='GET', path_info=[]}, State) ->
	Prefix = erest_config:lookup(prefix, ""),
	Schema = erest_schema:all(Prefix, erest_config:lookup(schema)),

	reply(Req, json, Schema).

reply(Req, Format, Content) ->
	RequestBridge = simple_bridge:make_request(cowboy_request_bridge, {Req, undefined}),

	Resp  = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),
	Resp1 = Resp:status_code(200),
	Resp2 = Resp1:header("Content-Type", content_type(Format)),
	Resp3 = Resp2:data(formating(Format, Content)),

	Resp3:build_response().


content_type(json) -> 
	"application/json".

formating(json, Content) ->
	jsx:encode(Content).

