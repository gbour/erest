
-module(erest).
-behaviour(application).

-export([start/2,stop/1]).

start(normal, Args) ->
	init_storage(),

	erest_config:init([]),
	erest_resource:init([]),
	init(Args),

	ok.


stop(_State) ->
	mnesia:stop(),
	ok.


init_storage() ->
	Ret = case mnesia:create_schema([node()]) of
		ok                               -> ok;
		{error, {_, {already_exists,_}}} -> ok;
		{error, Err}                     -> 
			io:format(user, "fail to initialise mnesia: ~p~n", [Err]),
			{error, Err}
	end,

	mnesia:start(),
	Ret.


init([]) ->
	ok;
%
% i.e 
%	/ws
%	/path/to/tuc
init([{prefix, Prefix}|T]) ->
	erest_config:insert(prefix, Prefix),
	init(T);
% yaml resources description file
init([{schema, File}|T]) ->
	case erest_yaml:load(File) of
		{ok, Schema} ->
			[ erest_resource:insert(R) || R <- Schema],
			init(T);

		{error, Reason} ->
			io:format(user, "load yaml resource fail: ~p~n", [Reason])
	end;
init([{server, cowboy}|T]) ->
	init(T),
	Prefix = case erest_config:lookup(prefix) of
		undefined -> '_';
		P         ->
			%NOTE: we converting prefix from string to binary array
			%			/foo/bar -> [<<"foo">>,<<"bar">>]
			%      we also append '...' to allow all urls STARTING with prefix
			%
			lists:append(
				lists:map(fun(E) -> utils:bin(E) end, string:tokens(P, "/")),
				['...']
			)
	end,

	application:start(cowboy),
	% NOTE: we add '...' at end of prefix 
	Dispatch = [
		{'_', [{Prefix, erest_serve, []}]}
	],

	cowboy:start_listener(http, 10,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	);

init([{server, Any}|T])     ->
	io:format(user, "~p server not handled ~n", [Any]);
init([Any|T])              ->
	io:format(user, "unknown ~p option~n", [Any]).
