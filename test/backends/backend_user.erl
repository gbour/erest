
-module(backend_user).
-export([init/0,list/2,insert/3,update/4,lookup/3, delete/3]).

-record(user, {
	uid    :: integer(),
	name   :: binary(),
	groups :: list(integer())
}).

-define(ETS_NAME, demo_users).

init() ->
	ets:new(?ETS_NAME, [set, named_table, {keypos, #user.uid}, public]),

	ets:insert(?ETS_NAME, #user{uid=1  ,name= <<"root">> ,groups=[]}),
	ets:insert(?ETS_NAME, #user{uid=100,name= <<"gbour">>,groups=[]}),
	ets:insert(?ETS_NAME, #user{uid=101,name= <<"jdoe">> ,groups=[]}),

	ok.

list(user, _Opts) ->
	lists:map(fun([Id]) -> [{uid, Id}] end,
		ets:match(?ETS_NAME, #user{uid='$1',_='_'})
	).

lookup(user, Id, Opts) ->
	case ets:lookup(?ETS_NAME, Id) of
		[]    -> undefined;
		[Res] -> Res
	end.

insert(user, User, Opts) ->
	Ret = ets:insert_new(?ETS_NAME, #user{
		uid   = proplists:get_value(uid   , User),
		name  = proplists:get_value(name  , User),
		groups= proplists:get_value(groups, User)
	}),

	case Ret of
		true  -> {ok   , proplists:get_value(uid, User)};
		false -> {error, found}
	end.

update(user, Id, User, Opts) ->
	Ret = case ets:lookup(?ETS_NAME, Id) of
		[] -> {error, 'not-found'};
		_  ->
			Ret2 = ets:insert(?ETS_NAME, #user{
				uid   = Id,
				name  = proplists:get_value(name  , User),
				groups= proplists:get_value(groups, User)
			}),

			case Ret2 of
				true        -> {ok   , Id};
				false       -> {error, fail}
			end
	end.

delete(user, Id, Opts) ->
	%NOTE: delete() always return true!
	case ets:delete(?ETS_NAME, Id) of
		true  -> ok;
		false -> {error, fail}
	end.

% ets:match_object(?ETS_NAME, {user,'_',<<"gbour">>,'_'}). 
%
