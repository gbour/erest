
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

% @doc eReST resources store
-module(erest_resource).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([init/1, insert/1, lookup/1, all/0, get/2]).
-include("erest_resource.hrl").


%% @doc Initialize resources store
%%
%% @params
%%   - Opts (proplist) : store options
%%
%% @return
%%   -  ok             if success
%%   - {error, Reason} if fail
%%
-spec init([{atom(), any()}]) -> ok|{error, atom()}.
init(_Opts) ->
	case mnesia:create_table(erest_resources, [{ram_copies, [node()]}, {type, set}]) of
		{atomic, ok}                  -> ok;
		{aborted, {already_exists,_}} -> ok;
		{aborted, Reason}             -> {error, Reason}
	end.


%% @doc Insert new resource in store
%%
%% @params
%%   - resource (#resource{} record)
%%
%% @return
%%   - ok
%%   - {error, Reason}
%%
%% @note
%%   - Resource is stored as a Key/Value tuple where the key is resource name (binary string) and
%%	   the value is the resource itself
%%	 - If Key already defined in store, value is replaced with new resource
%%
-spec insert(#resource{}) -> ok|{error, atom()}.
insert(Resource=#resource{name=Name}) ->
	mnesia:activity(sync_transaction, fun() ->
		mnesia:write({erest_resources, utils:bin(Name), Resource})
	end).


%% @doc Lookup resource from store
%%
%% @params
%%   - resource name (binary string)
%%
%% @return
%%   - resource  if found (#resource{} record)
%%   - undefined otherwise
%%
-spec lookup(binary()) -> undefined|#resource{}.
lookup(Name) ->
	case mnesia:dirty_read(erest_resources, Name) of
		[{_, Name, Schema}] -> Schema;
		[]                  -> undefined
	end.


%% @doc Return all stored resources
%%
%% @return
%%   - all stored resources as a list of #resource{} records
%%
-spec all() -> [#resource{}].
all() ->
	mnesia:activity(async_dirty, fun() ->
		fill(mnesia:first(erest_resources),[])
	end).

%% @doc Extract all resources from store
%% @private
%%
-spec fill(atom, [#resource{}]) -> [#resource{}].
fill('$end_of_table', Acc) ->
	Acc;
fill(Previous, Acc)        ->
	fill(mnesia:next(erest_resources,Previous), [?MODULE:lookup(Previous)|Acc]).


%% @doc Get resource spec
%%
%% @params
%%   - name (binary string) : resource name
%%   - spec (atom)          : spec name
%%
%% @return
%%   - spec value
%%   - {error, no_resource} if resource does not exists
%%   - {error, no_spec}     if spec not found for resource
%%
-spec get(binary(), atom()) -> {error, atom()}|any().
get(Name, Spec) ->
	case ?MODULE:lookup(Name) of
		undefined -> {error, no_resource};
		Schema    -> get_spec(Spec, Schema)
	end.

%% @doc Get resource spec (subfunction)
%% @private
%%
-spec get_spec(atom(), #resource{}) -> undefined|backend.
get_spec(backend, #resource{backend=Backend}) ->
	Backend;
get_spec(_,_) ->
	{error, no_spec}.

