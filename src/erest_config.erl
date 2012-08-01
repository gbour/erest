 
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

% @doc Access eReST configuration parameters (get/set)
-module(erest_config).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([init/1, insert/2, lookup/1, lookup/2]).


%% @doc Initialize configuration store
%%
%% @params
%%   - Opts: proplist of Key/Value options
%%
%% @return
%%   - ok              if success
%%   - {error, Reason} if fails
%%
-spec init([{atom(), any()}]) -> ok|{error, atom()}.
init(_Opts) ->
	case mnesia:create_table(erest_config, [{ram_copies, [node()]}, {type, set}]) of
		{atomic, ok}                  -> ok;
		{aborted, {already_exists,_}} -> ok;
		{aborted, Reason}             -> {error, Reason}
	end.


%% @doc Insert a new configuration Key/Value couple in configuration store
%%
%% NOTE: if Key already set, stored value is updated to new value
%%
-spec insert(atom(), any()) -> ok|{error, atom()}.
insert(Key, Value) ->
	mnesia:activity(sync_transaction, fun() -> mnesia:write({erest_config, Key,Value}) end).


%% @doc Lookup configuration key from store
%%
%% @params
%%   - configuration key (atom)
%% @return
%%   - associated value if found
%%   - undefined either
%%
-spec lookup(atom()) -> undefined|any().
lookup(Key) ->
	lookup(Key, undefined).

%% @doc Lookup configuration key from store, with default value
%%
%% @return
%%   - associated stored value if found
%%   - default value either
-spec lookup(atom(), any()) -> any().
lookup(Key, Default) ->
	case mnesia:dirty_read(erest_config, Key) of
		[{_,Key, Value}] -> Value;
		[]               -> Default
	end.

