
-module(utils).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([title/1, int/1, atom/1, str/1, bin/1, substr/2]).

title(<<H:1/binary, T/binary>>) ->
	<<(binstr:to_upper(H))/binary, (binstr:to_lower(T))/binary>>.

int(Val) when is_binary(Val) ->
	erlang:list_to_integer(erlang:binary_to_list(Val));
int(Val) when is_list(Val)   ->
	erlang:list_to_integer(Val).

atom(Val) when is_binary(Val) ->
	erlang:binary_to_atom(Val, latin1);
atom(Val) when is_list(Val)   ->
	erlang:list_to_atom(Val).

%
%
str(Val) when is_atom(Val)    ->
	erlang:atom_to_list(Val);
str(Val) when is_integer(Val) ->
	erlang:integer_to_list(Val);
str(Val) when is_binary(Val)  ->
	erlang:binary_to_list(Val).

bin(Val) when is_list(Val)   ->
	erlang:list_to_binary(Val);
bin(Val) when is_atom(Val)   ->
	erlang:atom_to_binary(Val, latin1).

substr(Val, Len) when is_binary(Val) andalso Len > 0 ->
	binary_part(Val, 0, Len);
substr(Val, Len) when is_binary(Val) andalso Len < 0 ->
	substr(Val, byte_size(Val)+Len).
