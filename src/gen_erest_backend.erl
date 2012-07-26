
-module(gen_erest_backend).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{list/2}, {insert/3}, {update/4}, {lookup/3}, {delete/3}];
behaviour_info(_) ->
	undefined.

