
-record(field, {
	name     = undefined  :: undefined|atom(),
	type     = undefined  :: undefined|atom(),
	default  = undefined  :: undefined|any(),
	required = true       :: boolean(),
	unique   = false      :: boolean(),
	desc     = undefined  :: undefined|binary()	
}).

-record(resource, {
	name     = undefined  :: undefined|atom(),
	record   = undefined  :: undefined|atom(),
	key      = undefined  :: undefined|atom()|list(atom()),

	fields   = []         :: list(#field{})
}).
