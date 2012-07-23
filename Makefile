
CC      = erlc
BEAMDIR = $(shell pwd)/ebin
EFLAGS  = -I $(shell pwd)/src
EFLAGS += -I $(shell pwd)/deps/cowboy/include
EFLAGS += -I /usr/lib/erlang/lib

ifeq ($(MAKECMDGOALS),test)
	EFLAGS += -Ddebug -DTEST +debug_info
endif
export CC BEAMDIR EFLAGS

all: deps src

deps:
	@$(MAKE) -C deps/

src:
	@$(MAKE) -C src/

run:
	cd ebin/ && erl -sname epbxd \
		-pa ../deps/cowboy/ebin/ \
		-pa ../deps/simple_bridge/ebin/ \
		-pa ../deps/yamler/ebin/ \
		-pa ../deps/yamler/priv/ \
	   	-eval 'erest:start(normal, [{prefix,"/foo/bar"},{server,cowboy},{schema,"./types.ws"}]).'

clean:
	rm -f $(BEAMDIR)/*.beam

distclean: clean
	@$(MAKE) -C deps/ clean

.PHONY: deps src tsrc

