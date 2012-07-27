
CC      = erlc
BEAMDIR = $(shell pwd)/ebin
EFLAGS  = -I $(shell pwd)/src
EFLAGS += -I $(shell pwd)/deps/cowboy/include
EFLAGS += -I $(shell pwd)/deps/erlang-record_info/include
# NEEDED for parse/transform
EFLAGS += -pa $(shell pwd)/deps/erlang-record_info/ebin
EFLAGS += -I /usr/lib/erlang/lib

ifeq ($(MAKECMDGOALS),test)
	EFLAGS += -Ddebug -DTEST +debug_info
endif
export CC BEAMDIR EFLAGS

all: deps src tst

deps:
	@$(MAKE) -C deps/

src:
	@$(MAKE) -C src/

tst:
	@$(MAKE) -C test/

run:
	cd ebin/ && erl -sname epbxd \
		-pa ../deps/cowboy/ebin/ \
		-pa ../deps/simple_bridge/ebin/ \
		-pa ../deps/yamler/ebin/ \
		-pa ../deps/yamler/priv/ \
		-pa ../deps/jsx/ebin/ \
		-pa ../deps/erlang-record_info/ebin
	   	-eval 'erest:start(normal, [{prefix,"/foo/bar"},{server,cowboy},{schema,"../test/schema.yml"}]).'

clean:
	rm -f $(BEAMDIR)/*.beam

distclean: clean
	@$(MAKE) -C deps/ clean

.PHONY: deps src tsrc

