
PATH    := $(PATH):$(shell pwd)/rebar
export PATH

all: rebar cowboy simple_bridge jsx yamler

rebar:
	if [ ! -d rebar ]; then\
		git clone https://github.com/basho/rebar.git;\
	fi
	cd rebar && git pull --rebase
	@$(MAKE) -C rebar/

cowboy:
	if [ ! -d cowboy ]; then\
		git clone https://github.com/extend/cowboy.git;\
	fi
	cd cowboy && git pull --rebase
	@$(MAKE) -C cowboy/

simple_bridge:
	if [ ! -d $@ ]; then\
		git clone https://github.com/choptastic/simple_bridge.git;\
	fi
	cd $@ && git pull --rebase
	@$(MAKE) -C $@/

jsx:
	if [ ! -d $@ ]; then\
		git clone https://github.com/talentdeficit/jsx.git;\
	fi
	cd $@ && git pull --rebase && ../rebar/rebar compile
	#@$(MAKE) -C $@/

yamler:
	if [ ! -d $@ ]; then\
		git clone https://github.com/goertzenator/yamler.git;\
	fi
	cd $@ && git pull --rebase && ../rebar/rebar compile
	#@$(MAKE) -C $@/

clean:
	find ./* -prune -type d -exec rm -Rf '{}' \;

.PHONY: rebar cowboy simple_bridge jsx yamler
