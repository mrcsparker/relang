PROJECT=relang
REBAR=$(shell which rebar || echo ./rebar)
VERSION=0.1.1

.PHONY: all docker
all: get-deps compile repl

get-deps: $(REBAR)
	@$(REBAR) get-deps

compile: $(REBAR)
	@$(REBAR) co

test: eunit

eunit: $(REBAR)
	@$(REBAR) eu skip_deps=true

docker:
	docker build -t kureikain/relang:$(VERSION) .
push:
	docker push kureikain/relang:$(VERSION)

wercker:
	wercker build --docker-host tcp://192.168.59.103:2376

repl:
	erl -pa ebin -pa deps/protobuffs/ebin deps/jsx/ebin

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = .$(PROJECT).plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) deps ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) deps ebin

dialyzer: compile
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin

# xref

xref:
	@$(REBAR) xref skip_deps=true
