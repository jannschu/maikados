all: compile

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

test:
	@./rebar eunit skip_deps=true
