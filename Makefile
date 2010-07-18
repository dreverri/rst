.PHONY: deps compile test

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

check:
	@./rebar check-deps

clean:
	@./rebar clean

test: compile
	@./rebar skip_deps=true eunit
