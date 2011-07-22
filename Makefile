.PHONY: all test clean compile deps

REBAR=./rebar

all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test_db:
	@$(REBAR) compile skip_deps=true
	@$(REBAR) eunit skip_deps=true suite=dca_db_tests
clean:
	@$(REBAR) clean
start:  all
	erl -pa ./ebin -pa ./deps/*/ebin -sname site_stater -boot start_sasl -eval "application:start(site_stater)."