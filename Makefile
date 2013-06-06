compile: deps
	rebar compile

.PHONY: deps
deps:
	rebar get-deps

erl: compile
	ERL_LIBS=deps erl -pa ebin -pa deps -s bucket_app -sname bucket
