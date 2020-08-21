REBAR=./rebar3
CONFIG_DIR=./config

clean:
	@${REBAR} clean

deps:
	@${REBAR} deps

compile:
	@${REBAR} compile

test:
	@${REBAR} eunit

dev:
	@${REBAR} shell
