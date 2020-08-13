REBAR=./rebar3
CONFIG_DIR=./config

clean:
	${REBAR} clean

deps:
	${REBAR} deps

compile:
	${REBAR} compile

dev:
	${REBAR} shell --config ${CONFIG_DIR}/dev.config
