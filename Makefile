all:
	rebar compile && rebar escriptize

clean:
	rebar clean

get-deps:
	rebar get-deps