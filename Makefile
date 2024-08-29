all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

eunit:
	rebar3 do eunit -c, cover

proper:
	rebar3 proper -n 100

ct:
	rebar3 do ct -c, cover

xref:
	rebar3 xref

dialyze:
	rebar3 dialyzer

upgrade:
	rebar3 upgrade

unlock:
	rebar3 unlock

lock:
	rebar3 lock

elvis_rock:
	rebar3 lint
