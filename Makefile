REBAR = ./rebar

all: compile

test: compile ct

compile: 
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump
	rm -f test/*.beam
	rm -rf logs
ct:
	@$(REBAR) ct suites=kjell_profile,kjell_extension
