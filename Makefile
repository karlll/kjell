REBAR = ./rebar
CFG_DIR = ~${USER}/.kjell

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

install-extensions:
	mkdir -p ${CFG_DIR}
	cp -R ext/extensions ${CFG_DIR} 
