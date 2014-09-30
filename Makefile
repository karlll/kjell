REBAR=./rebar
CFG_DIR=~${USER}/.kjell
DEFAULT_PREFIX=/usr/local/opt
BIN_DIR=/usr/local/bin

INSTALL_DIR=$(shell cat priv/prefix 2>/dev/null || echo $(DEFAULT_PREFIX)/kjell)

all: compile

test: compile ct

compile: 
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -f erl_crash.dump
	@rm -f test/*.beam
	@rm -rf logs
	@rm -rf priv/*
ct:
	@$(REBAR) ct suites=kjell_profile,kjell_extension

configure:
	@rm -rf priv/prefix
ifdef PREFIX
	@echo "Setting installation dir to ${PREFIX}"
	@mkdir -p priv
	@echo ${PREFIX} > priv/prefix
endif 

install:
	@echo "Installing in ${INSTALL_DIR}"
	@mkdir -p ${INSTALL_DIR}
	@cp -R * ${INSTALL_DIR}
	@sed -i'.bak' "s|^EBIN_DIR=\(.*\)|EBIN_DIR=${INSTALL_DIR}/ebin|" ${INSTALL_DIR}/bin/kjell && rm ${INSTALL_DIR}/bin/kjell.bak
ifndef NO_SYMLINK
	@echo "Creatink symbolic link ${BIN_DIR}/kjell -> ${INSTALL_DIR}/bin/kjell"
	@ln -sf ${INSTALL_DIR}/bin/kjell ${BIN_DIR}/kjell
endif

install-extensions:
	@mkdir -p ${CFG_DIR}
	@cp -R ext/extensions ${CFG_DIR} 
