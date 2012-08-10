REBAR = rebar
CUSTOM_OTP_REPO = https://github.com/karlll/otp.git
CUSTOM_OTP_DIR = deps/otp-custom_ttsl_drv
CUSTOM_OTP_BRANCH = esc_ttsl_drv
OTP_BUILD_CMD = ./otp_build setup

all: compile

test: compile ct

deps:
	mkdir -p $(CUSTOM_OTP_DIR)
	git clone -b $(CUSTOM_OTP_BRANCH) $(CUSTOM_OTP_DIR)
	cd $(CUSTOM_OTP_DIR); \
	$(OTP_BUILD_CMD)

compile: 
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump
	rm -f test/*.beam
	rm -rf logs
ct:
	@$(REBAR) ct suites=kjell_profile,kjell_extension
