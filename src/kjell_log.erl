%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%   
%%% @end
%%%-------------------------------------------------------------------

-module(kjell_log).

-compile([export_all]).
-compile({no_auto_import,[error/2]}).
-include("kjell.hrl"). % defines current log level

-define(INFO_LBL,"").
-define(WARN_LBL,"[WARNING] ").
-define(ERROR_LBL,"[ERROR] ").
-define(DEBUG_LBL,"[DEBUG] ").


out(Msg, LogLevel) when LogLevel =< ?LL ->
	io:format("~s\n", [Msg]);
out(Msg, _LogLevel) ->
	ok.
out(Fmt,Arg,LogLevel) ->
	out(io_lib:format(Fmt,Arg),LogLevel).
out(Label,Fmt,Arg,LogLevel) ->
	out(lists:flatten(Label,Fmt), Arg, LogLevel).

log(info,Fmt,Arg) ->
	out(?INFO_LBL,Fmt,Arg,?LL_INFO);
log(warn,Fmt,Arg) ->
	out(?WARN_LBL,Fmt,Arg,?LL_WARN);
log(error,Fmt,Arg) ->
	out(?ERROR_LBL,Fmt,Arg,?LL_ERROR);
log(debug,Fmt,Arg) ->
	out(?DEBUG_LBL,Fmt,Arg,?LL_DEBUG).

info(Msg) ->
	info(Msg,[]).
warn(Msg) ->
	warn(Msg,[]).
error(Msg) ->
	error(Msg,[]).
debug(Msg) ->
	debug(Msg,[]).

info(Fmt,Arg) ->
	log(info,Fmt,Arg).
warn(Fmt,Arg) ->
	log(warn,Fmt,Arg).
error(Fmt,Arg) ->
	log(error,Fmt,Arg).
debug(Fmt,Arg) ->
	log(debug,Fmt,Arg).



