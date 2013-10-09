%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%   
%%% @end
%%%-------------------------------------------------------------------

-module(kjell_log).

-compile([export_all]).
-include("kjell.hrl").

-define(INFO_LBL,"").
-define(WARN_LBL,"[WARNING] ").
-define(ERROR_LBL,"[ERROR] ").
-define(DEBUG_LBL,"[DEBUG] ").

out(Msg) ->
	io:format("~s\n", [Msg]).
out(Fmt,Arg) ->
	io:format(lists:flatten(Fmt,"\n"),Arg).
out(Label,Fmt,Arg) ->
	out(lists:flatten(Label,Fmt), Arg).

log(info,Fmt,Arg) ->
	out(?INFO_LBL,Fmt,Arg);
log(warn,Fmt,Arg) ->
	out(?WARN_LBL,Fmt,Arg);
log(error,Fmt,Arg) ->
	out(?ERROR_LBL,Fmt,Arg);
log(debug,Fmt,Arg) ->
	out(?DEBUG_LBL,Fmt,Arg).

info(Msg) ->
	log(info,Msg,[]).
warn(Msg) ->
	log(warn,Msg,[]).
error(Msg) ->
	log(error,Msg,[]).
debug(Msg) ->
	log(debug,Msg,[]).

info(Fmt,Arg) ->
	log(info,Fmt,Arg).
warn(Fmt,Arg) ->
	log(warn,Fmt,Arg).
error(Fmt,Arg) ->
	log(error,Fmt,Arg).
debug(Fmt,Arg) ->
	log(debug,Fmt,Arg).



