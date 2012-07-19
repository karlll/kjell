%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(start_kjell).

-export([start/0]).


start() ->
    user_drv:start(['tty_sl -e -c',{kjell,start,[init]}]),
    ok.
