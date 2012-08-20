-module(test_cmd).

% must be exported by all extensions
-export([extends/0]).
-export([cmd/1]).
-export([cmd_error/1]).

extends() ->
    {
      % Name, Version, Description
      {"Test Command","0.1","A small test command"},
      [
       % Extension point, {Module,Function}, Description
       {command,{?MODULE,cmd},"Test command"},
       {command,{?MODULE,cmd_error},"Test command 2"}

      ]
    }.


cmd(Args) ->
    Ret =lists:flatten( io_lib:format("(cmd) Extension got : ~p~n",[Args])),
    ct:pal("Ret = ~p~n",[Ret]),
    {ok, Ret}.

cmd_error(Args) ->
    {error, "Error message"}.
