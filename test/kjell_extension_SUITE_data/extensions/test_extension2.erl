-module(test_extension2).

% must be exported by all extensions
-export([extends/0]).
-export([input/1,cmd/1]).

extends() ->
    {
      % Name, Version, Description
      {"Test Extension 2","0.1","A small test extension"},
      [
       % Extension point, {Module,Function}, Description
       {shell_input_line,{?MODULE,input},"Test input 2"},
       {command,{?MODULE,cmd},"Test command 2 (test_cmd contains the same command)"}
      ]
    }.




input(Args) ->
    io:format("(input) Extension got : ~p~n",[Args]),
    {ok, Args}.

cmd(Args) ->
    Ret =lists:flatten( io_lib:format("(cmd) Extension got : ~p~n",[Args])),
    ct:pal("Ret = ~p~n",[Ret]),
    {ok, Ret}.
