-module(test_extension2).

% must be exported by all extensions
-export([extends/0]).
-export([input/1,cmd2/1]).

extends() ->
    {
      % Name, Version, Description
      {"Test Extension 2","0.1","A small test extension"},
      [
       % Extension point, {Module,Function}, Description
       {shell_input_line,{?MODULE,input},"Test input 2"},
       {command,{?MODULE,cmd2},"Test command"}
      ]
    }.




input(Args) ->
    io:format("(input) Extension got : ~p~n",[Args]),
    {ok, Args}.

cmd2(Args) ->
    io:format("(cmd) Extension got : ~p~n",[Args]),
    {ok, Args}.
