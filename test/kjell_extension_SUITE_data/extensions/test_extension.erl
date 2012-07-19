-module(test_extension).

% must be exported by all extensions
-export([extends/0]).
-export([input/1,output/1]).

extends() ->
    {
      % Name, Version, Description
      {"Test Extension","0.1","A small test extension"},
      [
       % Extension point, {Module,Function}, Description
       {shell_input_line,{?MODULE,input},"Test input"},
       {shell_output_line,{?MODULE,output},"Test output"}
      ]
    }.




input(Args) ->
    io:format("(input) Extension got : ~p~n",[Args]),
    {ok, Args}.

output(Args) ->
    Res = lists:flatten(io_lib:format("(output) Extension got : ~s\n",[Args])),
    io:format("~s",[Res]),
    {ok, Res}.
