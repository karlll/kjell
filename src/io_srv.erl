%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%
%%% @end

-module(io_srv).


-export([start_link/1, init/0, loop/1]).

-record(state, 
	{
	}
       ).

start_link(Name) ->

    Pid = spawn_link(?MODULE,init,[]),
    true = register(Name,Pid),
    %% test
    unlink(Pid),
    %% 
    Pid.

init() ->
    ?MODULE:loop(#state{}).

loop(State) ->
    receive

	{io_request, From, ReplyAs, Request} ->

	    case req(Request,From,ReplyAs,State) of
		
		{ok,State} ->
		    ?MODULE:loop(State);
		{error,Reason,State} ->
		    exit({Reason,State})

	    end;

	_Unknown ->
	    exit(unknown)
    end.


req({put_chars, Encoding, Characters},
    From, ReplyAs, State ) -> 

    Reply = output(Encoding, Characters),
    From ! {io_reply, ReplyAs, Reply},
    {ok, State};

req({put_chars, Encoding, Module, Function, Args},
    From, ReplyAs, State) ->

    Reply = output(Encoding, Module, Function, Args),
    reply(From, ReplyAs, Reply),
    {ok, State};

req({put_chars, Characters},
    From, ReplyAs, State) ->

    Reply = output(Characters),
    reply(From, ReplyAs, Reply),
    {ok, State};

req({put_chars, Module, Function, Args},
    From, ReplyAs, State) ->

    Reply = output(Module, Function, Args),
    reply(From, ReplyAs, Reply),
    {ok, State};


req({get_until, Encoding, Prompt, Module, Function, ExtraArgs},
    From, ReplyAs, State) ->    
    {ok, State};

req({get_chars, Encoding, Prompt, N},
    From, ReplyAs, State) ->
    {ok, State};


req({get_line, Encoding, Prompt},
    From, ReplyAs, State) ->
    {ok, State};

req({get_until, Prompt, Module, Function, ExtraArgs},
    From, ReplyAs, State) ->
    {ok, State};

req({get_chars, Prompt, N},
    From, ReplyAs, State) ->
    {ok, State};

req({get_line, Prompt},
    From, ReplyAs, State) ->
    {ok, State}.




reply(To,From,Reply) ->
    To ! {io_reply, From, Reply}.


output(Encoding,Module,Function,Args) ->
    try apply(Module,Function,Args) of
	Res ->
	    Dec = unicode:characters_to_list(Res,Encoding),
	    out(Dec),
	    ok
    catch
	Class:Reason ->
	    {Class,Reason}
			     
    end.

output(Module,Function,Args) ->
    try apply(Module,Function,Args) of
	Res ->
	    out(Res),
	    ok
    catch
	Class:Reason ->
	    {Class,Reason}
			     
    end.

output(Encoding,Characters) ->
    Dec = unicode:characters_to_list(Characters, Encoding),
    out(Dec),
    ok.
output(Characters) ->
    out(Characters),
    ok.


%% send to out device
out(Characters) ->
    io:format(Characters),
    ok.


%
% From the IO Protocol Doc
%

% input request
% {get_until, Encoding, Prompt, Module, Function, ExtraArgs}
%     Module, Function, ExtraArgs denotes a function and arguments to determine when enough data is written. 
%     The function should take two additional arguments, the last state, and a list of characters. The function should return one of:
%{done, Result, RestChars}
%{more, Continuation}
% The function should be called like apply(Module, Function, [ State, Data | ExtraArgs ]) 
% {get_chars, Encoding, Prompt, N}
% {get_line, Encoding, Prompt}
%% The server replies to the client with an io_reply where the Reply element is one of:
%% Data
%% eof
%% {error, Error}
%%  -  Data is the characters read, in either list or binary form (depending on the io_server mode, see below).
%%  -  Error describes the error to the client, which may do whatever it wants with it. The Erlang io-module typically returns it as is.
%%  -  eof is returned when input end is reached and no more data is available to the client process.
%% For backward compatibility the following Requests should also be handled by an io_server (these messages should not be present after R15B of OTP):
%% {get_until, Prompt, Module, Function, ExtraArgs}
%% {get_chars, Prompt, N}
%% {get_line, Prompt}
